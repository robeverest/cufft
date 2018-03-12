{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- The MIN_VERSION_Cabal macro was introduced with Cabal-1.24 (??)
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(major1,major2,minor) 0
#endif

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.Command
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Find
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity

#if MIN_VERSION_Cabal(1,25,0)
import Distribution.PackageDescription.PrettyPrint
import Distribution.Version
#endif
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif

import Control.Applicative
import Control.Exception
import Control.Monad
import Language.Haskell.TH
import Prelude
import System.Directory
import System.FilePath
import Text.Printf

import Foreign.CUDA.Path


-- Configuration
-- -------------

customBuildInfoFilePath :: FilePath
customBuildInfoFilePath = "cufft" <.> "buildinfo"

generatedBuildInfoFilePath :: FilePath
generatedBuildInfoFilePath = customBuildInfoFilePath <.> "generated"


-- http://docs.nvidia.com/cuda/cufft/index.html#static-library
--
staticLibs :: Platform -> [String]
staticLibs platform@(Platform _arch os) =
  case os of
    _ -> dynamicLibs platform

    -- TLM: I can't get this work at the moment. This package will build fine,
    -- but client packages (e.g. accelerate-fft) will fail with an error such as:
    --
    -- > dyld: lazy symbol binding failed: Symbol not found: ___cudaRegisterLinkedBinary_72_tmpxft_000005ef_00000000_15_fft_dimension_class_multi_compute_60_cpp1_ii_466e44ab
    --
    -- Windows -> dynamicLibs platform
    -- _       -> ["cufft_static", "cudart_static", "culibos", "pthread", "dl"]

dynamicLibs :: Platform -> [String]
dynamicLibs _ = ["cufft"]


-- Build setup
-- -----------

main :: IO ()
main = defaultMainWithHooks customHooks
  where
    readHook get_verbosity args flags = do
        noExtraFlags args
        getHookedBuildInfo (fromFlag (get_verbosity flags))

    preprocessors = hookedPreProcessors simpleUserHooks

    -- Our readHook implementation uses our getHookedBuildInfo. We can't rely on
    -- cabal's autoconfUserHooks since they don't handle user overwrites to
    -- buildinfo like we do.
    --
    customHooks =
      simpleUserHooks
        { preBuild            = preBuildHook -- not using 'readHook' here because 'build' takes extra args
        , preClean            = readHook cleanVerbosity
        , preCopy             = readHook copyVerbosity
        , preInst             = readHook installVerbosity
        , preHscolour         = readHook hscolourVerbosity
        , preHaddock          = readHook haddockVerbosity
        , preReg              = readHook regVerbosity
        , preUnreg            = readHook regVerbosity
        , postConf            = postConfHook
        -- , postBuild           = postBuildHook
        , hookedPreProcessors = ("chs", pp_c2hs) : filter (\x -> fst x /= "chs") preprocessors
        }

    -- The hook just loads the HookedBuildInfo generated by postConfHook, unless
    -- there is user-provided info that overwrites it.
    --
    preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
    preBuildHook _ flags = getHookedBuildInfo $ fromFlag $ buildVerbosity flags

    -- The hook scans system in search for CUDA Toolkit. If the toolkit is not
    -- found, an error is raised. Otherwise the toolkit location is used to
    -- create a `cufft.buildinfo.generated` file with all the resulting flags.
    postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConfHook args flags pkg_descr lbi = do
      let
          verbosity       = fromFlagOrDefault normal (configVerbosity flags)
          profile         = fromFlagOrDefault False  (configProfLib flags)
          currentPlatform = hostPlatform lbi
          compilerId_     = compilerId (compiler lbi)
      --
      noExtraFlags args
      generateAndStoreBuildInfo verbosity profile currentPlatform compilerId_ generatedBuildInfoFilePath
      validateLinker verbosity currentPlatform $ withPrograms lbi
      --
      actualBuildInfoToUse <- getHookedBuildInfo verbosity
      let pkg_descr' = updatePackageDescription actualBuildInfoToUse pkg_descr
      postConf simpleUserHooks args flags pkg_descr' lbi

    -- This hook fixes the embedded LC_RPATHs in the generated .dylib on OSX.
    postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postBuildHook _ flags pkg_descr lbi = do
      let
          verbosity           = fromFlag (buildVerbosity flags)
          platform            = hostPlatform lbi
          cid                 = compilerId (compiler lbi)
          uid                 =
#if MIN_VERSION_Cabal(1,25,0)
            localUnitId lbi
#else
            $( case withinRange cabalVersion (orLaterVersion (Version [1,24] [])) of
                 True  -> return ( AppE (VarE (mkName "localUnitId")) (VarE (mkName "lbi")) )
                 False -> return ( AppE (VarE (mkName "head"))
                                        (AppE (VarE (mkName ("componentLibraries")))
                                        (AppE (AppE (VarE (mkName "getComponentLocalBuildInfo")) (VarE (mkName "lbi"))) (ConE (mkName "CLibName")))) )

                 -- True  -> [| localUnitId lbi |]
                 -- False -> [| head (componentLibraries (getComponentLocalBuildInfo lbi CLibName)) |]
             )
#endif
          sharedLib           = buildDir lbi </> mkSharedLibName cid uid
          Just extraLibDirs'  = extraLibDirs . libBuildInfo <$> library pkg_descr
      --
      updateLibraryRPATHs verbosity platform sharedLib extraLibDirs'


-- It seems that the GHC and/or Cabal developers don't quite understand how
-- dynamic linking works on OSX. Even though we have specified
-- '-optl-Wl,-rpath,...' as part of the configuration, this (sometimes?) gets
-- filtered out somewhere, and the resulting .dylib that is generated does not
-- have this path embedded as an LC_RPATH. The result is that the cuFFT library
-- will not be found, resulting in a link-time error.
--
-- On *nix (and versions of OSX previous to El Capitan 10.11), we could use
-- [DY]LD_LIBRARY_PATH to specify where to resolve @rpath locations, but that is
-- no longer an option on OSX due to System Integrity Protection.
--
-- An alternate argument is that the CUDA installer should have updated the
-- install name (LC_ID_DYLIB) of its dynamic libraries to include the full
-- absolute path, rather than relying on @rpath in the first place, which is
-- what Apple's system libraries do for example.
--
updateLibraryRPATHs :: Verbosity -> Platform -> FilePath -> [FilePath] -> IO ()
updateLibraryRPATHs verbosity (Platform _ os) sharedLib extraLibDirs' =
  when (os == OSX) $ do
    exists <- doesFileExist sharedLib
    unless exists $ die' verbosity $ printf "Unexpected failure: library does not exist: %s" sharedLib
    --
    mint   <- findProgram verbosity "install_name_tool"
    case mint of
      Nothing                -> notice verbosity $ "Could not locate 'install_name_tool' in order to update LC_RPATH entries. This is likely to cause problems later on."
      Just install_name_tool ->
        forM_ extraLibDirs' $ \libDir ->
          runProgramInvocation verbosity $ simpleProgramInvocation install_name_tool ["-add_rpath", libDir, sharedLib]


-- Runs CUDA detection procedure and stores .buildinfo to a file.
--
generateAndStoreBuildInfo :: Verbosity -> Bool -> Platform -> CompilerId -> FilePath -> IO ()
generateAndStoreBuildInfo verbosity profile platform (CompilerId _ghcFlavor ghcVersion) path =
  storeHookedBuildInfo verbosity path =<< libraryBuildInfo profile platform ghcVersion

storeHookedBuildInfo :: Verbosity -> FilePath -> HookedBuildInfo -> IO ()
storeHookedBuildInfo verbosity path hbi = do
  notice verbosity $ "Storing parameters to " ++ path
  writeHookedBuildInfo path hbi

-- Reads user-provided `cufft.buildinfo` if present, otherwise loads
-- `cufft.buildinfo.generated` Outputs message informing about the other
-- possibility. Calls die when neither of the files is available. (generated one
-- should be always present, as it is created in the post-conf step)
--
getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
getHookedBuildInfo verbosity = do
  customBuildInfoExists <- doesFileExist customBuildInfoFilePath
  if customBuildInfoExists
    then do
      notice verbosity $ printf "The user-provided buildinfo from file '%s' will be used. To use default settings, delete this file." customBuildInfoFilePath
      readHookedBuildInfo verbosity customBuildInfoFilePath
    else do
      generatedBuildInfoExists <- doesFileExist generatedBuildInfoFilePath
      if generatedBuildInfoExists
        then do
          notice verbosity $ printf "Using build information from '%s'" generatedBuildInfoFilePath
          notice verbosity $ printf "Provide a '%s' file to override this behaviour" customBuildInfoFilePath
          readHookedBuildInfo verbosity generatedBuildInfoFilePath
        else
          die' verbosity $ printf "Unexpected failure: neither the default '%s' nor custom '%s' exist" generatedBuildInfoFilePath customBuildInfoFilePath


findProgram :: Verbosity -> FilePath -> IO (Maybe FilePath)
findProgram verbosity prog =
  findProgram_helper (findProgramOnSearchPath verbosity defaultProgramSearchPath prog)

class FindProgram f where
  findProgram_helper :: f -> IO (Maybe FilePath)

instance FindProgram (IO (Maybe (FilePath, [FilePath]))) where
  findProgram_helper f = fmap fst `fmap` f

instance FindProgram (IO (Maybe FilePath)) where
  findProgram_helper f = f


-- Generates build info with flags needed for CUDA Toolkit to be properly
-- visible to underlying build tools.
--
libraryBuildInfo :: Bool -> Platform -> Version -> IO HookedBuildInfo
libraryBuildInfo profile platform@(Platform arch os) ghcVersion = do
  let
      -- options for GHC
      extraLibDirs'     = [ cudaLibraryPath ]
      ccOptions'        = [ "-I" ++ cudaIncludePath ]
      ldOptions'        = [ "-L" ++ cudaLibraryPath ]
      ghcOptions        = map ("-optc"++) ccOptions'
                       ++ map ("-optl"++) ldOptions'
                       ++ if os /= Windows && not profile
                            then map ("-optl-Wl,-rpath,"++) extraLibDirs'
                            else []

      extraLibs'        = staticLibs platform
      extraGHCiLibs'    = dynamicLibs platform

      -- options for C2HS
      archFlag          = case arch of
                            I386   -> "-m32"
                            X86_64 -> "-m64"
                            _      -> ""
      emptyCase         = ["-DUSE_EMPTY_CASE" | versionBranch ghcVersion >= [7,8]]
      blocksExtension   = ["-U__BLOCKS__" | os == OSX ]
      c2hsOptions       = unwords $ map ("--cppopts="++) ("-E" : archFlag : emptyCase ++ blocksExtension)
      c2hsExtraOptions  = ("x-extra-c2hs-options", c2hsOptions)

      addSystemSpecificOptions :: BuildInfo -> IO BuildInfo
      addSystemSpecificOptions bi =
        case os of
          -- In the CUDA package this is used to populate the extraGHCiLibs
          -- field with the mangled .dll names. I'm not sure what those are for
          -- this library, so left out for the time being.
          _       -> return bi

  buildInfo' <- addSystemSpecificOptions $ emptyBuildInfo
    { ccOptions      = ccOptions'
    , ldOptions      = ldOptions'
    , extraLibs      = extraLibs'
    , extraGHCiLibs  = extraGHCiLibs'
    , extraLibDirs   = extraLibDirs'
    , options        = [(GHC, ghcOptions) | os /= Windows]
    , customFieldsBI = [c2hsExtraOptions]
    }

  return (Just buildInfo', [])


-- On Windows platform the binutils linker targeting x64 is bugged and cannot
-- properly link with import libraries generated by MS compiler (like the CUDA ones).
-- The programs would correctly compile and crash as soon as the first FFI call is made.
--
-- Therefore we fail configure process if the linker is too old and provide user
-- with guidelines on how to fix the problem.
--
validateLinker :: Verbosity -> Platform -> ProgramDb -> IO ()
validateLinker verbosity (Platform X86_64 Windows) db = do
  let say msg = printf "%s. If generated executables crash when making calls to CUFFT please see: %s" msg windowsHelpPage
  --
  maybeLdPath <- getRealLdPath verbosity db
  case maybeLdPath of
    Nothing     -> warn verbosity $ say "Cannot find ld.exe to check if it is new enough"
    Just ldPath -> do
      debug verbosity $ "Checking if ld.exe at " ++ ldPath ++ " is new enough"
      maybeVersion <- getLdVersion verbosity ldPath
      case maybeVersion of
        Nothing        -> warn verbosity $ say "Unknown ld.exe version"
        Just ldVersion -> do
          debug verbosity $ "Found ld.exe version: " ++ show ldVersion
          when (ldVersion < [2,25,1]) $ die' verbosity (windowsLinkerBugMsg ldPath)
validateLinker _ _ _ = return () -- The linker bug is present only on Win64 platform


-- On Windows GHC package comes with two copies of ld.exe.
--
-- ProgramDb knows about the first one: ghcpath\mingw\bin\ld.exe
-- This function returns the other one: ghcpath\mingw\x86_64-w64-mingw32\bin\ld.exe
--
-- The second one is the one that does actual linking and code generation.
-- See: https://github.com/tmcdonell/cuda/issues/31#issuecomment-149181376
--
-- The function is meant to be used only on 64-bit GHC distributions.
--
getRealLdPath :: Verbosity -> ProgramDb -> IO (Maybe FilePath)
getRealLdPath verbosity programDb =
  -- This should ideally work `programFindVersion ldProgram` but for some reason
  -- it does not. The issue should be investigated at some time.
  case lookupProgram ghcProgram programDb of
    Nothing            -> return Nothing
    Just configuredGhc -> do
      let ghcPath        = locationPath $ programLocation configuredGhc
          presumedLdPath = (takeDirectory . takeDirectory) ghcPath </> "mingw" </> "x86_64-w64-mingw32" </> "bin" </> "ld.exe"
      info verbosity $ "Presuming ld location" ++ presumedLdPath
      presumedLdExists <- doesFileExist presumedLdPath
      return $ if presumedLdExists
                 then Just presumedLdPath
                 else Nothing

-- Tries to obtain the version `ld`. Throws an exception on failure.
--
getLdVersion :: Verbosity -> FilePath -> IO (Maybe [Int])
getLdVersion verbosity ldPath = do
  -- Examples of version string format:
  --  * GNU ld (GNU Binutils) 2.25.1
  --  * GNU ld (GNU Binutils) 2.20.51.20100613
  --
  ldVersionString <- getProgramInvocationOutput normal (simpleProgramInvocation ldPath ["-v"])

  let versionText   = last $ words ldVersionString -- takes e.g. "2.25.1"
      versionParts  = splitOn (== '.') versionText
      versionParsed = Just $ map read versionParts

  -- last and read above may throw and message would be not understandable for user,
  -- so we'll intercept exception and rethrow it with more useful message.
  let handleError :: SomeException -> IO (Maybe [Int])
      handleError e = do
          warn verbosity $ printf "cannot parse ld version string: '%s'. Parsing exception: %s" ldVersionString (show e)
          return Nothing

  evaluate versionParsed `catch` handleError

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =
  case dropWhile p s of
    [] -> []
    ss -> let (w,s') = break p ss in w : splitOn p s'


windowsHelpPage :: String
windowsHelpPage = "https://github.com/robeverest/cufft/blob/master/WINDOWS.md"

windowsLinkerBugMsg :: FilePath -> String
windowsLinkerBugMsg ldPath = printf (unlines msg) windowsHelpPage ldPath
  where
    msg =
      [ "********************************************************************************"
      , ""
      , "The installed version of `ld.exe` has version < 2.25.1. This version has known bug on Windows x64 architecture, making it unable to correctly link programs using CUDA. The fix is available and MSys2 released fixed version of `ld.exe` as part of their binutils package (version 2.25.1)."
      , ""
      , "To fix this issue, replace the `ld.exe` in your GHC installation with the correct binary. See the following page for details:"
      , ""
      , "  %s"
      , ""
      , "The full path to the outdated `ld.exe` detected in your installation:"
      , ""
      , "> %s"
      , ""
      , "Please download a recent version of binutils `ld.exe`, from, e.g.:"
      , ""
      , "  http://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-binutils-2.25.1-1-any.pkg.tar.xz"
      , ""
      , "********************************************************************************"
      ]


-- Replicate the default C2HS preprocessor hook here, and inject a value for
-- extra-c2hs-options, if it was present in the buildinfo file
--
-- This is largely copied from Distribution.Simple.PreProcess, with some hacks
-- to make it work with different versions of Cabal-the-library.
--
class PPC2HS f where
  pp_c2hs :: f

instance PPC2HS (BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor) where
  pp_c2hs bi lbi _ = pp_c2hs bi lbi

instance PPC2HS (BuildInfo -> LocalBuildInfo -> PreProcessor) where
  pp_c2hs bi lbi =
    PreProcessor
      { platformIndependent = False
      , runPreProcessor     = \(inBaseDir, inRelativeFile)
                               (outBaseDir, outRelativeFile) verbosity ->
          runDbProgram verbosity c2hsProgram (withPrograms lbi) . filter (not . null) $
            maybe [] words (lookup "x-extra-c2hs-options" (customFieldsBI bi))
            ++ ["--include=" ++ outBaseDir]
            ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
            ++ ["--output-dir=" ++ outBaseDir,
                "--output=" ++ outRelativeFile,
                inBaseDir </> inRelativeFile]
      }


getCppOptions :: BuildInfo -> LocalBuildInfo -> [String]
getCppOptions bi lbi
    = hcDefines (compiler lbi)
   ++ ["-I" ++ dir | dir <- includeDirs bi]
   ++ [opt | opt@('-':c:_) <- ccOptions bi, c `elem` "DIU"]

hcDefines :: Compiler -> [String]
hcDefines comp =
  case compilerFlavor comp of
    GHC  -> ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
    JHC  -> ["-D__JHC__=" ++ versionInt version]
    NHC  -> ["-D__NHC__=" ++ versionInt version]
    Hugs -> ["-D__HUGS__"]
    _    -> []
  where version = compilerVersion comp

-- TODO: move this into the compiler abstraction
-- FIXME: this forces GHC's crazy 4.8.2 -> 408 convention on all the other
-- compilers. Check if that's really what they want.
versionInt :: Version -> String
versionInt v =
  case versionBranch v of
    []      -> "1"
    [n]     -> show n
    n1:n2:_ -> printf "%d%02d" n1 n2

#if MIN_VERSION_Cabal(1,25,0)
versionBranch :: Version -> [Int]
versionBranch = versionNumbers
#endif

#if !MIN_VERSION_Cabal(2,0,0)
die' :: Verbosity -> String -> IO a
die' _ = die
#endif

