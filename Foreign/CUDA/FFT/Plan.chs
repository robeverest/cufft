{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.FFT.Plan (

  -- * Context
  Handle(..),
  Type(..),
  plan1D,
  plan2D,
  plan3D,
  planMany,
  destroy,

) where

-- Friends
import Foreign.CUDA.FFT.Error
import Foreign.CUDA.FFT.Internal.C2HS

-- System
import Foreign
import Foreign.C
import Control.Monad                            ( liftM )
import Data.Maybe

#include <cbits/wrap.h>
{# context lib="cufft" #}


-- | Operations handle
--
newtype Handle = Handle { useHandle :: {# type cufftHandle #}}

{# enum cufftType as Type
  {}
  with prefix="CUFFT" deriving (Eq, Show) #}

-- Context management ----------------------------------------------------------
--

-- | Creates a 1D FFT plan configuration for a specified signal size and data type.
-- The third argument tells CUFFT how many 1D transforms to configure.
--
plan1D :: Int -> Type -> Int -> IO Handle
plan1D nx t batch = resultIfOk =<< cufftPlan1d nx t batch

{# fun unsafe cufftPlan1d
  { alloca-   `Handle' peekHdl*
  ,           `Int'
  , cFromEnum `Type'
  ,           `Int'             } -> `Result' cToEnum #}
  where
    peekHdl = liftM Handle . peek

-- | Creates a 2D FFT plan configuration for a specified signal size and data type.
--
plan2D :: Int -> Int -> Type -> IO Handle
plan2D nx ny t = resultIfOk =<< cufftPlan2d nx ny t

{# fun unsafe cufftPlan2d
  { alloca-   `Handle' peekHdl*
  ,           `Int'
  ,           `Int'
  , cFromEnum `Type'            } -> `Result' cToEnum #}
  where
    peekHdl = liftM Handle . peek

-- | Creates a 3D FFT plan configuration for a specified signal size and data type.
--
plan3D :: Int -> Int -> Int -> Type -> IO Handle
plan3D nx ny nz t = resultIfOk =<< cufftPlan3d nx ny nz t

{# fun unsafe cufftPlan3d
  { alloca-   `Handle' peekHdl*
  ,           `Int'
  ,           `Int'
  ,           `Int'
  , cFromEnum `Type'            } -> `Result' cToEnum #}
  where
    peekHdl = liftM Handle . peek

-- | Creates a batched plan configuration for many signals of a specified size in
-- either 1, 2 or 3 dimensions, and of the specified data type.
--
planMany :: [Int]                   -- ^ The size of each dimension
         -> Maybe ([Int], Int, Int) -- ^ Storage dimensions of the output data,
                                    -- the stride, and the distance between
                                    -- signals for the input data
         -> Maybe ([Int], Int, Int) -- ^ As above but for the output data
         -> Type                    -- ^ The type of the transformation.
         -> Int                     -- ^ The batch size (either 1, 2 or 3)
         -> IO Handle
planMany n ilayout olayout t batch
  = do
      let (inembed, istride, idist) = fromMaybe ([], 0, 0) ilayout
      let (onembed, ostride, odist) = fromMaybe ([], 0, 0) olayout
      resultIfOk =<< cufftPlanMany (length n) n inembed istride idist onembed ostride odist t batch

{# fun unsafe cufftPlanMany
  { alloca-   `Handle' peekHdl*
  ,           `Int'
  , asArray *  `[Int]'
  , asArray *  `[Int]'
  ,           `Int'
  ,           `Int'
  , asArray *  `[Int]'
  ,           `Int'
  ,           `Int'
  , cFromEnum `Type'
  ,           `Int'} -> `Result' cToEnum #}
  where
    peekHdl = liftM Handle . peek
    asArray [] f = f nullPtr
    asArray xs f = withArray (map fromIntegral xs) f

-- | This function releases hardware resources used by the CUFFT plan. The
-- release of GPU resources may be deferred until the application exits. This
-- function is usually the last call with a particular handle to the CUFFT
-- plan.
--
destroy :: Handle -> IO ()
destroy ctx = nothingIfOk =<< cufftDestroy ctx

{# fun unsafe cufftDestroy
  { useHandle `Handle' } -> `Result' cToEnum #}
