{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.FFT.Error
  where

-- System
import Data.Typeable
import Control.Exception

#include <cbits/wrap.h>
{# context lib="cufft" #}


-- Error codes -----------------------------------------------------------------
--
{# enum cufftResult as Result
  { underscoreToCase }
  with prefix="CUFFT" deriving (Eq, Show) #}

-- Describe each error code
--
describe :: Result -> String
describe Success                 = "success"
describe InvalidPlan             = "invalid plan handle"
describe AllocFailed             = "resource allocation failed"
describe InvalidType             = "no longer used"
describe InvalidValue            = "unsupported value or parameter passed to a function"
describe InternalError           = "internal error"
describe ExecFailed              = "failed to execute an FFT on the GPU"
describe SetupFailed             = "the CUFFT library failed to initialize"
describe InvalidSize             = "invalid transform size"
describe UnalignedData           = "no longer used"
#if CUDA_VERSION >= 6000
describe IncompleteParameterList = "missing parameters in call"
describe InvalidDevice           = "execution of a plan was on a different GPU than plan creation"
describe ParseError              = "internal plan database error"
describe NoWorkspace             = "no workspace has been provided prior to plan execution"
#endif
#if CUDA_VERSION >= 6050
describe NotImplemented          = "not implemented"
describe LicenseError            = "cufft license error"
#endif
#if CUDA_VERSION >= 8000
describe NotSupported            = "operation not supported for given parameters"
#endif


-- Exceptions ------------------------------------------------------------------
--
data CUFFTException
  = ExitCode  Result
  | UserError String
  deriving Typeable

instance Exception CUFFTException

instance Show CUFFTException where
  showsPrec _ (ExitCode  s) = showString ("CUFFT Exception: " ++ describe s)
  showsPrec _ (UserError s) = showString ("CUFFT Exception: " ++ s)


-- | Raise a CUFFTException in the IO Monad
--
cufftError :: String -> IO a
cufftError s = throwIO (UserError s)


-- | Return the results of a function on successful execution, otherwise throw
-- an exception with an error string associated with the return code
--
resultIfOk :: (Result, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)


-- | Throw an exception with an error string associated with an unsuccessful
-- return code, otherwise return unit.
--
nothingIfOk :: Result -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (ExitCode status)

