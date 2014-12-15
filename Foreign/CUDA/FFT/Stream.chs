{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.FFT.Stream (

  -- * Streamed transforms
  setStream,

) where

-- friends
import Foreign.CUDA.Types
import Foreign.CUDA.FFT.Plan
import Foreign.CUDA.FFT.Error
import Foreign.CUDA.FFT.Internal.C2HS

-- system
import Foreign
import Foreign.C

#include <cbits/wrap.h>
{# context lib="cufft" #}

-- | Associates a CUDA stream with a CUFFT plan. All kernel launches made during
-- plan execution are now done through the associated stream, enabling overlap
-- with activity in other streams (e.g. data copying). The association remains
-- until the plan is destroyed or the stream is changed.
--
setStream :: Handle -> Stream -> IO ()
setStream ctx st = nothingIfOk =<< cufftSetStream ctx st

{# fun unsafe cufftSetStream
  { useHandle `Handle'
  , useStream `Stream' } -> `Result' cToEnum #}

