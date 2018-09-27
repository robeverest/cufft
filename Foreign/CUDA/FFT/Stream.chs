{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.FFT.Stream
-- Copyright   : [2013..2018] Robert Clifton-Everest, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.FFT.Stream (

  Stream,
  setStream,

) where

-- friends
import Foreign.CUDA.Driver.Stream                         ( Stream(..) )
import Foreign.CUDA.FFT.Plan
import Foreign.CUDA.FFT.Error
import Foreign.CUDA.FFT.Internal.C2HS

-- system
import Foreign
import Foreign.C

#include <cbits/wrap.h>
{# context lib="cufft" #}


-- | Set the execution stream which all subsequent cuFFT library functions will
-- execute with. This enables the activity in this execution stream (e.g. kernel
-- launches and data transfer) to overlap with activity in other execution
-- streams. The association remains until the plan is destroyed or the stream is
-- changed.
--
-- If not set, functions execute in the default stream, which never overlaps
-- with any other operation.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftsetstream>
--
{-# INLINEABLE setStream #-}
{# fun unsafe cufftSetStream as setStream
  { useHandle `Handle'
  , useStream `Stream'
  }
  -> `()' checkStatus*- #}

