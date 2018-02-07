{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.FFT.Execute
-- Copyright   : [2013..2018] Robert Clifton-Everest, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.FFT.Execute (

  Mode(..),

  execC2C, execZ2Z,
  execR2C, execD2Z,
  execC2R, execZ2D,

) where

-- friends
import Foreign.CUDA.FFT.Error
import Foreign.CUDA.FFT.Plan
import Foreign.CUDA.FFT.Internal.C2HS

import Foreign.CUDA.Ptr

-- system
import Foreign
import Foreign.C
import Data.Complex

#include <cbits/wrap.h>
{# context lib="cufft" #}

#c
typedef enum cufftMode_enum {
    CUFFT_MODE_FORWARD = CUFFT_FORWARD,
    CUFFT_MODE_INVERSE = CUFFT_INVERSE
} cufftMode;
#endc

-- | FFT transform direction
--
{# enum cufftMode as Mode
  { underscoreToCase }
  with prefix="CUFFT_MODE" deriving (Eq, Show, Bounded) #}


-- | Executes a single-precision complex-to-complex transform.
--
-- If the input and output device pointers are the same, an in-place transform
-- is executed.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftexecc2c-cufftexecz2z>
--
{-# INLINEABLE execC2C #-}
execC2C
    :: Handle                     -- ^ plan handle, of type 'C2C'
    -> Mode                       -- ^ transform direction
    -> DevicePtr (Complex Float)  -- ^ input data
    -> DevicePtr (Complex Float)  -- ^ output data
    -> IO ()
execC2C hdl dir i o = cufftExecC2C hdl i o dir
  where
    {# fun unsafe cufftExecC2C
      { useHandle     `Handle'
      , useDevicePtr' `DevicePtr (Complex Float)'
      , useDevicePtr' `DevicePtr (Complex Float)'
      , cFromEnum     `Mode'
      }
      -> `()' checkStatus*- #}


-- | Executes a double-precision complex-to-complex transform.
--
-- If the input and output device pointers are the same, an in-place transform
-- is executed.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftexecc2c-cufftexecz2z>
--
{-# INLINEABLE execZ2Z #-}
execZ2Z
    :: Handle                     -- ^ plan handle, of type 'Z2Z'
    -> Mode                       -- ^ transform direction
    -> DevicePtr (Complex Double) -- ^ input data
    -> DevicePtr (Complex Double) -- ^ output data
    -> IO ()
execZ2Z hdl dir i o = cufftExecZ2Z hdl i o dir
  where
    {# fun unsafe cufftExecZ2Z
      { useHandle     `Handle'
      , useDevicePtr' `DevicePtr (Complex Double)'
      , useDevicePtr' `DevicePtr (Complex Double)'
      , cFromEnum     `Mode'
      }
      -> `()' checkStatus*- #}


-- | Executes a single-precision real-to-complex, implicitly forward, transform.
--
-- If the input and output device pointers refer to the same address, an
-- in-place transform is executed.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftexecr2c-cufftexecd2z>
--
{-# INLINEABLE execR2C #-}
{# fun unsafe cufftExecR2C as execR2C
  { useHandle     `Handle'                      -- ^ plan handle, of type 'R2C'
  , useDevicePtr' `DevicePtr Float'             -- ^ input data
  , useDevicePtr' `DevicePtr (Complex Float)'   -- ^ output data
  }
  -> `()' checkStatus*- #}


-- | Executes a double-precision real-to-complex, implicitly forward, transform.
--
-- If the input and output device pointers refer to the same address, an
-- in-place transform is executed.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftexecr2c-cufftexecd2z>
--
{-# INLINEABLE execD2Z #-}
{# fun unsafe cufftExecD2Z as execD2Z
  { useHandle     `Handle'                      -- ^ plan handle, of type 'D2Z'
  , useDevicePtr' `DevicePtr Double'            -- ^ input data
  , useDevicePtr' `DevicePtr (Complex Double)'  -- ^ output data
  }
  -> `()' checkStatus*- #}


-- | Executes a single-precision complex-to-real, implicitly forward, transform.
--
-- If the input and output device pointers refer to the same address, an
-- in-place transform is executed.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftexecc2r-cufftexecz2d>
--
{-# INLINEABLE execC2R #-}
{# fun unsafe cufftExecC2R as execC2R
  { useHandle     `Handle'                      -- ^ plan handle, of type 'C2R'
  , useDevicePtr' `DevicePtr (Complex Float)'   -- ^ input data
  , useDevicePtr' `DevicePtr Float'             -- ^ output data
  }
  -> `()' checkStatus*- #}


-- | Executes a double-precision complex-to-real, implicitly forward, transform.
--
-- If the input and output device pointers refer to the same address, an
-- in-place transform is executed.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftexecc2r-cufftexecz2d>
--
{-# INLINEABLE execZ2D #-}
{# fun unsafe cufftExecZ2D as execZ2D
  { useHandle     `Handle'                      -- ^ plan handle, of type 'Z2D'
  , useDevicePtr' `DevicePtr (Complex Double)'  -- ^ input data
  , useDevicePtr' `DevicePtr Double'            -- ^ output data
  }
  -> `()' checkStatus*- #}


-- c2hs treats pointers to complex values as 'Ptr ()' (they are structs on the
-- C side) and uses 'CFloat' instead of 'Float', etc.
--
{-# INLINE useDevicePtr' #-}
useDevicePtr' :: DevicePtr a -> Ptr b
useDevicePtr' = castPtr . useDevicePtr

