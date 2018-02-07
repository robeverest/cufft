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

  execC2C, execZ2Z,
  execR2C, execD2Z,
  execC2R, execZ2D,

) where

-- Friends
import Foreign.CUDA.FFT.Error
import Foreign.CUDA.FFT.Plan
import Foreign.CUDA.FFT.Internal.C2HS

import Foreign.CUDA.Ptr

-- System
import Foreign
import Foreign.C

#include <cbits/wrap.h>
{# context lib="cufft" #}


-- | Executes a single-precision complex-to-complex transform plan in the
-- transform direction specified by the fourth argument
--
execC2C :: Handle -> DevicePtr Float -> DevicePtr Float -> Int -> IO ()
execC2C ctx i o dir = nothingIfOk =<< cufftExecC2C ctx i o dir

{# fun unsafe cufftExecC2C
  { useHandle  `Handle'
  , useDev     `DevicePtr Float'
  , useDev     `DevicePtr Float'
  ,            `Int'             } -> `Result' cToEnum #}
  where
    useDev      = useDevicePtr . castDevPtr

-- | Executes a double-precision complex-to-complex transform plan in the
-- transform direction specified by the fourth argument
--
execZ2Z :: Handle -> DevicePtr Double -> DevicePtr Double -> Int -> IO ()
execZ2Z ctx i o dir = nothingIfOk =<< cufftExecZ2Z ctx i o dir

{# fun unsafe cufftExecZ2Z
  { useHandle  `Handle'
  , useDev     `DevicePtr Double'
  , useDev     `DevicePtr Double'
  ,            `Int'             } -> `Result' cToEnum #}
  where
    useDev      = useDevicePtr . castDevPtr

-- | Executes a single-precision real-to-complex (implicitly forward)
-- transform plan
--
execR2C :: Handle -> DevicePtr Float -> DevicePtr Float -> IO ()
execR2C ctx i o = nothingIfOk =<< cufftExecR2C ctx i o

{# fun unsafe cufftExecR2C
  { useHandle  `Handle'
  , useDev     `DevicePtr Float'
  , useDev     `DevicePtr Float' } -> `Result' cToEnum #}
  where
    useDev      = useDevicePtr . castDevPtr

-- | Executes a double-precision real-to-complex (implicitly forward)
-- transform plan
--
execD2Z :: Handle -> DevicePtr Double -> DevicePtr Double -> IO ()
execD2Z ctx i o = nothingIfOk =<< cufftExecD2Z ctx i o

{# fun unsafe cufftExecD2Z
  { useHandle  `Handle'
  , useDev     `DevicePtr Double'
  , useDev     `DevicePtr Double' } -> `Result' cToEnum #}
  where
    useDev      = useDevicePtr . castDevPtr

-- | Executes a single-precision complex-to-real (implicitly forward)
-- transform plan
--
execC2R :: Handle -> DevicePtr Float -> DevicePtr Float -> IO ()
execC2R ctx i o = nothingIfOk =<< cufftExecC2R ctx i o

{# fun unsafe cufftExecC2R
  { useHandle  `Handle'
  , useDev     `DevicePtr Float'
  , useDev     `DevicePtr Float' } -> `Result' cToEnum #}
  where
    useDev      = useDevicePtr . castDevPtr

-- | Executes a double-precision complex-to-real (implicitly forward)
-- transform plan
--
execZ2D :: Handle -> DevicePtr Double -> DevicePtr Double -> IO ()
execZ2D ctx i o = nothingIfOk =<< cufftExecZ2D ctx i o

{# fun unsafe cufftExecZ2D
  { useHandle  `Handle'
  , useDev     `DevicePtr Double'
  , useDev     `DevicePtr Double' } -> `Result' cToEnum #}
  where
    useDev      = useDevicePtr . castDevPtr

