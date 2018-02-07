{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.FFT.Plan
-- Copyright   : [2013..2018] Robert Clifton-Everest, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.FFT.Plan (

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


-- | A handle used to store and access cuFFT plans. A handle is created by the
-- FFT planning functions (e.g. 'plan1D') and used to execute the plan.
--
newtype Handle = Handle { useHandle :: {# type cufftHandle #}}


-- | The cuFFT library supports complex- and real-valued transforms. This data
-- type enumerates the kind of transform a plan will execute.
--
-- Key:
--
--   * __R__: real (32-bit float)
--   * __D__: double (64-bit float)
--   * __C__: single-precision complex numbers (32-bit, interleaved)
--   * __Z__: double-precision complex numbers (64-bit, interleaved)
--
{# enum cufftType as Type
  {}
  with prefix="CUFFT" deriving (Eq, Show) #}

-- Context management ----------------------------------------------------------
--

-- |

-- | Creates a 1D FFT plan configured for a specified signal size and data type.
--
-- The third argument tells cuFFT how many 1D transforms, of size given by the
-- first argument, to configure. Consider using 'planMany' for multiple
-- transforms instead.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftplan1d>
--
{-# INLINEABLE plan1D #-}
{# fun unsafe cufftPlan1d as plan1D
  { alloca-   `Handle' peekHdl*
  ,           `Int'               -- ^ size of the transformation
  , cFromEnum `Type'              -- ^ transformation data type
  ,           `Int'               -- ^ number of one-dimensional transforms to configure
  }
  -> `()' checkStatus*- #}
  where
    peekHdl = liftM Handle . peek


-- | Creates a 2D FFT plan configuration for a specified signal size and data type.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftplan2d>
--
{-# INLINEABLE plan2D #-}
{# fun unsafe cufftPlan2d as plan2D
  { alloca-   `Handle' peekHdl*
  ,           `Int'               -- ^ the transform size in the /x/-dimension. This is the slowest changing dimension of a transform (strided in memory)
  ,           `Int'               -- ^ the transform size in the /y/-dimension. This is the fastest changing dimension of a transform (contiguous in memory)
  , cFromEnum `Type'              -- ^ transformation data type
  }
  -> `()' checkStatus*- #}
  where
    peekHdl = liftM Handle . peek


-- | Creates a 3D FFT plan configuration for a specified signal size and data type.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftplan3d>
--
{-# INLINEABLE plan3D #-}
{# fun unsafe cufftPlan3d as plan3D
  { alloca-   `Handle' peekHdl*
  ,           `Int'               -- ^ the transform size in the /x/-dimension. This is the slowest changing dimension of the transform (strided in memory)
  ,           `Int'               -- ^ the transform size in the /y/-dimension.
  ,           `Int'               -- ^ the transform size in the /z/-dimension. This is the fastest changing dimension of the transform (contiguous in memory)
  , cFromEnum `Type'              -- ^ transformation data type
  }
  -> `()' checkStatus*- #}
  where
    peekHdl = liftM Handle . peek


-- | Creates a batched plan configuration for many signals of a specified size
-- and data type in either 1, 2 or 3 dimensions.
--
-- This function supports more complicated input and output data layouts. If not
-- specified (that is, 'Nothing' is passed for either of the second or third
-- parameters), contiguous data arrays are assumed.
--
-- Data layout configuration consists of three fields, respectively:
--
--   * storage dimensions of the input data in memory
--   * the distance between two successive input elements in the innermost (least significant) dimension
--   * the distance between the first element of two consecutive signals in a batch of the input data
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftplanmany>
--
planMany :: [Int]                   -- ^ the size of each dimension of the transform, with @(n !! 0)@ being the size of the outermost dimension, and @(n !! rank-1)@ the innermost (contiguous) dimension of the transform.
         -> Maybe ([Int], Int, Int) -- ^ input data layout (if 'Nothing', the data is assumed to be contiguous)
         -> Maybe ([Int], Int, Int) -- ^ output data layout (if 'Nothing', the data is stored contiguously)
         -> Type                    -- ^ transformation type
         -> Int                     -- ^ batch size for this transform
         -> IO Handle
planMany n ilayout olayout t batch =
  cufftPlanMany (length n) n inembed istride idist onembed ostride odist t batch
  where
    (inembed, istride, idist) = fromMaybe ([], 0, 0) ilayout
    (onembed, ostride, odist) = fromMaybe ([], 0, 0) olayout

    peekHdl = liftM Handle . peek

    asArray [] f = f nullPtr
    asArray xs f = withArray (map fromIntegral xs) f

    {# fun unsafe cufftPlanMany
      { alloca-   `Handle' peekHdl*
      ,           `Int'
      , asArray*  `[Int]'
      , asArray*  `[Int]'
      ,           `Int'
      ,           `Int'
      , asArray*  `[Int]'
      ,           `Int'
      ,           `Int'
      , cFromEnum `Type'
      ,           `Int'
      }
      -> `()' checkStatus*- #}


-- | Release resources associated with the given plan. This function should be
-- called once a plan is no longer needed, to avoid wasting GPU memory.
--
-- <http://docs.nvidia.com/cuda/cufft/index.html#function-cufftdestroy>
--
{-# INLINEABLE destroy #-}
{# fun unsafe cufftDestroy as destroy
  { useHandle `Handle' } -> `()' checkStatus*- #}

