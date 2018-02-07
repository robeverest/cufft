-- |
-- Module      : Foreign.CUDA.FFT
-- Copyright   : [2013..2018] Robert Clifton-Everest, Trevor L. McDonell
-- License     : BSD
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The cuFFT library is an implementation of Fast Fourier Transform (FFT)
-- operations for NVIDIA GPUs.
--
-- The FFT is a divide-and-conquer algorithm for efficiently computing discrete
-- Fourier transforms of real- or complex-valued data sets. It is one of the
-- most important and widely used numerical algorithms in computational physics
-- and general signals processing. The cuFFT library provides a simple interface
-- for computing FFTs on a NVIDIA GPU.
--
-- To use operations from the cuFFT library, the user must allocate the required
-- arrays in the GPU memory space, fill them with data, call the desired
-- sequence of cuFFT library functions, then copy the results from the GPU
-- memory back to the host.
--
-- The <http://hackage.haskell.org/package/cuda cuda> package can be used for
-- writing to and retrieving data from the GPU.
--
-- For more information see <http://docs.nvidia.com/cuda/cufft/index.html>
--
-- [/Example/]
--
-- __TODO__
--

module Foreign.CUDA.FFT (

  -- * Control
  module Foreign.CUDA.FFT.Plan,
  module Foreign.CUDA.FFT.Stream,
  module Foreign.CUDA.FFT.Error,

  -- * Operations
  module Foreign.CUDA.FFT.Execute,

) where

import Foreign.CUDA.FFT.Error                                       ( CUFFTException(..) )
import Foreign.CUDA.FFT.Execute
import Foreign.CUDA.FFT.Plan                                        hiding ( useHandle )
import Foreign.CUDA.FFT.Stream

