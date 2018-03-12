Haskell FFI Bindings to CUDA FFT
================================

[![Travis build status](https://img.shields.io/travis/tmcdonell/cufft/master.svg?label=linux)](https://travis-ci.org/tmcdonell/cufft)
[![AppVeyor build status](https://img.shields.io/appveyor/ci/tmcdonell/cufft/master.svg?label=windows)](https://ci.appveyor.com/project/tmcdonell/cufft)
[![Stackage LTS](https://stackage.org/package/cufft/badge/lts)](https://stackage.org/lts/package/cufft)
[![Stackage Nightly](https://stackage.org/package/cufft/badge/nightly)](https://stackage.org/nightly/package/cufft)
[![Hackage](https://img.shields.io/hackage/v/cufft.svg)](https://hackage.haskell.org/package/cufft)

The cuFFT library provides high performance implementations of Fast Fourier
Transform (FFT) operations on NVIDIA GPUs. This is a collection of bindings to
allow you to call those functions from Haskell. You will need to install the
CUDA driver and developer toolkit.

  <https://developer.nvidia.com/cuda-toolkit>

The configure script will look for your CUDA installation in the standard
places, and if the `nvcc` compiler is found in your `PATH`, relative to that.

