Haskell FFI Bindings to CUDA FFT
================================

[![Build status](https://travis-ci.org/tmcdonell/cufft.svg?branch=master)](https://travis-ci.org/tmcdonell/cufft)

The CUFFT library provides high performance implementations of Fast Fourier
Transform (FFT) operations on NVIDIA GPUs. This is a collection of bindings to
allow you to call those functions from Haskell. You will need to install the
CUDA driver and developer toolkit.

[http://developer.nvidia.com/cuda-downloads][cuda]

The configure script will look for your CUDA installation in the standard
places, and if the `nvcc` compiler is found in your `PATH`, relative to that.

[cuda]: http://developer.nvidia.com/object/cuda.html


