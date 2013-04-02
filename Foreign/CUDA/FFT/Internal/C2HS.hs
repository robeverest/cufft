
module Foreign.CUDA.FFT.Internal.C2HS (

  -- * Conversion between C and Haskell types
  cIntConv, cFloatConv, cToBool, cFromBool, cToEnum, cFromEnum

) where

-- System
import Foreign


-- Conversions -----------------------------------------------------------------
--

-- | Integral conversion
--
{-# INLINE cIntConv #-}
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

-- | Floating conversion
--
{-# INLINE cFloatConv #-}
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac
-- As this conversion by default goes via `Rational', it can be very slow...
{-# RULES
  "cFloatConv/Float->Float"   forall (x::Float).  cFloatConv x = x;
  "cFloatConv/Double->Double" forall (x::Double). cFloatConv x = x
 #-}

-- | Obtain C value from Haskell 'Bool'.
--
{-# INLINE cFromBool #-}
cFromBool :: Num a => Bool -> a
cFromBool  = fromBool

-- | Obtain Haskell 'Bool' from C value.
--
{-# INLINE cToBool #-}
cToBool :: (Eq a, Num a) => a -> Bool
cToBool  = toBool

-- | Convert a C enumeration to Haskell.
--
{-# INLINE cToEnum #-}
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

-- | Convert a Haskell enumeration to C.
--
{-# INLINE cFromEnum #-}
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum


