

```hs
class (Num a, Ord a) => Real a where
    toRational :: a -> Rational
-- MINIMAL: toRational

class (Real a, Fractional a) => RealFrac a where
    properFraction  :: Integral b => a -> (b, a)
    truncate        :: Integral b => a -> b
    round           :: Integral b => a -> b
    ceiling         :: Integral b => a -> b
    floor           :: Integral b => a -> b
-- MINIMAL: properFraction

class (RealFrac a, Floating a) => RealFloat a where
    scaleFloat     :: Int -> a -> a
    encodeFloat    :: Integer -> Int -> a
    decodeFloat    :: a -> (Integer, Int)
    floatRange     :: a -> (Int, Int)
    floatRadix     :: a -> Integer
    floatDigits    :: a -> Int
    exponent       :: a -> Int
    significand    :: a -> a
    atan2          :: a -> a -> a
    isNaN          :: a -> Bool
    isIEEE         :: a -> Bool
    isInfinite     :: a -> Bool
    isDenormalized :: a -> Bool
    isNegativeZero :: a -> Bool
-- MINIMAL: floatRadix, floatDigits, floatRange, decodeFloat, isNaN
-- encodeFloat, isInfinite, isDenormalized, isNegativeZero, isIEEE
```
