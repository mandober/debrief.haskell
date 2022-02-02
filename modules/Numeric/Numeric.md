# Numeric

* Numeric
  - Numeric.Lens
  - Numeric.Natural
  - Numeric.Natural.Lens

```hs
import Numeric

readDec       :: (Eq a, Num a) => ReadS a
readHex       :: (Eq a, Num a) => ReadS a
readOct       :: (Eq a, Num a) => ReadS a
readFloat     :: RealFrac a    => ReadS a

readSigned    :: Real a => ReadS a -> ReadS a

readInt       :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a

showEFloat    :: RealFloat a => Maybe Int -> a -> ShowS
showFFloat    :: RealFloat a => Maybe Int -> a -> ShowS
showFFloatAlt :: RealFloat a => Maybe Int -> a -> ShowS
showGFloat    :: RealFloat a => Maybe Int -> a -> ShowS
showGFloatAlt :: RealFloat a => Maybe Int -> a -> ShowS

showHex       :: (Integral a, Show a) => a -> ShowS
showHFloat    :: RealFloat a          => a -> ShowS
showInt       ::  Integral a          => a -> ShowS
showOct       :: (Integral a, Show a) => a -> ShowS

GHC.Read.lexDigits      :: ReadS String

GHC.Float.floatToDigits :: RealFloat a => Integer  -> a -> ([Int], Int)
GHC.Float.fromRat       :: RealFloat a => Rational -> a
GHC.Float.showFloat     :: RealFloat a => a -> ShowS

GHC.Real.showSigned     :: Real a => (a -> ShowS) -> Int -> a -> ShowS

-- imported via Numeric
showIntAtBase :: (Integral a, Show a)  => a -> (Int -> Char) -> a -> ShowS

GHC.Float.expm1     :: Floating a => a -> a
GHC.Float.log1mexp  :: Floating a => a -> a
GHC.Float.log1p     :: Floating a => a -> a
GHC.Float.log1pexp  :: Floating a => a -> a

-- imported via Prelude
pi                  :: Floating a => a
acos                :: Floating a => a -> a
acosh               :: Floating a => a -> a
asin                :: Floating a => a -> a
asinh               :: Floating a => a -> a
atan                :: Floating a => a -> a
atanh               :: Floating a => a -> a
cos                 :: Floating a => a -> a
cosh                :: Floating a => a -> a
exp                 :: Floating a => a -> a
sin                 :: Floating a => a -> a
sinh                :: Floating a => a -> a
sqrt                :: Floating a => a -> a
tan                 :: Floating a => a -> a
tanh                :: Floating a => a -> a
log                 :: Floating a => a -> a
logBase             :: Floating a => a -> a -> a
(**)                :: Floating a => a -> a -> a

class Fractional a => Floating a
```
