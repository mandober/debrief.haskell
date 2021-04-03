# GHC.Real

```hs
import GHC.Real

data Ratio a = !a :% !a

type Rational = Ratio Integer

infinity   :: Rational
notANumber :: Rational

ratioPrec  :: Int
ratioPrec1 :: Int

overflowError               :: a
underflowError              :: a
divZeroError                :: a
ratioZeroDenominatorError   :: a


(%)     :: Integral a => a -> a -> Ratio a

(^)     :: (Num a, Integral b)        => a -> b -> a
(^^)    :: (Fractional a, Integral b) => a -> b -> a

(^%^)   :: Integral a => Rational -> a -> Rational
(^^%^^) :: Integral a => Rational -> a -> Rational

odd  :: Integral a => a -> Bool
even :: Integral a => a -> Bool

lcm         :: Integral a => a -> a -> a
gcd         :: Integral a => a -> a -> a
gcdInt'     :: Int -> Int -> Int
gcdWord'    :: Word -> Word -> Word

numerator   :: Ratio a -> a
denominator :: Ratio a -> a

toRational      :: Real a => a -> Rational
fromRational    :: Fractional a => Rational -> a
realToFrac      :: (Real a, Fractional b) => a -> b
toInteger       :: Integral a => a -> Integer
fromIntegral    :: (Integral a, Num b) => a -> b

integralEnumFrom       :: (Integral a, Bounded a) => a -> [a]
integralEnumFromTo     :: Integral a => a -> a -> [a]
integralEnumFromThen   :: (Integral a, Bounded a) => a -> a -> [a]
integralEnumFromThenTo :: Integral a => a -> a -> a -> [a]

numericEnumFrom        :: Fractional a => a -> [a]
numericEnumFromTo      :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromThen    :: Fractional a => a -> a -> [a]
numericEnumFromThenTo  :: (Ord a, Fractional a) => a -> a -> a -> [a]

reduce          :: Integral a => a -> a -> Ratio a
showSigned      :: Real a => (a -> ShowS) -> Int -> a -> ShowS
properFraction  :: RealFrac a => Integral b => a -> (b, a)

(/)             :: Fractional a => a -> a -> a
recip           :: Fractional a => a -> a

rem             :: Integral a => a -> a -> a
mod             :: Integral a => a -> a -> a
div             :: Integral a => a -> a -> a
quot            :: Integral a => a -> a -> a
divMod          :: Integral a => a -> a -> (a, a)
quotRem         :: Integral a => a -> a -> (a, a)

truncate        :: RealFrac a => Integral b => a -> b
round           :: RealFrac a => Integral b => a -> b
ceiling         :: RealFrac a => Integral b => a -> b
floor           :: RealFrac a => Integral b => a -> b



class Num a => Fractional a where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: Rational -> a
{-# MINIMAL fromRational, (recip | (/)) #-}

class (Real a, Enum a) => Integral a where
    quot      :: a -> a -> a
    rem       :: a -> a -> a
    div       :: a -> a -> a
    mod       :: a -> a -> a
    quotRem   :: a -> a -> (a, a)
    divMod    :: a -> a -> (a, a)
    toInteger :: a -> Integer
{-# MINIMAL quotRem, toInteger #-}

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational
{-# MINIMAL toRational #-}

class (Real a, Fractional a) => RealFrac a where
    properFraction :: Integral b => a -> (b, a)
    truncate       :: Integral b => a -> b
    round          :: Integral b => a -> b
    ceiling        :: Integral b => a -> b
    floor          :: Integral b => a -> b
{-# MINIMAL properFraction #-}
```
