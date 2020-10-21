# Numeric types

https://wiki.haskell.org/Generic_number_type
https://www.haskell.org/tutorial/numbers.html
https://stackoverflow.com/questions/31841767/explanation-of-numbers-in-haskell
https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270011
https://wiki.haskell.org/Converting_numbers
https://en.wikibooks.org/wiki/Haskell/Type_basics_II
https://mmhaskell.com/blog/2017/5/22/numbers-of-every-shape-and-size
https://www.haskelltutorials.com/without-theory/basic-types-and-functions.html

## Integers

Bounded => Int, Int8, Int16, Int32, Int64
Bounded => Word, Word8, Word16, Word32, Word64

Integer: If the number is too large to fit in a single register in memory, Haskell will use a byte array to represent the number.


GHC.Enum
GHC.Real
GHC.Num
GHC.Natural
Data.ByteString
GHC.Int
GHC.IntWord64
GHC.Integer
GHC.Integer.GMP.Internals
GHC.Integer.Logarithms
GHC.Integer.Logarithms.Internals
GHC.Integer.Type
GHC.Integer.Type.GmpLimb
GHC.Integer.Type.BigNat
GHC.Float
GHC.Float.ConversionUtils
GHC.Float.RealFracMethods
GHC.IntWord64


```hs
inf :: Rational
inf = GHC.Real.infinity -- 1 % 0

-- Defined in GHC.Natural
data Natural
    = NatS# integer-gmp-1.0.2.0:GHC.Integer.Type.GmpLimb#
    | NatJ# {-# UNPACK #-}integer-gmp-1.0.2.0:GHC.Integer.Type.BigNat
```

toEnum
toInteger
toRational
isValidNatural
wordToNatural
wordToNatural#
wordToNaturalBase




Integral typeclass
- conversion between integral types
- provides `toInteger :: Integral a => a -> Integer`
- `div` and `mod` round toward negative infinity
- `quot` and `rem` round toward 0


## Converting numbers

### Converting between integral types

fromIntegral converts from any Integral into any Num type, including Int, Integer, Rational, Double.

fromIntegral :: (Num b, Integral a) => a -> b
