# Numeric types




```hs
-- fromIntegral
fromIntegral 5 :: Int       -- 5
fromIntegral 5 :: Double    -- 5.0
fromIntegral 5 :: Rational  -- 5 % 1



minBound :: Int             -- -9_223_372_036_854_775_808 (-A)  -9..808
maxBound :: Int             --  9_223_372_036_854_775_807 (+A)   9..807

maxBound + 1 :: Int         -- -A
minBound - 1 :: Int         -- +A

- 2^63 :: Int               -- -A
2^63 - 1 :: Int             -- +A

import Data.Bits

1 `shiftL` 63 :: Int        -- -A
1 `shiftL` 63 - 1 :: Int    -- +A


--  9223372036854775807
-- -9223372036854775808

0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111
-- 18446744073709551615

0b_01111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111
-- 9223372036854775807

Int range -9223372036854775808..9223372036854775807
```



## Refs

https://stackoverflow.com/questions/31841767/explanation-of-numbers-in-haskell
https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270011
https://mmhaskell.com/blog/2017/5/22/numbers-of-every-shape-and-size
https://www.haskelltutorials.com/without-theory/basic-types-and-functions.html
https://en.wikibooks.org/wiki/Haskell/Type_basics_II

https://www.haskell.org/tutorial/numbers.html
https://wiki.haskell.org/Generic_number_type


## Integers

* *Integral*
  - Integer
  - Int
    - Int8
    - Int16
    - Int32
    - Int64
  - Word
    - Word8
    - Word16
    - Word32
    - Word64
  - Natural



* If small enough Haskell will store an `Integer` in a single register; if too large for that then a byte array is used to represent it, aka BigNum.


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
https://wiki.haskell.org/Converting_numbers


### Converting from and between integral types

* *Integral* memebers are only whole numbers, not fractions.

* `Integer` is the arbitrary-precision integer (BigNum)

* `Int` is the fixed-width machine-specific integer; min guaranteed range is from `−2²⁹` to `2²⁹ − 1`. On x86-64, it os 64-bit wide from `- 2^63 :: Int`
to `2^63 - 1 :: Int`

* To convert `Integral a -> Num a` use `fromIntegral` it converts from any Integral type into Num type (Int, Integer, Rational, Double, et al.)


## Rational

* Rational values represent rational nums exactly as the ratio of 2 Integers.

* Applying `toRational` to an *Integral* number `n` will produce the rational number `n % 1`; applying `toRational` to a *Real* number will produce its rational value (or its closest approximation).

* construct Rational values explicitly using the (%) operator, e.g. `2 % 3`




https://markkarpov.com/tutorial/generics.html
https://chrisdone.com/posts/data-typeable/

https://wiki.haskell.org/index.php?title=Special:Search&limit=20&offset=20&profile=default&search=numbers

https://wiki.haskell.org/Applications_and_libraries/Mathematics
https://wiki.haskell.org/Applicative-numbers
https://wiki.haskell.org/Blog_articles/Comparisons
https://wiki.haskell.org/Blow_your_mind
https://wiki.haskell.org/Circular_programming
https://wiki.haskell.org/Combinatory_logic
https://wiki.haskell.org/Converting_numbers
https://wiki.haskell.org/Cookbook/Numbers
https://wiki.haskell.org/Exact_real_arithmetic
https://wiki.haskell.org/Example_code
https://wiki.haskell.org/Floating_point_numbers
https://wiki.haskell.org/Global_keys
https://wiki.haskell.org/Haskell_Quiz/Weird_Numbers
https://wiki.haskell.org/Numeric_Prelude
https://wiki.haskell.org/Peano_numbers
https://wiki.haskell.org/Phone_number
https://wiki.haskell.org/Prelude_extensions
https://wiki.haskell.org/Prime_numbers
https://wiki.haskell.org/Prime_numbers_miscellaneous
https://wiki.haskell.org/Recursive_function_theory
https://wiki.haskell.org/Simple_to_complex

http://prime.haskell.org/wiki/NumericClasses
http://magma.maths.usyd.edu.au/magma/handbook/
http://lambda-the-ultimate.org/node/1655#comment-20299
