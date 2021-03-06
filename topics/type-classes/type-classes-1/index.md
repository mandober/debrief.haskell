# Type Classes I




Basic type classes
- Eq
- Ord (depends on Eq)
- Show
- Read
- Bounded
- Enum

* Derivable classes: Eq, Ord, Show, Read, Bounded, Enum

* Numeric classes:
  - Mother numeric: `Num`
  - Main numeric types: Integral, Floating
    - Integral: Int, Integer
    - Floating: Float, Double
  - Extra numeric types: Fractional, Real




* Ord
  * functions:
    - compare
    - (<), (<=), (>=), (>)
    - max, min

* Enum
  * functions:
    - succ, pred
    - toEnum, fromEnum
    - enumFrom, enumFromThen, enumFromTo, enumFromThenTo

* Bounded
  * `class Bounded a where`
  * functions:
    - minBound :: a
    - maxBound :: a
  * minimal impl: minBound, maxBound
  * instances: Word, Ordering, Int, Char, Bool

* Show
  * `class Show a where`
    - show      :: a -> String
    - showList  :: [a] -> ShowS
    - showsPrec :: Int -> a -> ShowS
  - minimal impl:
    - showsPrec | show
  - Defined in GHC.Show
  - instances:
    - ()
    - Word
    - TyCon
    - TrName
    - Ordering
    - Module
    - Integer
    - Int
    - Char
    - Bool
    - Float
    - Double
    - a => Show [a]
    - a => Show (Maybe a)
    - (Show b, Show a) => Show (Either a b)
    - (Show a, Show b) => Show (a, b) ...and other tuples

* Read
  - `class Read a where`
    - readsPrec :: Int -> ReadS a
    - readList :: ReadS [a]
    - GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
    - GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec [a]
  - MINIMAL: readsPrec | readPrec
  - instances:
    - instance Read a => Read [a]
    - instance Read Word
    - instance Read Ordering
    - instance Read a => Read (Maybe a)
    - instance Read Integer
    - instance Read Int
    - instance Read Float
    - instance Read Double
    - instance Read Char
    - instance Read Bool
    - instance Read ()
    - instance (Read b, Read a) => Read (Either a b)
    - instance (Read a, Read b) => Read (a, b) ...and other tuples

* Num
  * `class Num a where`
  * members:
    - Word
    - Int, Integer
    - Float, Double
      * in GHC.Int
        - instance Num Int8
        - instance Num Int64
        - instance Num Int32
        - instance Num Int16
      * in GHC.Real
        instance Integral a => Num (Ratio a) -- Defined in ‘GHC.Real’
      * in GHC.Num
        instance Num Word -- Defined in ‘GHC.Num’
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Int -- Defined in ‘GHC.Num’
      * in GHC.Float
        instance Num Float -- Defined in ‘GHC.Float’
        instance Num Double -- Defined in ‘GHC.Float’
  * functions:
    - (+)    :: a -> a -> a
    - (-)    :: a -> a -> a
    - (*)    :: a -> a -> a
    - abs    :: a -> a
    - negate :: a -> a
    - signum :: a -> a
    - fromInteger :: Integer -> a
  * minimal impl:
    - (+), (*), abs, signum, fromInteger, negate | (-)
* Integral
  - `class (Real a, Enum a) => Integral a where`
    quot :: a -> a -> a
    rem :: a -> a -> a
    div :: a -> a -> a
    mod :: a -> a -> a
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a)
    toInteger :: a -> Integer
  - MINIMAL: quotRem, toInteger
  - instances:
    - instance Integral Word
    - instance Integral Integer
    - instance Integral Int
* Fractional
  - class Fractional a => Floating a where
    pi :: a
    exp :: a -> a
    log :: a -> a
    sqrt :: a -> a
    (**) :: a -> a -> a
    logBase :: a -> a -> a
    sin :: a -> a
    cos :: a -> a
    tan :: a -> a
    asin :: a -> a
    acos :: a -> a
    atan :: a -> a
    sinh :: a -> a
    cosh :: a -> a
    tanh :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a
    GHC.Float.log1p :: a -> a
    GHC.Float.expm1 :: a -> a
    GHC.Float.log1pexp :: a -> a
    GHC.Float.log1mexp :: a -> a
  - MINIMAL:
    - pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh
  - instances:
    instance Floating Float -- Defined in GHC.Float
    instance Floating Double -- Defined in GHC.Float

* Real
  - class (Num a, Ord a) => Real a where
    - toRational :: a -> Rational
  - MINIMAL: toRational
  - Defined in GHC.Real
  - instances:
    - instance Real Word
    - instance Real Integer
    - instance Real Int
    - instance Real Float
    - instance Real Double


- Num
  - (+), (-), (*)
  - negate
  - abs, signum
  - fromInteger
- Real
  - toRational
- Integral
  - quot, rem
  - div, mod
  - quotRem, divMod
  - toInteger
- Fractional
  - (/)
  - recip
  - fromRational
- Floating
  - pi
  - exp, (**)
  - sqrt
  - log, logBase
  - sin, cos, tan,
  - asin, acos, atan
  - sinh, cosh, tanh
  - asinh, acosh, atanh
- RealFrac
  - properFraction
  - truncate, round
  - ceiling, floor
- RealFloat
  - floatRadix, floatDigits, floatRange
  - decodeFloat, encodeFloat
  - exponent, significand
  - isNaN, isInfinite, isDenormalized, isIEEE, isNegativeZero
  - scaleFloat
  - atan2

* Numeric functions
  - subtract
  - even, odd
  - gcd, lcm
  - (^), (^^)
  - fromIntegral
  - realToFrac
