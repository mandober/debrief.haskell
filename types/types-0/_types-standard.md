# Standard types

* unit
  - empty tuple
  - () :: ()
* Bool
  - `data Bool = False | True`, (otherwise = True)
  - not, (&&), (||)
  - membership classes:
    - Eq, Ord
    - Bounded, Enum
    - Show, Read
* Ordering
  - `Ordering = LT | EQ | GT`
  - result of Ord functions
* Char
  - unicode chars
  - String :: [Char]
* Word
  - `data Word = GHC.Types.W# GHC.Prim.Word#`
  - membership classes:
    - Eq, Ord
    - Bounded, Enum
    - Show, Read
    - Num, Integral, Real
* Int
  - `data Int = I# Int#`
  - `Int` basic type
  - bounded, machne-sized, 32-bit or 64-bit
  - membership in classes:
    - Eq, Ord
    - Bounded, Enum
    - Read, Show
    - Num, Integral Real

* Maybe
  - Maybe = Nothing | Just a, maybe
* Either
  - Either = Left | Right), either

* tuples
  - fst/snd
  - curry/uncurry
* lists
  - `:`, `[]`

* Number types and classes
  * *Num*
    - Word
    - Int
    - Integer
    - Float
    - Double
    - Rational
    - Scientific
  * *Integral*
    - Word
    - Int
    - Integer
  * *Floating*
    - Float
    - Double
  * *Fractional*
    - Float
    - Double
    - Rational
    - Scientific, GHC.Real
