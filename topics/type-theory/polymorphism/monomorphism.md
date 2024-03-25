# Monomorphism

https://wiki.haskell.org/Monomorphism

*Monomorphic types* are the base types (Int, Float, Char) as well as fully applied, completely instantiated, polymorphic types; that is, polymorphic types become monomorphic types by consistent substitution of their type parameters.

The set of *the base types* includes language primitive types. `B` is the representative subset of the base types set: `B = {Int,Float,Char} ⊆ BaseTypes`.

Monomorphic functions work with a single, concrete, type, `t`. The type variable `t` here belongs to the meta-language (English) used to describe the object language (Haskell). So, `t` is a type variable in the meta-language that ranges over the base types, `t ∈ B`. In Haskell, it will always be replaced ("instantiated") by a concrete base type.


```hs
-- monomorphic function in meta-lang
mm :: t -> t

-- monomorphic function in Haskell
mm :: Int -> Int
```

The biggest problem with monomorphism is code duplication, which arises because each combination of types requires its own dedicated function. Considering just one arithmetic functions, viz. addition, this means writing an additional function for addition for each dictinct pair of numeric types:

```hs
data BaseNumericTypes
  = Int8  | Int16  | Int32  | Int64
  | Word8 | Word16 | Word32 | Word64
  | Float | Double

type Int = Int64
type Word = Word64
```

There are (at least these) 10 base numeric types, that require 10 functions for addition:

```hs
sumInt8 :: Int8 -> Int8 -> Int8
-- ...
sumDouble :: Double -> Double -> Double
```

However, then we gotta take into account addition with different, but compatible, types. The number of functions for addition thus grows extremely large. In fact, there is no thorough solution for this - at some lower level there will have to be one function per addition. But, at the higer level, like that of a PL there are several solutions for the problem of writing an overwhelming number of monomorphic functions for each generic function.

## Monomorphisation

Monomorphisation is one such solution.
