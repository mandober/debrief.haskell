# Type-Level Literals

GHC supports numeric and string literals at the type level, giving convenient access to a large number of predefined type-level constants. This feature is enabled by the `DataKinds` extension.

Numeric literals are of kind `Nat`, while string literals are of kind `Symbol`.

The kinds of the literals and all other low-level operations for this feature are defined in modules `GHC.TypeLits` and `GHC.TypeNats`. Note that these modules define some type-level operators that clash with their value-level counterparts (e.g. `(+)`). Import and export declarations referring to these operators require an explicit namespace annotation (using `type` export/import modifier).

Here is an example of using type-level numeric literals to provide a safe interface to a low-level function:

```hs
{-# LANGUAGE DataKinds #-}

import GHC.TypeLits
import Data.Word
import Foreign

newtype ArrPtr (n :: Nat) a = ArrPtr (Ptr a)

clearPage :: ArrPtr 4096 Word8 -> IO ()
clearPage (ArrPtr p) = ...
```

Here is an example of using type-level string literals to simulate simple record operations:

```hs
data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

data Point = Point Int Int deriving Show

instance Has Point "x" Int where from (Point x _) _ = x
instance Has Point "y" Int where from (Point _ y) _ = y

example = from (Point 1 2) (Get :: Label "x")
```


## Runtime Values for Type-Level Literals

Sometimes it is useful to access the value-level literal associated with
a type-level literal. This is done with the functions `natVal` and
`symbolVal`. For example: :

    GHC.TypeLits> natVal (Proxy :: Proxy 2)
    2

These functions are overloaded because they need to return a different
result, depending on the type at which they are instantiated. :

    natVal :: KnownNat n => proxy n -> Natural  -- from GHC.TypeNats
    natVal :: KnownNat n => proxy n -> Integer  -- from GHC.TypeLits

    -- instance KnownNat 0
    -- instance KnownNat 1
    -- instance KnownNat 2
    -- ...

GHC discharges the constraint as soon as it knows what concrete
type-level literal is being used in the program. Note that this works
only for *literals* and not arbitrary type expressions. For example, a
constraint of the form `KnownNat (a + b)` will *not* be simplified to
`(KnownNat a, KnownNat b)`; instead, GHC will keep the constraint as is,
until it can simplify `a + b` to a constant value.

It is also possible to convert a run-time integer or string value to the
corresponding type-level literal. Of course, the resulting type literal
will be unknown at compile-time, so it is hidden in an existential type.
The conversion may be performed using `someNatVal` for integers and
`someSymbolVal` for strings: :

    someNatVal :: Natural -> Maybe SomeNat  -- from GHC.TypeNats
    someNatVal :: Integer -> Maybe SomeNat  -- from GHC.TypeLits

    SomeNat    :: KnownNat n => Proxy n -> SomeNat

The operations on strings are similar.

Computing With Type-Level Naturals {#typelit-tyfuns}
----------------------------------

GHC 7.8 can evaluate arithmetic expressions involving type-level natural
numbers. Such expressions may be constructed using the type-families
`(+), (*), (^)` for addition, multiplication, and exponentiation.
Numbers may be compared using `(<=?)`, which returns a promoted boolean
value, or `(<=)`, which compares numbers as a constraint. For example:

``` {.sourceCode .none}
GHC.TypeLits> natVal (Proxy :: Proxy (2 + 3))
5
```

At present, GHC is quite limited in its reasoning about arithmetic: it
will only evaluate the arithmetic type functions and compare the
results\-\-- in the same way that it does for any other type function.
In particular, it does not know more general facts about arithmetic,
such as the commutativity and associativity of `(+)`, for example.

However, it is possible to perform a bit of \"backwards\" evaluation.
For example, here is how we could get GHC to compute arbitrary
logarithms at the type level:

``` {.sourceCode .none}
lg :: Proxy base -> Proxy (base ^ pow) -> Proxy pow
lg _ _ = Proxy

GHC.TypeLits> natVal (lg (Proxy :: Proxy 2) (Proxy :: Proxy 8))
3
```
