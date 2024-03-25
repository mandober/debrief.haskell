# Algebra of algebraic data types

## Algebraic data types

Algebraic Data Types are types built from a set `{0,1,+,⨯,↑}`, consisting of the base data types (the empty type 0, the unit type 1), and elementary algebraic operations, here addition (`+`), multiplication (`⨯`) and exponentiation (`↑`), but can be extended with additional operations, all of which behave similarly as they do in algebra.

In Haskell, each item in the set `{0, 1, +, ⨯, ↑}` is represented by a canonical type (for base types), or type construction (for ops), each using a specific type ctor.

## Isomorphic types

A lot of concrete types (and type constructions, i.e. type ctors), even though not nominally the same, are actually structurally equal, and thus we decide on a particular representative type - the canonical type - to stand for each group. All types that are member of the same group are isomorphic to each other.

Types that have the same cardinality are isomorphic to each other. For our purposes here, is the same as being equal - they are equal enough. Or, more precisely, they are equal up to an isomorphism.

For example, a user-defined data type `Dir`, is inhabited by two values - the two nullary data ctors `Left` and `Right`. Since the `Bool` data type alike, these two must be isomorphic, so converting between `Dir` and `Bool` is trivial. This isomorphism is given by defining two function, `to` and `from` (from the perspective of one of them) such that `to ∘ from = id = from ∘ to`.

```hs
data Bool = True | False
data Dir = Left | Right

-- from the perspective of Bool
into :: Bool -> Dir
into False = Left
into True = Right

from :: Dir -> Bool
from Left = False
from Right = True

into . from = from . into = id
```

As another example, any nullary data ctor represents the unit type. So, the `Nothing` data ctor in `Maybe`, and `Nil` data ctor in lists (i.e. `[]` data ctor), could both be replaced with the canonical unit, `()`, if that wouldn't introduce name clashes - however, during compilation GHC does in fact replaces all such unit-like ctors with the canonical unit.

```hs
-- actual def
data Maybe a = Nothing | Just a
-- but 'Nothing' might as well be called anything else, or just ()
data Maybe a = () | Just a
-- also 'Nil' or '[]', aka the empty list data ctor
data List a = Nil | Cons a (List a)
```

## The empty type

The empty type is an uninhabited type that plays the role of additive unit and is thus denoted by `0`. To emphasize that it is a type, `𝟘` is also used.

In Haskell, the canonical type that represents 0 is the uninhabited type `Void` from `Data.Void` (has to be imported, not available in the Prelude). However, the `Void` type is not special in any way, it is user-definable by declaring a new data type without data ctors.

```hs
data Empty
data Zero
```

Tecnically, in Haskell, neither `Void` nor any isomorphic type is actually uninhabited becase all types are lifted - all types are extended with a special value bottom (⟘); bottom is a value of any type.

## Unit type

The unit type is an inhabited type that plays the role of multiplicative unit and is thus denoted by `1`. To emphasize that it is a type, `𝟙` is also used.

In Haskell, the canonical unit type is `()` that has a single value, also denoted `()` and built by using the nullary data ctor `()` - the data ctor is the value. Again, (apart from ita particular name) `()` is nothing special and may be user-defined by declaring a new data type with a single nullary data ctor.

```hs
data Unit = Unit
data One = One
```

## Sum types

Sum types are compound types.

Sum or coproduct types represent the disjunction of types, i.e. the logical OR.

To construct a value of a sum type, you only need to provide one of the constituent value.

Cardinality-wise, the number of values of a sum type is the sum of the cardinality of the constituent typess, `|a| + |b|`.

In Haskell, the canonical sum type is `Either`, a + b = Either a b

## Product types

Product types are compound types.

Product types represent the conjunction of types, i.e. the logical AND.

To construct a value of a product type, you have to provide all of the constituent values.

Cardinality-wise, the number of values of a product type is the product of the cardinality of the constituent types, `|a| ⨯ |b|`.

In Haskell, the canonical sum type is `(,)`, a ⨯ b = (a, b)

## Exponential types

- Exponential types are compound types.
- Exponential types represent the logical implication.
- Exponential types represent the iterated multiplication.
- Exponential types are functions, i.e. the function type
- Exponential type `a -> b` or `bᵃ`
- Cardinality-wise, the number of values of an exponential type, `a -> b` is the cardinality of the output type to the power of the cardinality of the input type, `bᵃ`, more preciely, `|b| ↑ |a|`

In Haskell, the canonical (and only) exponential type is `(->)`, b ↑ a = a -> b

## The set of types and type ctors

`{𝟘, 𝟙, +, ⨯, ↑}`
- 0  `Void`, the empty data type, additive unit
- 1  `()` (unit), nullary data ctor (`Nil`, `Nothing`), multiplicative unit
- (+) Sum type `a + b`, canonically `Either a b`
- (⨯) Product type, `a ⨯ b`, canonically pair `(a, b)`
- (↑) Exponential type, `b ↑ a`, canonically function `a -> b`


## Canonical representations

```hs
data Void    ≅  0
data () = () ≅  1
Either a b   ≅  a + b
(a, b)       ≅  a * b
(a -> b)     ≅  b ↑ a
```


## Boolean is canonically Either

```hs
data Bool = True | False
```

This is `1 + 1` as both `True` and `False` are nullary data ctors. The canonical type for representing sums is `Either`, so `Bool` is canonically:   
`type Bool = Either True False`   
that is, just   
`type Bool = Either () ()`
becuase Either can tell apart each component:
- the first `()` is actually `Left ()`
- the second `()` is actually `Right ()`

```hs
type Bool = Either () ()
```


## Maybe is canonically Either

```hs
Maybe a = Nothing | Just a
```

`Maybe a` is 1 + a 
≅ Either 1 a 
≅ Either () a

```hs
type Maybe a = Either () a
-- or, in point-free style just
type Maybe = Either ()
```

## Type constructions

In Haskell, types are declared using the 3 keywords, `data`, `newtype` and `type`.

>Haskell types are possibly recursive sums of products.

An ADT is a sum of products declared using the `data` keyword, and it consists of a set of alternatives (variants), split with `|`, (which are the sum type part); however, each variant is a poduct type made from zero or more fields - thus, the overall type is the sum of products (SoP).

A GADT is a sum of products declared using the `data` keyword, but it doesn't employ the `|` symbol; instead each data ctor introduces a variant. Also, the signatures of data ctors are stated explicitly.

The `data` keyword, used for a product type, actually constructs a tuple (which is the canonical product) of types.

```hs
-- as ADT
data Product a b = P a b

-- as the equivalent newtype
newtype Product a b = P (a, b)

-- as the equivalent GADT
data Product a b where
  P :: a -> b -> P a b

-- as an isomorphic GADT
data (,) a b where
  P :: a -> b -> (,) a b
```




## Writer

```hs
data Writer w a = Writer a w
```

This is just another form for a pair `(a,w)`. Actually it would be more efficient to declare it as such using `newtype` instead of `data`.

```hs
newtype Writer w a = Writer (a, w)
```




## State

State s a = State (s -> (a,s))



---

https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/

https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types

https://serokell.io/blog/algebraic-data-types-in-haskell
