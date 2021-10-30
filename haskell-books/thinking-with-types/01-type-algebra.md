# Type algebra

## Algebraic Data Types

One of FP killer features is pattern matching made possible by algebraic data types. As their name suggests, there is in fact an algebra behind ADT.

To start with ADT, we associate each type with its cardinality but ignoring bottoms.

```hs
-- 0
data Void
-- 1
data () = ()
-- 2
data Bool = False | True
```

> Any two types that have the same cardinality will always be isomorphic to one another.

An isomorphism between types s and t is defined as a pair of functions, such that composing either after the other gets you back where you started, in which case they are isomorphic, `s ≅ t`.

```hs
to   :: s -> t
from :: t -> s

to . from = id
from . to = id
```

If two types have the same cardinality, any one-to-one mapping between them is exactly the `to` and `from` functions. Such mapping can come from anywhere; we can just pick an arbitrary ordering on each type (not necessarily corresponding to an `Ord` instance) and then map the first element under one ordering to the first element under the other. Rinse and repeat.

For example, this new type `Spin` is isomorphic to `Bool`:

```hs
data Spin = Up | Down

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True = Down

spinToBool1 :: Spin -> Bool
spinToBool1 Up = False
spinToBool1 Down = True

-- there is another isomorphism between Spin and Bool:
boolToSpin2 :: Bool -> Spin
boolToSpin2 False = Down
boolToSpin2 True = Up

spinToBool2 :: Spin -> Bool
spinToBool2 Up = True
spinToBool2 Down = False
```

In general, for any two types with cardinality `n`, there are `n!` unique
isomorphisms between them. As far as the math goes, any of the isomorphisms between them is just as good as any other, and for the most purposes just knowing that an isomorphism indeed exists is enough.

An isomorphism between types `s` and `t` is a proof that for all intents and purposes, `s` and `t` are the same thing. They might have different instances available, but this is more a statement about Haskell's typeclass machinery than it is about the equivalence of `s` and `t`.


## Sum, Product and Exponential Types

In the language of cardinalities, sum types correspond to addition. The canonical example is `Either a b`, which is either an `a` or a `b`. As a result, the cardinality of `Either a b` is |Either a b| = |a|+|b|. This is why they are called *sum* types.

Nullary data ctors have a single value so they are always 1, for example in the case of the `Maybe` type, |Maybe a| = 1 + |a|.

Dual to sum types are the *product* types, with the pair `(a, b)` as the canonicical example. The cardinality of a product is |(a, b)| = |a|×|b|.

An interesting consequence of this is that we can express math truths in the terms of types. For sum types, `Void` acts as the id element, 0. For products the id element is a singleton type, like a unit, repr 1. The aritmetical operations match the type algebra. For example, we can prove that `a×1 = a` by showing an isomorphism between `(a, ())` and `a`. We show that `(a, ()) ≅ a` by coming up with the `to` and `from` functions.

```hs
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a
```

Likewise, `Void` acts as a monoidal unit for sum types. To show it, the trivial statement `a + 0 = a` can be witnessed as an isomorphism between `Either a Void` and `a`.

```hs
sumUnitTo :: Either a Void -> a
sumUnitTo (Left a)  = a
sumUnitTo (Right v) = absurd v

sumUnitFrom :: a -> Either a Void
sumUnitFrom = Left
```

The `absurd` has the type `Void -> a`. It says that given a `Void`, it can return anything; but since values of `Void` type don't exist, it's a lie that cannot be disproven.

The *cardinality of function types corresponds to exponentialization*. A function of type `a -> b` has cardinality `bᵃ`. It is such because for every value of `a` in the domain, we need to give back a `b`; but we can chose any value of `b` for every value of `a`.

```
|a -> b| = |b| × |b| × ... × |b|
           \___________________/
                |a| times
```

### Example

Determine the cardinality of:

```hs
f :: Either Bool (Bool, Maybe Bool) -> Bool
```

The cardinality of `f`: it is a function type `x -> Bool`, so it is 2ˣ. The x is a sum type `Either Bool y`, which is 2 + |y|. The y is a product of `Bool` (2) and `Maybe Bool` (1 + 2), so |y| = 2 * 3 = 6. Then |x| = 2 + 6 = 8. Finally, |f| = 2⁸.


## Curry-Howard Isomorphism

Algebra | Logic | Types
--------|-------|----------
a + b   | a ∨ b | Either a b
a × b   | a ∧ b | (a, b)
bᵃ      | a -> b| a -> b
a = b   | a<=>b | isomorphism
0       | ⊥     | Void
1       | ⊤     | ()


The Curry-Howard isomorphism allows us to analyze mathematical theorems through the lens of FP. Math theorems can be expressed as types. Consider the theorem `a¹ = a`. It describes an isomorphism between `() -> a` and `a`. That is, it shows that there is no distinction between having a value and having a pure function that computes that value.


Use Curry-Howard to prove the exponent laws:

* Exercise 1: `aᵇ * aᶜ = aᵇᐩᶜ`

```hs
-- aᵇ * aᶜ = aᵇᐩᶜ

(b -> a, c -> a) ≡ Either b c -> a
(b -> a) -> (c -> a) -> Either b c -> a

(Either b c -> a) ≡ (b -> a, c -> a)
(Either b c -> a) -> (b -> a) -> (c -> a)
```

* Exercise 2: `(a × b)ᶜ = aᶜ × bᶜ`

```hs
-- (a × b)ᶜ = aᶜ × bᶜ

c -> (a, b) ≡ c -> a, c -> b
```


* Exercise 3: `(aᵇ)ᶜ = aᵇᶜ`


```hs
-- (aᵇ)ᶜ = aᵇᶜ = aᶜᵇ

c -> (b -> a) ≡ (b, c) -> a
c -> b -> a   ≡ (c, b) -> a
c -> b -> a   ≡ c -> b -> a
```



## Canonical Representations

A direct corollary that any two types with the same cardinality are isomorphic, is that there are multiple ways to represent any given type.

The canonical representation of types is a **sum of products**.

We also make the stipulation that all additions are to be repr by `Either` and multiplications by a pair.

Each of following types is in its canonical representation:
- ()
- Either a b
- Either (a, b) (c, d)
- Either a (Either b (c, d))
- a -> b
- (a, b)
- (a, Int)

We make an exception to the rule for numeric types (like `Int` in the last case) as it would be too much work to express them as sums.

The canonical repr of `Maybe a` is `Either () a`.
