# Algebraic Data Types

KEYWORDS
- Algebraic data type
- Cardinality
- Isomorphism
- Algebra of types
- Canonical representation
- Sum type
- Product type
- Function (exponential) type
- The Curry-Howard Isomorphism
- propositions as types

SECTIONS
- Algebraic data types
- Cardinality of types and isomorphisms between types
- Sum, product and exponential types



## Cardinality of types and isomorphisms between types

Any two types that have the same **cardinality** will always be isomorphic to one another. An **isomorphism** between two types is defined by a pair of functions to convert between them, such that composing either after the other gets you back where you started (id). In such case, the two types are isomorphic, which can be denoted by `T ≅ S`.

```hs
to   :: S -> T
from :: T -> S

to . from = id -- idᴛ
from . to = id -- ids
```

If two types have the same cardinality, any one-to-one mapping between their elements is exactly these `to` and `from` functions. Such a mapping can come from anywhere. It doesn't matter - just pick an arbitrary ordering on each type (not necessarily corresponding to an `Ord` instance) and then map the first element under one ordering to the first element under the other.

> In general, for any two types with cardinality `n`, there are `n!` unique isomorphisms between them.

Any one of these isomorphisms is good enough. In fact, for most purposes, just knowing that an isomorphism exists is good enough. An isomorphism between types `S` and `T` is a proof that for all intents and purposes, `S` and `T` are the same thing. They might have different instances available, but this is more a statement about Haskell's typeclass machinery than about their equivalence.

## Sum, product and exponential types

The inintuition behind addition generalizes to any datatype with multiple data ctors, i.e. the cardinality of a type is always the sum of the cardinalities of its data ctors.

Void
- any empty data type (just type ctor without data ctors) acts like 0
- the `Void` is the canonical 0, a monoidal identity for sum types
- `Either Void a` ≅ `a` is like `0 + a = a`

Unit
- any nullary data ctor acts like 1
- the `()` is the canonical 1, a monoidal identity for product types
- `(a, ())` ≅ `a` is akin to `a * 1 = a`

Ops
- (+): Either is the canonical repr of sum types
       `Either a b` ~ `a + b`
- (*): a pair is the canonical repr of product types
       `(a, b)` ~ `a * b`
- (^): function type ctor is the canonical repr of exponential types
       `a -> b` ~ `bᵃ`


An interesting consequence of this is that we can express math truths in terms of types. For example, we can prove that `a × 1 = a` by showing that an isomorphism between `(a, ())` and `a` exists.

```
Void                  0
()                    1

Either                +
(,)                   *
->                    ^

(a, ()) ≅ a           a * 1 = a
Either Void a ≅ a     a + 0 = a
```

- *subtraction* corresponds to types with particular values removed
- *division* of a type makes some of its values equal (in the sense of being defined equally—rather than having an `Eq` instance which equates them)
- *differentiation* from calculus also has meaning in the domain of types; refer to Conor McBride's `The derivative of a regular type is its type of one-hole contexts`.


Consider the theorem `a¹ = a`. When viewed through the *Curry–Howard correspondence*, it describes `() -> a` ≅ `a`, so this theorem shows that there is no distinction between having a value and having a pure program that computes that value.

## Canonical Representations

A direct corollary that any two types with the same cardinality are isomorphic, is that there are multiple ways to represent any given type.

Since it is useful to have a conventional form when working with types generically, the canonical type representation is known as a, **possibly recursive, sum of products**.

Beside making an exception for numeric types (`Int` would be too much work to express as sum), all ADTs have their **canonical representation**, which is a form they should be in when using generics.

Canonical representation (CR) and algebraic equivalents

- ()            ~ 1
- (a, b)        ~ a * b
- Either a b    ~ a + b
- a -> b        ~ bᵃ
- (a, Int)

- Either (a, b) (c, d)          ~ ab + cd
- Either a (Either b (c, d))    ~ a + b + cd

- a -> b -> c ≅ (a, b) -> c     ~ (cᵇ)ᵃ = (cᵃ)ᵇ = c^(a*b) = cᵃ + cᵇ

- Either (a, ()) ((), d)          ~ ab + cd

- Either (a b) (a c)            ~ ab + ac

(a, Either (b, c))

a(b+c) = ab+ac its CR is `Either (a b) (a c)`


these are not:
- (a, Bool) is like a*(1+1)=a+a=2a so its CR is `Either a a`
- Maybe a is 1+a ≅ Either a () is a+1

(this doesn't mean you should prefer using `Either a ()` over `Maybe a`)
