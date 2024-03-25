# Type-level programming :: Type-level naturals

## Natural numbers


Besides the regular, parameterized data types, having the natural numbers at the type level would allows us to define **type-indexed data types**.

The natural numbers are ideal as an indexing type as they have an easily managable infinite sequence of type-level terms: `Z`, `S Z`, `S (S Z)`, …, **each of which *is* a distinct type**.

Moreover, they can be easily manupulated even at the type-level: we immediately have available the `S` type function for de/incrementing numbers, and we can define more functions on type-level naturals using type families.

## Peano-style natural numbers

It is easy to define the natural number type using the Peano-style unary representation:

```hs
-- as ADT
type Nat :: Type
data Nat = Z | S Nat

-- as GADT
type Nat :: Type
data Nat where
  Z :: Nat
  S :: Nat -> Nat
```

* Note: The `Type` kind classifies inhabited types. Although seemingly useless, and ineed useless at *value level (VL)*, uninhabited types can be very useful at *type level (TL)*.

The Nat declaration gets us
- a new data type called `Nat`
- nullary type ctor `Nat` of kind `Type`
- nullary data ctor `Z`
- unary data ctor `S`

The data ctor `Z` repr the first natural number, i.e. zero. The data ctor `S` repr the successor function that takes an existing natural and produces a new one. These two ctors are a direct translations of the first two Peano axioms:
1. Z ∈ Nat
2. n ∈ Nat => S n ∈ Nat

```hs
                      n :: Nat
--------- ZERO      ----------- SUCC
Z :: Nat            S n :: Nat
```

that is
- (1) zero, denoted by the symbol `Z`, is a natural number.
- (2) if `n` is a nat then `S n` is a nat.





* Note: The data ctor `S` can be seen as being parameterized by any type `a` - as if its type was `S :: a -> Nat` - but then `a` is restricted just to the type of `Nat`, resulting in its actual type `S :: Nat -> Nat`.

* On a related note: In the legacy definition of TL nats, `S` actually has the type `S :: a -> Nat`, which is bad since it allows construction of nonsense types like `S Bool`.
