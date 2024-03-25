# Type-level programming :: Value-level Peano-style natural numbers

The Peano-style unary encoding of the natural numbers is not a very efficient representation at the value level, although it has its uses. For example, the naturals may be used as the result type of the `lenght` function on lists (instead of the `Int`), which would encourage laziness with infinite lists. Also, the `Nat` type of arguments would allow a more classical pattern that peels off a ctor in the left-hand side match, as `S n` (instead of using e.g. `n - 1` on the right-hand side).

In Agda (which is written in Haskell and relies on many GHC extensions), once we define the Peano style naturals (and attach the appropriate BUILTIN pragma), we can write the natural numbers both in symbolic (e.g. `S Z`) and the usual Hindu-Araic form.


This works great in Agda where the naturals can be writen in their native form (e.g. in lhs patterns), along with writing them as the usual Hindu-Araic numerals - any which way they are written, the compiler converts them into a more efficient representation behind the scenes (into machine-type integers).

Anywaym this value-level definition is what is gonna be lifted to the TL later, so the naturals are defined this way as it is the representation of the naturals that is the most useful at the type level.

So we define the naturals by translating the first two Peano axioms:

```hs
                      n :: Nat
--------- ZERO      ----------- SUCC
Z :: Nat            S n :: Nat
```

that is,
- (ZERO) the first natural, denoted by the symbol `Z`, is a natural number
- (SUCC) if `n` is a natural then `S n` is a natural.

We can easily translate these two axioms into a Haskell ADT, but a GADT will show the types and the resemblence more clearly:

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

The data ctor `Z` repr the first natural number, i.e. zero. The data ctor `S` repr the successor function that takes an existing natural and produces a new one. These two ctors are a direct translations of the first two Peano axioms:
1. Z ∈ Nat
2. n ∈ Nat => S n ∈ Nat

With this we get the naturals at the value level, with some of the terms being: `Z`, `S Z`, `S (S Z)`, `S (S (S Z))`, …

We need to define functions for manipulating the naturals: arithemtic ops, relational ops, and other functions common on numberic types. The form of these functions will be later mimicked at the TL when, using type families, we define functions on TL naturals that manipulate them.

As shown in the standalone kind signature, the `Nat` type has the kind `Type` which classifies inhabited types exclusively.

The `Nat` declaration gets us so far
- a new data type called `Nat`
- nullary type ctor `Nat` of kind `Type`
- nullary data ctor `Z` of type `Nat`
- unary data ctor `S` of type `Nat -> Nat`
