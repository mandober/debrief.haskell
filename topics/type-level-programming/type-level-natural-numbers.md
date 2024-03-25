# Type-level natural numbers

natural numbers
- value-level natural numbers, `Nat`
- type-level natural numbers, promoted `Nat`
  - kind `Nat` with 2 uninhabited type ctors, `'Z` and `'S`
  - `'Z :: Nat`
  - `'S :: Nat -> Nat`
- singletons for each natural number for explicit passing
- class that generates singletons for each natural for implicit passing
- conversion between value and type level naturals
  - reification: from type to value
    - Reify class
  - reflection: from value to type


## Code

```hs
-- | Value-level nats are also promoted to the type level nats.
data Nat = Z | S Nat

-- | Value-level SNat generates singletons - runtime witnesses for each nat.
data SNat (n :: Nat) where
  SZ ::           SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | The class NatI generates singletons, SNat, to be implicitly passed.
-- It defines the runtime witness of the static nat as a singleton GADT.
-- Its single method natI generates singletons for naturals.
class NatI (n :: Nat) where
  natI :: SNat n

-- | NatI generates the singleton SZ for the type-level nat Z.
instance NatI 'Z where
  natI :: SNat 'Z
  natI = SZ

-- | NatI generates the singleton `SS natI` for each type-level nat
-- provided `NatI n` then `NatI ('S n)`
instance NatI n => NatI ('S n) where
  natI :: SNat ('S n)
  natI = SS natI
```


## Value-level natural numbers

We can define the natural numbers at the value level using Peano axioms.

```hs
{-# LANGUAGE DataKinds #-}

data Nat where
  Z :: Nat
  S :: Nat -> Nat
```

In a dependent language this single definition would make the naturals available at both value and type level.

Value-level natural numbers have their use, but here we're tolerating them just in order to promote them to the type level.



## Type-level natural numbers

Using `DataKinds` extension, we also get the type-level naturals.

The type-level naturals allows to define *type-indexed data types*, like vector, `Vec n a`, which is indexed by the type-level naturals, `(n :: Nat)`, and *parmeterized* by any type, `(a :: Type)`.

```hs
data Vec (n :: Nat) (a :: Type) where
  VNil  ::                 Vec  'Z    a
  VCons :: a -> Vec n a -> Vec ('S n) a
```

The `Vec` type depends on the value `n` (its length) and data types that depend on values are called *dependent types*.

Dependently-typed programming languages such as Agda and Idris provide support for dependent types, but Haskell wasn't immediately imagined as a dependently-typed language. Now that such features became desirable retrofitting dependent types into Haskell's existing type system is not an easy task. The bigest issue with dependent typing is that type inference becomes undecidably.

Another issue with with type-level naturals, and with any type for that matter, is that types get erased past the compilation phase, so they are not available at runtime. Dependently-typed languages have a slew of mechanisms to decide which types are retained and which are erased, but Haskell, for now at least, erases them all. To overcome this problem we need to provide a value-level witness for each type-level natural number, and that's where singletons come into play.

Singletons have the property that each value corresponds to exactly one type, and vice versa; i.e. they have a bijective relation between types and values.

GADTs allow us to define all the singletons at once, where each singleton corresponds to a particular natural number.

For types of the kind Nat can be ocurred only as a parameter of other type, we have to define some data-type carrying Nat type as its parameter and the structure of its data constructors should reflect the one of corresponding type-level natural. With GADTs, we can define such a data-type:

```hs
data SNat (n :: Nat) where
  SZ ::           SNat 'Z
  SS :: SNat n -> SNat ('S n)
```

The `Fin` GADT is ideal for indexing vectors. The `Fin` is a finite set that contains a list of numbers:
- `Fin Z` contains the empty list, []
- `Fin (S Z)` contains [0]
- `Fin (S (S Z))` contains [0..1]
- `Fin (S (S (S Z)))` contains [0..2]
- in general, `Fin n` contains [0..n-1]

```hs
data Fin (n :: Nat) where
  FZ ::          Fin ('S n)
  FS :: Fin n -> Fin ('S n)
```

So a `Vec n a` contains `n` items, so in order to index it in a type safe way, we can only use indices from 0 to `n - 1`, which is exactly what `Fin` provides.

Now we need a way to connect the `n` in a `Vec n a`, and the `n` in `Fin n`, with the `n` in `SNat n`.
