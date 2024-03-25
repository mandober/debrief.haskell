# Singleton type

The type `()` (unit) is a singleton type because it is inhabited by a single value, `()`. In fact, `()` is actually the sole data ctor of the unit type, so the data ctor itself is the value.

We are here interested in the bijection between a type and value, like the unit type has. Actually, we are mostly concerned with the injection part of this bijective relation, which takes a type to a value - which only the singleton types posses. Whenever we have a value of a singleton type, we immediately know infer its type, and vice versa (the reverse relation is not so interesting because there are literal values for which we can infer the type easily, e.g. 'c' is surely a `Char`).

> A type that only ever has a single value always upholds this bijection.

Any two singleton types are isomorphic to each other.

Inpired by this unique property of the unit type, we can model more such types after it. We have term level natural numbers, `Z`, `S Z`, `S (S Z)`, …, all of the type `Nat`. Using the `DataKinds` extension, we can promote the data ctors `Z` and `S` into the *uninhabited* type ctors `'Z` and `'S` of the kind `Nat`; the type ctor `Nat` is promoted into a new kind, `Nat`, to which the uninhabited type ctors `'Z` and `'S` belong. This was possible before the introduction of the `DataKinds`, but it was far less useful - we'd have two completely disjoint type ctors `Zero` and `Succ`, each declared as the empty data type, `data Zero` and `data Succ`. Now, however, the two type ctors are classified by the same kind, `Nat`.

```hs
data Nat where
  Z :: Nat
  S :: Nat -> Nat

-- values of type Nat
n0 = Z :: Nat
n1 = S Z :: Nat
n2 = S (S Z) :: Nat

{-# LANGUAGE DataKinds #-}

-- Now 'Z is 
>>> :kind 'Z
  Nat

>>> :kind 'S
  Nat

>>> kind! S (S (S Z))
  S (S (S Z)) :: Nat
  = 'S ('S ('S 'Z))
```

What we'd like at this point is to have a singleton type for each natural number. So far, we have all the naturals numbers as values, but they all have the same one type `Nat`. The promotion of the two data ctors, `Z` to `'Z`, and `S` to `'S`, gives us two uninhabited type ctors of the same kind, `Nat`. They allow us to use naturals at the type level, so we can talk about e.g. vectors which are list that carry their length, i.e. `Vec Nat a` is a like a plain old list but augented with the `Nat` type that tracks its length.

```hs
data Vec (n :: Nat) a where
  Nil  :: forall a. Vec Z a
  (:>) :: forall a (n :: Nat). a -> Vec n a -> Vec (S n) a
```

However, the information about the length is tracked only at the type level, so it gets erased before the run time. Still, it is very useful at the compile time since it allow us to define total functions over the list which are otherwise partial, like `head` and `tail`.

```hs
head :: Vec (S n) a -> a
head (x :> _) = x

tail :: Vec (S n) a -> Vec n a
tail (_ :> xs) = xs
```

Now the `head` function is total because it only accepts a nonempty list, that is, the length of the list must be the successor of some natural number `n`. The type is not just `Vec n a`, for any natural `n`, but `Vec (S n) a`, which ensures the list cannot be empty. The `tail` function also benefits from the type-level naturals because we can now say that the output list's length is one less then the input list's length.

The problem is now the type erasure, which erases this info about the length, so we cannot refer to it. For example, we still have to define the `length` function for vectors as we'd do for lists. This is especially unfortunate if we want to return the length as the term level natural number, since it looks as though we already have that information in the vector's type.

```hs
-- we can't just do this:
len :: Vec n a -> Nat
len _ = n
-- not only coz we're trying to refer to a type var at the value level
-- but also becasue that type is erased by the time we try to refer it.

-- we must do it the old way
length :: Vec n a -> Nat
length Nil = Z
length (x :> xs) = S (length xs)
```

In `len` above, `n` is a type variable of kind `Nat`; it represents exactly the natural number we'd like to get a hold of at the value level. However, we have no means to establish such a connection, from the type level to the value level.

This is where the singleton types come in. We'd like to establish a connection between the type-level and value-level natural numbers, so we'd like that each natural number has its own type. So, that zero is the value `Z` of the type `Z`, one is the value `S Z` of the type `S Z`, etc.

```hs
-- taking the unit as the ispiration
data () = ()

-- we'd like to achieve this bijection between the type and value level but without defining each singleton natural as a standalone type, i.e.
data      Z  =      Z
data    S Z  =    S Z  -- the natural 1 as the type (S Z) and the value (S Z)
data S (S Z) = S (S Z)
-- ...
```

So far we have naturals at the value level, and naturals at the type level, but no connection between them. We'd like to link them somehow, so that the value of a natural number represented at the type-level is the same as that of its term-level representation. We'd also like to be able to refer to a type level natural at the term level.

This would be possible if we had a singleton type for every natural, but we can't define each natural as a standalone singleton type as that would take forever (as a countable infinity).

To the same in less time, we define the data type `SNat n`, where `n` is type variable of the kind `Nat`.

```hs
data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
```


The data ctor `Sz` is exactly the same singleton we'd get if we had define it as a standalone type. But the data ctor `Ss` allows us to define more naturals by applying it some number of times to `Sz`.

```hs
n2 :: SNat ('S ('S 'Z))
n2 = SS (SS SZ)
```

Given a type, now there's only one valid value of that type. And vice versa, given a value, there's only one type it could possibly have. This allows GHC to fit value holes (possibly in a few refinement steps).

```hs
-- Inferring a value given only a (singleton) type:

x :: SNat ('S ('S 'Z))
x = _
-- Found hole: _ :: SNat ('S ('S 'Z))
-- Valid refinement hole fits include: SS (_ :: SNat ('S 'Z))

-- so we replace the hole with that, obtaining another hole:
x = SS (_ :: SNat ('S 'Z))
-- Found hole: _ :: SNat ('S 'Z)
-- Valid refinement hole fits include: SS (_ :: SNat 'Z)

-- we fit the hole again, obtaining another hole:
x = SS ((SS (_ :: SNat 'Z)) :: SNat ('S 'Z))
-- Found hole: _ :: SNat 'Z
-- Valid hole fits include: SZ :: SNat 'Z

-- we fit the hole again and we end up getting the final value:
x = SS ((SS ((SZ :: SNat 'Z) :: SNat 'Z)) :: SNat ('S 'Z))
```

We now have two representations of naturals at the value level:
- `Z`, `S Z`, `S (S Z)`, …
- `SZ`, `SS SZ`, `SS (SS SZ)`, …

However, while the former values all have the same type, `Nat`, each of the latter values has a unique corresponding type that has the same structure:

```hs
SZ :: SNat 'Z
SS SZ :: SNat ('S 'Z)
SS (SS SZ) :: SNat ('S ('S 'Z))
...
```

These types are made possible by having a type-level naturals. In turn, they are made possible by promoting the `Nat` type and its two data ctors, `Z` and `S`, to obtain two new unihabited type ctors, `'Z` and `'S`, and the new kind, `Nat`, that classifies them.
