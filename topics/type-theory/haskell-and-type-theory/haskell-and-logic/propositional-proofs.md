# Propositional proofs

Writing (structural) inductive proofs to let GHC know that two types are equal.

## Weak propositional equality

We can do proofs using propositional equality type, `(Data.Type.Equality.:~:)`.The operator `:~:` is used to show *the weak propositional equality of two types*; "weak" because it disregards their kinds.

```hs
import Data.Type.Equality

type (:~:) :: forall k. k -> k -> Type
data a :~: b where
  Refl :: forall k (a :: k). a :~: a
```

This type declaration says that if `a` is propositionally equal to `b`, denoted by `a :~: b`, and this type is inhabited by some terminating value, then the type `a` is the same as the type `b`.

To use the equality in practice, do the pattern-match on `a :~: b` to get the `Refl` data ctor out; then, in the body of the pattern-match, the GHC will know that `a ~ b` (where `~` denotes equality between two types, sometimes also referred as unification, as in "`a` unifies with `b`" aka, they are the same type).


The type `:~:` also implements these instances of the standard classes:

```hs
instance forall k (a :: k) (b :: k).            Eq      (a :~: b)
instance forall k (a :: k) (b :: k).            Ord     (a :~: b)
instance forall k (a :: k) (b :: k).            Show    (a :~: b)
instance forall k (a :: k) (b :: k). (a ~ b) => Read    (a :~: b)
instance forall k (a :: k) (b :: k). (a ~ b) => Enum    (a :~: b)
instance forall k (a :: k) (b :: k). (a ~ b) => Bounded (a :~: b)
```

These instances are auto-derived.

---

The propositional equality allows us to prove to GHC that a theorem holds by showing that two types are equal. For example, if we wanted to prove to GHC that `m + Z = m` follows from `Z + m = m` (part of the proof of the commutativity of addition over natural numbers), then we'd create a function whose type reflects that property.

We have a type family that does `Nat` addition and it has two equations: the first one that handles the base case and the second one that deals with the recursive case (we stick with the first one only here for simplicity). So, the first one deals with the base case saying that `Z + m = m`. The problem is that GHC cannot derive from this that `m + Z = m`, so, if we have some type function that uses the params in the wrong order, it blows up.


```hs
type family (n :: Nat) + (m :: Nat) where
    Z + m = m  -- given base case

--  m + Z = m  -- we need to prove the commutativity
-- which this function below does! somehow...

mPlusZero :: forall m. SNat m -> (m + Z) :~: m
mPlusZero SZ = Refl
mPlusZero (SS n) = case mPlusZero n of Refl -> Refl
```




## Strong propositional equality

Strong propositional equality does not disregard the kinds of the type involved. It is denoted by `:~~:` and it lives in the same module as `:~:`, i.e. in `Data.Type.Equality`:

```hs
type (:~~:) :: forall k1 k2. k1 -> k2 -> Type
data a :~~: b where
  HRefl :: forall k1 (a :: k1). a :~~: a
```



## Uses with Vec

The need for propositional equality arises when dealing with static-length vector types. For example, the impl of the `reverse` function for vectors, if done naiviely, runs in O(n²). To bring this down to a more reasonable O(n), we need a helper function with an accumulator. However, because the `Vec` type is declared using GADTs, GHC requires the signatures, not only on the parent function, but on the helper function as well. And it is the nested function's signature that will be the end of us, my love.



Related to the "association" between certain TF and functions on vectors. That is, related to the necessary mirroring of their structures.

For example, `NatAdd` TF has the equation `Z + m = m`, but the function using it, to type-encode a Vec's length (which is a Nat) fails because GHC cannot extrapolate commutativity of addition from this equation alone; instead, it needs to be shown a proof of it: `m + Z = m`.

And just adding this equation to the TF `NAtAdd` screws things up coz the strucutures ceise to be mirrors of eachother, they ceise to be almost exactly the same. On the other hand, adding the similar equation to both TF and the function is not always feasable for the function (TF would work fine on its own, though).

There then, the TF has the equation `Z + m = m`, so we need to prove that this still holds with the params swapped: `m + Z = m`


*           Z + m = m  ≡  m + Z = m                 (1)
* S n + m = S (n + m)  ≡  n + S m = S (n + m)       (2)


```hs
type (+) :: Nat -> Nat -> Nat
type family n + m where
  Z     + m = m             (1)
  (S n) + m = S (n + m)     (2)


-- THE PROOF
mPlusZero :: forall m. SNat m -> (m + Z) :~: m
mPlusZero SZ = Refl
mPlusZero (SS n) = case mPlusZero n of Refl -> Refl
```
