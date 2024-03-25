# Vector

https://www.youtube.com/watch?v=jPZciAJ0oaw&list=TLPQMTQxMjIwMjJaY7-OIafG4w&index=30

```hs
data Vec (n :: Nat) (a :: Type) where
  Nil  ::                 Vec  Z    a
  (:>) :: a -> Vec n a -> Vec (S n) a
```

## Filter

```hs
filter :: (a -> Bool) -> Vec n a -> Vec m a
filter _ Nil = Nil             -- (1)
filter p (x :> xs)             -- (2)
  | p x  = x :> filter p xs
  | otherwise = filter p xs
```

Writen naively as above, the `filter` errors with
- Could not deduce ((m :: Nat) ~ ('Z :: Nat))
- from the context: (n :: Nat) ~ ('Z :: Nat)

in (1), while in (2) the error is
- Could not deduce ((m :: Nat) ~ ('S n0 :: Nat))
- from the context: (n :: Nat) ~ ('S n1 :: Nat)

It seems as if `n` and `m` (type vars of kind `Nat`) are intended to signify two distinct natural (for two different vec lengths), but the way all of this stuff works is that `n` and `m` are both chosen by the caller. We can think of
`n` and `m` (and `a` for that matter) as being the extra arguments to the `filter` function. Every time we call `filter`, we have to choose what is `a`, `n`, and `m`. Most of the time, GHC infers this for us.

However, if we think about these type params as input type args (a la System F), then there's nothing that says that `m` in the return type is going to be zero just because the input is zero, i.e. because the input vec is empty; there is nothing tying these assumptions together.

On the other hand, we do know that `n` is zero if the input vec (1) is empty, i.e. if it is `Nil :: Vec Z a`. When we pattern matched on `Nil` in (1), we know that `n` has to be zero (i.e. `Z`), but we don't know anything about `m`.

This type signature isn't gonna work for this - what we need instead are existential types. We want to say that there exists some `m` such that the result type is `Vec m a`, or, in faux syntax, `∃ (m : Nat). Vec m a`, instead of the usual `∀ (m : Nat). Vec m a`. Alas, DependentHaskell is still far away, so to solve this problem, we have to define a separate data type just to deal with the existential quantification of vector's index parameters.

```hs
-- | Serves to wrap a regular vec and make it forget its length.
data EVec a = forall (n :: Nat). EVec (Vec n a)

-- | or, as equally useless GADT
data EVec a where
  MkEVec :: Vec n a -> EVec a

deriving stock instance (Show a) => Show (EVec a)

filter :: (a -> Bool) -> Vec n a -> EVec a
filter _ Nil = EVec Nil
filter p (x :> xs)
  | p x = case filter p xs of EVec ys -> EVec (x :> ys)
    -- jerking around instead of: p x = x :> filter p xs
  | otherwise = filter p xs


-- Now we can't do shit after filtering:
vx = v3 ++ filter (< 12) v5    -- ERROR
x1 = length (filter (< 12) v5) -- ERROR
-- and any other fn that works on 'Vec Nat a' of course fails on 'EVec a'.
-- What a fantastic fuckup. Alas poor Yorick and the effort invested to create indexed lists in the first place. What a waster, you pissed it all up the wall.
-- There's tears coming out from everywhere. The city's hard, the city's fair.
-- Go back inside, You got nothing on, Mind yer bleedin' own, You two bob cunt.
```

To add to the pain, this types also changes the strictness of this `filter` function, which is normally lazy. The regular `filter` on lists produces output as soon as it consumes some input, and we also have optimizations like the list fusion. But this unfortunate filter has to evaluate the whole output before it starts consuming any of input. Best throw it away, it's completely unusable. Waiting for DependentHaskell instead is time better spent.

Actually, we can improve the `EVec` type still today because we have some more info about the output vector's length - it cannot be greater than the input vector's length. Instead of just quantifying `EVac` over a type, we'd like to also quantify it over a constraint on the length.

https://youtu.be/8AKtqFY1ueM?list=TLPQMTQxMjIwMjJaY7-OIafG4w&t=502

```hs
type EVec :: (Nat -> Constraint) -> Type -> Type
data EVec c a where
  EVec :: c n => Vec n a -> EVec c a
```

We specify a constraint (essentially, a class constraint) that takes a `Nat`, and this allows us to say that the output length is going to be less than some number. Whatever `n` (representing length) is, it has to obey the constraint, we denote with `c`. The constraint is that `c` applied to `n` must hold, i.e. the `(c n) =>` context (needs the `ConstraintKinds` extrension).

To appease the type checker, for now we just introduce a vacuuous, arbitrary constraint that is always satisfied through a class definition.

```hs
type WeKnowNothing :: Nat -> Constraint
class WeKnowNothing n
instance WeKnowNothing n

filter :: (a -> Bool) -> Vec n a -> EVec WeKnowNothing a
filter _ Nil = EVec Nil
filter p (x :> xs)
  | p x  = case filter p xs of EVec ys -> EVec (x :> ys)
  | otherwise = filter p xs
```

### Defining a class to use as a constraint

```hs
type (>=) :: Nat -> Nat -> Constraint
class       m >= n
instance    Z >= n
instance {-# OVERLAPPABLE #-} (S m >= n) -- what a mess
instance (S m >= S n)

filter :: (a -> Bool) -> Vec n a -> EVec ((>=) n) a
filter _ Nil = EVec Nil
filter p (x :> xs)
  | p x       = case filter p xs of  EVec ys -> EVec (x :> ys)
  | otherwise = case filter p xs of  EVec ys -> EVec       ys
```
