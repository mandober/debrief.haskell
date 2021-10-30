# Heterogeneous Lists

One of the primary motivations of GADTs is building *inductive type-level structures out of term-level data*.

We can use GADTs to define a heterogeneous list.

```hs
-- Necessary Extensions
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Necessary Imports
import Data.Kind (Constraint, Type)

-- Heterogeneous Lists
data HList (ts :: [Type]) where                 -- 1
    HNil :: HList '[]                           -- 2
    (:#) :: t -> HList ts -> HList (t ': ts)    -- 3

infixr 5 :#
```

At (1), we've given HList's `ts` an explicit kind signature. The type parameter `ts` is defined to have the kind `[Type]`, because we'll store the contained types inside it. This kind signature isn't necessary (GHC can infer it), it is there for programer's benefit. A good rule is to annotate every kind but `Type`.

HList is analogous to the regular list:
- empty list ctor is `HNil`
- cons ctor is `:#` (symbollically named ctors must start with a colon)
- their types are explicitlly stated thanx to GADTs

`HNil` represents an empty HList. It takes nothing and returns `ts ∼ '[]`, i.e. an empty list of types.

`:#` takes 2 args, the first of type `t` and the second `HList ts`. It returns a `HList (t ': ts)`, i.e. the result is this new type consed onto the HList.



But how do I turn HList into its type equality representation? I've got this far, it even doesn't error immediately, but querying about Hons type, does:
```hs
data Hist ts = (ts ~ '[]) => Hil
             | forall t. (ts ~ (t ': ts), Show t) => Hons t (Hist ts)
>:t Hons
-- ERROR: Occurs check: cannot construct the infinite type: ts ~ t : ts
--        arising from a use of Hons
```


HList can be pattern matched just like a regular lists. For example, we can implement a `length` function:

```hs
hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts
```

Having this explicit list of types to work with allows us to implement total functions; e.g. we can implement `head` as a total function:

```hs
hHead :: HList (t ': ts) -> t
hHead (t :# _) = t
```

So, when taking the `Hhead HNil`, we don't get an exception like we do with `head []`, but a type mismatch.

We can also deconstruct any length-3 HList whose second element is a Bool, show it (but we have to write the Show class instances first).

```hs
showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b
```

### HList class instances

Unfortunately, GHC's stock deriving machinery doesn't play nicely with GADTs, it will refuse to write `Eq`, `Show` or other instances. We can write our own instances though, by providing an instance for the base case and an instances for the inductive case (we need 2 separate instances).


#### HList 1 class 2 instances
The base case is that two empty HLists are always equal. The recurring case is that two cons HLists are equal iff their heads and tails are equal.

```hs
instance Eq (HList a) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs
```

#### HList 1 class 1 instances
However, I've realized that we can also do it by writing one instance (instead of two separate ones, one for each case), but then we have to modify the data declaration, placing appropriate class constraints in there. If we have access to the data declaration, this might be preferably in order to avoid shorten the types of instances we plan to define.

```hs
data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: (Eq t, Show t) => t -> HList ts -> HList (t ': ts)
--          ^^^^^^^^^^^^^^^^^
--             constraints

-- when constraints are placed here, we can define 1 instance:
instance Eq (HList a) where
    HNil == HNil = True
    (x :# xs) == (y :# ys) = x == y && xs == ys
```

#### HList 1 class 1 instances as a type family

The reason we had to write two instances for `Eq` was to assert that every element in the list also had an Eq instance. While it worked, it was rather unsatisfactory.

As an alternative, we can write a *closed type family* which will fold `ts` into a big constraint, indicating that each element has an `Eq` instance.

```hs
type family AllEq (ts :: [Type]) :: Constraint where
    AllEq '[] = ()
    AllEq (t ': ts) = (Eq t, AllEq ts)


type family AllOrd (ts :: [Type]) :: Constraint where
    AllOrd '[] = ()
    AllOrd (t ': ts) = (Ord t, AllOrd ts)
```

`AllEq` performs type-level pattern matching on a list of types, determining whether it is empty. If it is a *promoted empty list ctor*, (`'[]`), we simply return the *unit constraint*; because of the kind signature on `AllEq`, Haskell interprets this as `Constraint` rather than the unit type.

However, if `ts` is a *promoted cons list ctor*, (`':`), we build a *constraint tuple*. Notice that AllEq is defined inductively, so it will eventually find an empty list and terminate. By using the `:kind!` command in GHCi, we can see what this type family expands to.

> :kind! AllOrd '[Int,Bool]
AllOrd '[Int,Bool] :: Constraint
= (Ord Int, (Ord Bool, () :: Constraint))

AllEq has successfully fold `[Type]`s into a `Constraint`. But there is nothing specific to `Eq` about `AllEq`. Instead, it can be generalized into a fold over any `Constraint c`. We need *ConstraintKinds* in order to talk about **polymorphic constraints**.


```hs
{-# LANGUAGE ConstraintKinds #-}

type family All (c  ::  Type ->   Constraint)
                (ts :: [Type]) :: Constraint
    where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)
```

With `All`, we can now write our (`Eq` and `Ord`) instances directly.

```hs
instance All Eq ts => Eq (HList ts) where
    HNil == HNil = True
    (a :# as) == (b :# bs) = a == b && as == bs

instance All Ord ts => Ord (HList ts) where
    -- (<=) :: a -> a -> Bool
    HNil <= HNil = True
    (a :# as) <= (b :# bs) = a <= b && as <= bs
```
