# Vectors


## Issues

- *How to lift a term value for length into a type-level number?* and v.v.
- `Vec m Int` are distinct types for each distinct `m` repr length
- Partial type application: therefore it must be `Vec n a` (not `Vec a n`)


## Inductive hypothosis

Inductive hypothosis is the assumption made in the inductive step: assuming that a property `P` holds for an element `n`, show that `P` holds for `n + 1`.

Since the `Vec` type depends on its length, `Vec` class instances will usually have to be split into a base instance that will handle the `Vec Z a` case, and an inductive instance that will handle the inductive hypothesis case. It will have the form: if `Vec n a` is an instance of the class then so is `Vec (S n) a` (maybe without mentioning the type of elements, `a`, depending on the class). Typically, the inductive assumption will be expressed in the context, with its conclusion expressed as the target type (ctor) for the class. For example:

```hs
-- base step
instance Functor (Vec Z) where
-- inductive step
instance Functor (Vec n) => Functor (Vec (S n)) where
```

which states that, if `Vec n` is a Functor, then so is `Vec (S n)`.


## Vector implementations

`Vec` here, is not a proper vector, i.e. a resizable array located on the heap with notions of length, capacity and an expamsion strategy. Static-length vector is an enhanced list that can carry around its length, encoded into its type, such that breaking this invariant generates a much more favorable compile time (as opposed to run time) error.

```hs
data Vec n a
```

Obviously, we need some numeric type to encode the length, and a very convenient (good for learning, bad for performance) type is `Peano` or `Nat`, i.e. natural numbers that are encoded using the basic Peano ctors, `Z` and `S`. They are convenient because they enable easy pattern matching on the LHS, similar to the deprecated n+1 patterns (normally, with, e.g. Int, you can't use pattern matching, so in a recursive function you must use, possibly multiple time, the predecessor on the RHS, instead of using successor just once in a pattern match on the LHS, forgetaboutit).

```hs
data Vec :: ℕ -> * -> * where
  Nil  :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a
```


Like a list, a vector's type depends on the type of its elements, additionally combined with the "type" of their length. So, even if two vectors both hold `Int`s, they are different types if their length is different (in a way similar to tuples).


These are NOT the same TYPE (regardless of the `a` type):

```
    Vec Z a  ≁  Vec (S Z) a  ≁  Vec (S (S n)) a
    └──┬──┘     └────┬────┘     └──────┬───────┘
       │             │                 └ type of vectores with len > 1
       │             └ type of singleton vectores
       └ type of empty vectores
```

As a consequence, pay attention when, e.g., implementing a class - it is not one instance fits all situation. For example,

```hs
instance Unfoldable (Vec Z) where
  unfold _ _ = Nil

instance Unfoldable (Vec n) => Unfoldable (Vec (S n)) where
  unfold f x0 = let (y, x1) = f x0 in Cons y unfold f x1
```


While contemplating the exact type and especially the exact order of type params, we must ackowledge that vectors have a special type param, commonly named `n`, dedicated to encoding a vector's length by relying on some sort of type-level natural number representation. We can define out own `Nat` just for kicks or use the one predefined in the `GHC.TypeLits` module, which also offers misc operations to manipulate type-level `Nat`s.

In any case as to which impl of Nat we use, we should definitely fix the order of Vec's type params to `Vec n a`, in expectation to partially apply the Vec type ctor as `Vec n`. It cannot ever be partially applied only as `Vec` because the `n` type param must always be present. Only `Vec n` carries some meaning, and `Vec` is meaningless (?), at least as a potential target for the usual classes. `Vec n a` is nothing like a seemingly similar type `Either e a`, and so only `Vec n` is a similar type ctor as, e.g. `Maybe`. The unsaturated type ctor `Vec n` that has the appropraite kind `* -> *` will be made a `Functor` et al. instance.

```hs
{-# LANGUAGE DataKinds #-}

import GHC.TypeLits

data Vec (n :: Nat) a where
    Nil  :: Vec 0 a
    Cons :: a -> Vec n a -> Vec (n + 1) a
```
