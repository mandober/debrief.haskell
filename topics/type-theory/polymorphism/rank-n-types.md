# Rank-N Types

The `-XRankNTypes` is best thought of as making polymorphism first-class. It allows us to introduce polymorphism anywhere a type is allowed, rather than only on top-level bindings.

In general, *type inference is **undecidable** in the presence of higher-rank polymorphism* (theoretically, it is possible to infer types for rank-2 polymorphism, but at this time GHC does not support it).

A **rank** of a function is the "depth" of its polymorphism. A function that has no polymorphic parameters is rank 0. Almost all familiar polymorphic functions are rank 1.

## Continuation Monad

The types `a` and `forall r. (a -> r) -> r` are isomorphic as witnessed by the following pair of functions:

```hs
cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = let callback = id in f callback
```

Intuitively, this says that having a value is just as good as having a function that will give that value to a callback.

Since isomorphisms are transitive: `Identity a ≅ a ≅ forall r. (a -> r) -> r`,and since `Identity a` is a monad, and isomorphisms preserve typeclasses, we expect that CPS is also a monad.


## Relation between Rank-N types and existential types

In order to unpack an existential type, you need a polymorphic function that works on any type that could be stored in the existential. This leads to a natural relation between higher-rank types and existentials, and an encoding of existentials in terms of higher rank types in CPS.

In general, you can replace

```hs
data T a₁ … aᵢ =
  forall t₁ … tⱼ.
  (Constraints) =>
  Ctor e₁ … eₖ
-- where e₁ … eₖ are types in terms of a₁ … aᵢ and t₁ … tⱼ

-- apply ctor:
Ctor exp₁ … expₖ
-- case ctor:
case e of
  Ctor pat₁ … patₖ -> res
```

with

```hs
data T a₁ … aᵢ =
  Ctor
    (forall b.
      (forall t₁ … tⱼ.
      (Constraints) => 
      e₁ -> e₂ -> … -> eₖ -> b) -> b
    )

-- apply
Ctor (\f -> f exp₁ … expₖ)
-- pattern match
case e of
  Ctor f -> let k pat₁ … patₖ = res
            in  f k
```

```hs
-- So, we can replace this
data T a b = forall x y z. (Show ?) => Ctor (a x) (b y) z
--                                          e₁    e₂    e₃
-- with
data T a b = Ctor
    (forall b x y z.         (Show ?) => a x -> b y -> e₃ -> b) -> b))
--  (forall b. forall x y z. (Show ?) => e₁  -> e₂  -> e₃ -> b) -> b))
```
