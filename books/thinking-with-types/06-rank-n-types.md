# Rank-N Types

-XRankNTypes

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
