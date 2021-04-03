# Existential Type

Closely related to rank-n types are the existential types.

For example: the `Any` type.

```hs
data Any = forall a. Any a
```

`Any` is capable of storing a value of any type, and in doing so, forgets what type it has. The type constructor doesn't have any type variables, and so it can't remember anything - there's nowhere it can store information.

In order to introduce `a` type variable for `Any` to be polymorphic over, we can use the same `forall a.` as when working with rank-n types. This `a` type exists only within the context of `Any` data ctor, therefore it is *existential*.

Using GADTs, we can declare a more idiomatic (but the same) syntax for this construction:

```hs
data Any where
    Any :: a -> Any
```

It'd seem that we can declare a hetero list with Any, but its usefulness is limited due to the fact that its values can never be recovered.

*Existential types are eliminated (consumed) via continuation passing*. An eliminator is a rank-2 function which takes an existential type and a continuation that can produce a value regardless of what it gets. Elimination occurs when our existential type is fed into this rank-2 function.

The eliminator for Any is `elimAny`:

```hs
elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a
```

Pay attention to where the `a` and `r` types are quantified. The caller of `elimAny` gets to decide the result `r`, but `a` is determined by the type inside of the `Any`.

This approach of packing an existential type with a typeclass might be useful sometimes.

```hs
data HasShow where
    HasShow :: Show t => t -> HasShow

instance Show HasShow where
    show (HasShow s) = "HasShow " ++ show s
```

The definition of `HasShow` is remarkably similar to the GADT definition of `Any`, with the addition of the `Show` constraint. This constraint requires a Show instance whenever constructing a HasShow, and Haskell will remember this. Because a Show instance was required to build a HasShow, whatever type is inside of HasShow must have a Show instance. Haskell is smart enough to realize this, and allow us to call show on whatever type exists inside. We can use this fact to write a Show instance for HasShow.

More generally, we are able to write an eliminator for `HasShow` which knows we have a `Show` dictionary in scope.

```hs
elimHasShow :: (forall a. Show a => a -> r)
            -> HasShow
            -> r
elimHasShow f (HasShow a) = f a
```
