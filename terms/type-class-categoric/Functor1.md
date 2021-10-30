# Functor class


```hs
class Functor (f :: * -> *) where
    fmap  :: (a -> b) -> f a -> f b
    (<$)  ::  a       -> f b -> f a
    (<$) = fmap . const
-- MINIMAL: fmap
```

The `f` in the signature repr a unary data ctor, that takes a type to a type (a star to a star), `* -> *`. Type ctors that match this sig and that are indeed functors are `[]`, `Maybe`, `IO` - they match the sig naturally, as they are. On the other hand, some types of larger arity could be massaged into conforming to the functor requirements. So, for example, `Either` can be made a Functor in the second type param by fixing its first type param - `Either e` (note the form: it is `Either a` not just `Either`) is indeed a functor, and so is `((->) r)`, `((,) r)`, `Map k`.





For example, `Maybe` is such a type for its kind is `* -> *`. Maybe is not a concrete type per se, but requires a type as a parameter in order to be complete (e.g. `Maybe Int`). Thus, writing `instance Functor Int` is not valid, but `instance Functor Maybe` is.

`fmap` takes a (normal, unlifted) function `(a -> b)` and a value of type `f a`, and returns a value of type `f b`.

Under the container interpretation, `fmap` applies a function to each element within, without altering its structure.

Under the context interpretation, `fmap` applies a function to a value without altering its context.

Instead of applying a function to values in a container or context, the `<$` operator merely replaces them with a given value. This is the same as applying a constant function, so (<$) can be implemented in terms of `fmap`.

## List as a functor

Under the container interpretation, the list constructor `[]` is a functor since we can use `map` to apply a function to each element of a list. The `Maybe` type constructor is also a functor, representing a container which might hold a single element. The function `fmap g` has no effect on `Nothing`, otherwise it applies `g` to the `Just a` value.

```hs
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map
    -- or, in a long-winded way:
    fmap _ []     = []
    fmap g (x:xs) = g x : fmap g xs
```


## Maybe as a functor

Under the context interpretation, the list functor represents a context of nondeterministic choice; that is, a list is thought as representing a single value which is nondeterministically chosen out of several possible values (list elements). Likewise, the Maybe functor represents a context with a possible failure.


```hs
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap g (Just a) = Just (g a)
```

## Other Functor instances

* `Either e` is an instance of Functor. `Either e a` represents a container which can contain either a value of type `a`, or a value of type `e`. It is similar to Maybe in that it represents possible failure, but it can carry some extra information about that failure.

* `((,) e)` represents a container which holds an *"annotation"* of type `e` along with the actual value. Although it may be clearer to write it as `(e,)` (by analogy with sectioning) that syntax is not allowed in types (eneble it for expressions with *TupleSections* GHC extension).

* `((->) e)`, in disallowed syntax `(e -> ...)`, is the type of functions that take a parameter of type `e`. As a container, `(e -> a)` represents a (possibly infinite) set of values of `a` indexed by values of `e`. Also, `((->) e)` can alternatively be thought as a context in which a value of type `e` is available to be consulted in a read-only fashion; this is why `((->) e)` is sometimes referred to as *the reader monad*.

* `IO`: a value of type `IO a` represents an effectual computation that computes the value of type `a`. If `m` effectfully computes the value `x`, then `fmap g m` computes the value `g x` while performing the same side-effects.

* Many types from the `containers` internal package, such as `Tree`, `Map`, `Sequence` are also `Functor` instances.

A notable exception is `Set`, which cannot be a Functor in Haskell (although it is in math) since it requires an `Ord` constraint on its elements; `fmap` must be applicable to any types `a` and `b`. However, `Set` can be an instance of a suitable Functor generalization, either by making `a` and `b` args to the `Functor` class themselves, or by adding an associated constraint.

- [Instances for Set of Functor](http://archive.fo/9sQhq)
- [Constraint Kinds for GHC](http://blog.omega-prime.co.uk/2011/09/10/constraint-kinds-for-ghc/)
- [Haskell Programming: Attractive Types](https://archive.fo/40Wg3)
- [Haskell Restricted Types](http://okmij.org/ftp/Haskell/types.html)
