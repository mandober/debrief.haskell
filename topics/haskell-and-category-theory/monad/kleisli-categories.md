# Kleisli categories

Normally, functions have the type `a -> b`, but Kleisli arrows have an embellished return type, `a -> m b`, which allows us to encode many effectful constructs with pure functions.

A Kleisli category has types as objects, but we'll redefine the morphisms to be the embellished functions, called Kleisli arrows. To constitute a category Kleisli arrows need to compose and we cannot compose any two of them due to the input/output mismatch.

The usual composition operator is intended for composition of normal functions:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \a -> g (f a)
```

And the Kleisli arrows have the type `a -> m b`, so their composition would not be possible with `.`. We need a different composition operator, called "the fish", `>=>`. Unlike `.`, the fish takes functions in left-to-right order. In fact, there is another two fish operator that takes functions in right-to-left order, `<=<`.

```hs
(>=>) :: forall m a b c. Functor m => (a -> m b) -> (b -> m c) -> (a -> m c)
mf >=> mg = \a -> let mb :: m b = mf a
                      mc = mb >>= mg
                  in  mc  where
  (>>=) :: m b -> (b -> m c) -> m c
  mb >>= bmc = join $ fmap bmc mb
    where
    join :: m (m c) -> m c
    join = undefined -- impl depends on the carrier type ctor
```

The `>=>` operator is more appropriate for point-free programming - an operator derived from it, called bind, `>==`, is more suitable for point-full style of programming.

The code above shows how the bind is derived from the fish, but it also shows that we cannot define `>=>` or `>>=` completely because both, along with `join`, actually depend on the carrier type ctor `m`. That's why the `Monad` (aka the category of Kleisli arrows) is a class, defined in terms of `return` and either `bind` or `join`. The `return` method, `a -> m a`, serves as the unit of Kleisli composition, just like the `id` function is the unit of regular function composition.

```hs
class Applicative m => Monad m where
  return :: a -> m a
  return = pure
  -- and either
  (>>=) :: m a -> (a -> m b) -> m b
-- or
  join :: m (m a) -> m a
```

Haskell std chose to define the `Monad` class in terms of `return` and `bind`, with `join` as a separate function with default implementation in terms of `bind`. That is, `bind` and `join` are definable in terms of each other. Also Haskell's class `Monad` is a subclass of `Applicative`, which in turn is a subclass of `Functor`. This implies all monads are functor, so the `fmap` method is always available, as are `Applicative`'s methods, `<*>`, `<*` and `*>`, with `pure` being the same as `return`. In fact, the recent advance in Haskell is to enforce the Monad's `return` method to always be defined in terms of `pure`.
