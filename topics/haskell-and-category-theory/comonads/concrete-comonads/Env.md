# Env comonad

The Env (aka Environment, co-Reader, Product) comonad

- https://hackage.haskell.org/package/comonad
- https://hackage.haskell.org/package/comonad/docs/Control-Comonad-Env.html
- https://hackage.haskell.org/package/transformers
- https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-Reader.html
- https://hackage.haskell.org/package/mtl
- https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html
- https://www.youtube.com/watch?v=FtaT73bpEGs&list=PLcAu_kKy-krxDD1WwRX_9rc0knAFK3nHs&index=9


A co-Kleisli arrow, `w a -> b`, in the `Env` comonad is isomorphic
to a Kleisli arrow, `a -> m b`, in the `Reader` monad:

>a -> e -> m   ≅   (a, e) -> m   ≅   Env e a -> m
>a -> e -> a   ≅   (a, e) -> a   ≅   Env e a -> a

The `Env` comonad
- has a single slot, `a`
- has a single view to the context, `e`
- it's like an object-oriented `Reader` monad

Unlike the `Stream` comonad, which has inifinitely many slots, the `Env` comonad has a single slot. Thus, we can view it from only one spot. The `Env` also resambles the `Reader` monad, which has a fixed context (environment) to read from, that is, it is a way to implicitly pass some information around without threading that info through each function explicitly, as an arg. The `Env` is the opposite of that in that we're still passing information around but we have that information explicitly as a concrete object that we can manipulate as we like, whereas with `Reader` we're waiting for the info to be provided.

The definition of `Env` shows it's just a named pair, i.e. it is isomorphic to a pair `(e, a)`.

```hs
data Reader e a = Reader (e -> a)

data Env e a = Env e a
-- actually, it is def in terms of EnvT comonad transformer
type Env  e   a = EnvT e Identity a
data EnvT e w a = EnvT e (w a)
-- EnvT comonad transformer
```

The `Env` is a comonad:

```hs
instance Comonad (Env e) where
  extract :: Env e a -> a
  extract (Env _ a) = a

  duplicate :: Env e a -> Env e (Env e a)
  duplicate (Env e a) = Env e (Env e a)

  extend :: (Env e a -> b) -> (Env e a -> Env e b)
  extend f w@(Env e a) = Env e (f w)
  -- extend f w@(Env e a) = fmap f (Env e w)
  -- extend f w = fmap f (duplicate w)
```

The functions on the `Env`: similarly to the `Reader`, there is the `ask` function that gets us the environment.

```hs
-- | Fetch the environment
ask :: (Comonad w) => Env e a -> e
ask (Env e _) = e

-- | Fetch the environment
ask :: Reader r r
ask (Reader)
```

The primary use case for the `Env` comonad is that it's really handy when you don't want to work in a monadic context, but you just want to have a hold on the real value, a value that you can pass around and mutate. Compared to the `Reader` monad, the `Env` comonad is like an object-oriented approach, where you have this object with all the context and you can perform operations on it by using the queries as if though they are methods on that object. The queries have access to all the fields of the object, and can even make changes to the fileds. Also, when you extend a query it is as though you're doing a mutation because it calculates the new value and sets it back into a slot. The other part of it is with comonad transformers - you can put the `Env` anywhere in the comonadic transformer stack, just like you can put the `Reader` anywhere in the monadic stack.
