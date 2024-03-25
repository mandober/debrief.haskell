# Free monads

## Control.Monad.Free

https://hackage.haskell.org/package/free

Monads provide substitution with `fmap` and renormalization with `join`:
>m >>= f = Control.Monad.join (fmap f m)

A **free monad** is one that does no work during the normalization step beyond simply grafting the two monadic values together.

List is not a free monad (in this sense) because `join [[a]]` smashes the lists flat, into `[a]`. On the other hand, consider this `Tree` data type:

```hs
data Tree a = Bin (Tree a) (Tree a) | Tip a

instance Monad Tree where
  return = Tip

  Tip a >>= f = f a
  Bin l r >>= f = Bin (l >>= f) (r >>= f)
```

This monad is the free monad of `Pair`:

```hs
data Pair a = Pair a a
```

And we could make an instance of the `MonadFree` class for it directly:

```hs
instance MonadFree Pair Tree where
  wrap (Pair l r) = Bin l r
```

Or we could choose to program with `Control.Monad.Free.Free Pair` instead of `Tree` and thereby avoid having to define our own `Monad` instance.

Moreover, `Control.Monad.Free.Church` provides a `MonadFree` instance that can improve the asymptotic complexity of code that constructs free monads by effectively reassociating the use of `>>=`.

You may also want to take a look at the `kan-extensions` package:
http://hackage.haskell.org/package/kan-extensions

See Control.Monad.Free.Free for a more formal definition of the free `Monad` for a `Functor`.

### MonadFree class

(the above text is docs to it)

```hs
class Monad m => MonadFree f m | m -> f where
  -- | Add a layer.
  -- wrap (fmap f x) ≡ wrap (fmap return x) >>= f
  wrap :: f (m a) -> m a
  default wrap :: (m ~ t n, MonadTrans t, MonadFree f n, Functor f) 
               => f (m a)
               -> m a
  wrap = join . lift . wrap . fmap return
```

>wrap (fmap f x) ≡ wrap (fmap return x) >>= f


## Control.Monad.Free.Church

https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free-Church.html

Free Monads for Less
https://ekmett.github.io/reader/2011/free-monads-for-less/
https://ekmett.github.io/reader/2008/monads-for-free/
