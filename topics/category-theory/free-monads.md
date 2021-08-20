# Free monads

https://hackage.haskell.org/package/free-5.1.7

Free monads are useful for many tree-like structures and domain specific languages.

If `f` is a functor then the *free monad on the functor `f`* is the type of trees whose nodes are labeled with the constructors of `f`.

The word "free" is used in the sense of "unrestricted" rather than "zero-cost": `Free f` makes no constraining assumptions beyond those given by `f` and the definition of `Monad`. As used here, it is a standard term from the mathematical theory of *adjoint functors*.

*Cofree comonads*, dual to *free monads*, provide convenient ways to talk about branching streams and rose-trees, and can be used to annotate syntax trees. The cofree comonad can be seen as a stream parameterized by a Functor that controls its branching factor.

More information:
- http://comonad.com/reader/2008/monads-for-free/
- http://comonad.com/reader/2011/free-monads-for-less/


## Monads for Free - Edward Kmett, 2008
http://comonad.com/reader/2008/monads-for-free/

Packlages:
- https://hackage.haskell.org/package/comonad-5.0.8/docs/Control-Comonad.html
- https://hackage.haskell.org/package/free-5.1.7/docs/Control-Monad-Free.html
- https://hackage.haskell.org/package/free-5.1.7/docs/Control-Comonad-Cofree.html


Monads provide substitution, as `fmap`, and renormalization, as `Control.Monad.join`:

m >>= f = Control.Monad.join (fmap f m)

A free monad is one that does no work during the normalization step beyond simply grafting the two monadic values together. In this sense, `[]` is not a free monad because `Control.Monad.join [[a]]` smashes the lists flat, producing a `[a]`. On the other hand, consider:

```hs
data Tree a = Tip a | Bin (Tree a) (Tree a)

instance Monad Tree where
  return :: a -> f a
  return = Tip

  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (>>=) (Tip x)   k = k x
  (>>=) (Bin l r) k = Bin (l >>= k) (r >>= k)
```

This monad is the free Monad of `Pair`:

```hs
data Pair a = Pair a a
```

And we could make an instance of `MonadFree` for it directly:

```hs
instance MonadFree Pair Tree where
  wrap (Pair l r) = Bin l r
```

Or we could choose to program with Control.Monad.Free.Free Pair instead of Tree
and thereby avoid having to define our own Monad instance.

Moreover, "Control.Monad.Free.Church" provides a MonadFree
instance that can improve the /asymptotic/ complexity of code that
constructs free monads by effectively reassociating the use of
(>>=). You may also want to take a look at the kan-extensions
package (<http://hackage.haskell.org/package/kan-extensions>).

See 'Control.Monad.Free.Free' for a more formal definition of the free 'Monad'
for a 'Functor'.


```hs
class Monad m => MonadFree f m | m -> f where
  -- Add a layer
  -- wrap (fmap f x)  ≡  wrap (fmap return x) >>= f
  wrap :: f (m a) -> m a

  default wrap :: (m ~ t n, MonadTrans t, MonadFree f n, Functor f) 
               => f (m a)
               -> m a
  wrap = join . lift . wrap . fmap return
```
