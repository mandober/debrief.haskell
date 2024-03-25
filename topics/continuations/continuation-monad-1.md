# Continuation monad

The `transformers` package is where the monad transformer `ContT` is declared, followed but its implementation for FAM and other class' instances (by the way, the `mtl` package also mentions `ContT` but it imports it from the `transformers` package). Here's the link directly to the source: https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Cont.html#line-168

```hs
-- ContT as a monad transformer
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- ConT's Applicative instance
instance Applicative (ContT r m) where
  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  f <*> v = ContT $ \ c -> runContT f $ \ g -> runContT v (c . g)
```

I've kept failing to reimplement this class for `ContT` myself, and even when I manged to get it to type check in GHCi it didn't feel right. So I gave up and took a look at the official definition above. Why was that identifier placed exactly there? Just how did they know to introduce that lambda there but apply it way over here? I know the mantra that you should just follow types, but at the time that didn't help me at all.

However, it was exactly that lambda introduction that reminded me of natural deduction and the inference rules for implication introduction. Of course, this is the well-known Curry-Howard correspondence between logic and PLs, but so far I haven't stumbled onto the recommendation to actually use it try and ease a function implementation (spoiler: it's not a wonderfully straightforward way that just shits the correct function definition for you, but it is an aid).
