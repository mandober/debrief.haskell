# Writer

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Writer-CPS.html


## WriterT monad transformer

```hs
data WriterT w m a
```

A writer monad parameterized by:
- `w` output to accumulate
- `m` inner monad

The `return` function produces the output `mempty`, while `m >>= k` combines the outputs of the subcomputations using `mappend` (aka `<>`):

![alt text](https://hackage.haskell.org/package/transformers-0.6.1.1/docs/images/bind-WriterT.svg)



## Writer operations

- `tell w` is an action that produces the output `w`
- `listen m` is an action that executes the action `m` and adds its output to the value of the computation.
- `listens f m` is an action that executes the action `m` and adds the result of applying `f` to the output to the value of the computation.
- `pass m` is an action that executes the action `m`, which returns a value and a function, and returns the value, applying the function to the output.
- `censor f m` is an action that executes the action `m` and applies the function `f` to its output, leaving the return value unchanged.


```hs
tell :: (Monoid w, Monad m) => w -> WriterT w m ()

listen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a, w)
-- runWriterT (listen m) = liftM (\ (a, w) -> ((a, w), w)) (runWriterT m)

listens :: (Monoid w, Monad m) => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
-- listens f m = liftM (id *** f) (listen m)
-- runWriterT (listens f m) = liftM (\ (a, w) -> ((a, f w), w)) (runWriterT m)

pass :: (Monoid w, Monoid w', Monad m) => WriterT w m (a, w -> w') -> WriterT w' m a
-- runWriterT (pass m) = liftM (\ ((a, f), w) -> (a, f w)) (runWriterT m)

censor :: (Monoid w, Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
-- censor f m = pass (liftM (\ x -> (x,f)) m)
-- runWriterT (censor f m) = liftM (\ (a, w) -> (a, f w)) (runWriterT m)
```
