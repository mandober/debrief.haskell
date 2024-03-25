# Reader

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Reader.html

Contents
- The Reader monad
- The ReaderT monad transformer
- Reader operations
- Lifting other operations

## Control.Monad.Trans.Reader

Declaration of the `ReaderT` monad transformer, which adds a static environment to a given monad.

If the computation is to modify the stored information, use Control.Monad.Trans.State instead.

## The Reader monad


```hs
newtype Reader r a = Reader { runReadeT :: r -> a }

-- but defined in terms of ReaderT
type Reader r = ReaderT r Identity
```

The parameterizable reader monad.

Computations are functions of a shared environment.

The `return` function ignores the environment, while `m >>= k` passes the inherited environment to both subcomputations:

![Reader](https://hackage.haskell.org/package/transformers-0.6.1.1/docs/images/bind-ReaderT.svg)


```hs
-- Constructor for computations in the reader monad (equivalent to `asks`)
reader :: Monad m => (r -> a) -> ReaderT r m a

runReader
:: Reader r a    -- a Reader to run
-> r             -- initial environment
-> a             -- runs a Reader and extracts the final value from it
                 -- (inverse of `reader`)


-- Transform the value returned by a Reader.
-- runReader (mapReader f m) = f . runReader m
mapReader :: (a -> b) -> Reader r a -> Reader r b


-- runReader (withReader f m) = runReader m . f
withReader
  :: (r' -> r)   -- function to modify the environment
  -> Reader r a  -- Computation to run in the modified environment.
  -> Reader r' a -- Execute a computation in a modified environment
                 -- (a specialization of `withReaderT`)
```

## ReaderT monad transformer

```hs
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

The reader monad transformer, which adds a read-only environment to the given monad.

The return function ignores the environment, while m >>= k passes the inherited environment to both subcomputations:

![ReaderT](https://hackage.haskell.org/package/transformers-0.6.1.1/docs/images/bind-ReaderT.svg)



```hs
-- Transform the computation inside a ReaderT.
-- runReaderT (mapReaderT f m) = f . runReaderT m
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b


-- runReaderT (withReaderT f m) = runReaderT m . f
withReaderT
  :: (r' -> r)      -- function to modify the environment
  -> ReaderT r m a  -- Computation to run in the modified environment
  -> ReaderT r' m a -- Execute a computation in a modified environment
                    -- (more general version of `local`)
```

### Reader operations

Reader,  {reader, mapReader, withReader},
ReaderT, {runReaderT, mapReaderT, withReaderT}, 
{ask, asks, local}



```hs
-- | Fetch the value of the environment.
ask :: Monad m => ReaderT r m r

-- | asks f = liftM f ask
asks :: Monad m 
     => (r -> a)      -- selector function to apply to the environment
     -> ReaderT r m a -- Retrieve a function of the current environment


-- | runReaderT (local f m) = runReaderT m . f
local :: (r -> r)      -- The function to modify the environment
      -> ReaderT r m a -- Computation to run in the modified environment
      -> ReaderT r m a -- Execute a computation in a modified environment
                       -- (a specialization of withReaderT)
```



## Lifting other operations

```hs
-- Lift a callCC operation to the new monad.
liftCallCC :: CallCC m a b -> CallCC (ReaderT r m) a b

-- Lift a catchE operation to the new monad.
liftCatch :: Catch e m a -> Catch e (ReaderT r m) a
```
