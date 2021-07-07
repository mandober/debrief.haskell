# Control.Monad.IO.Class

```hs
class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

-- impl for IO
instance MonadIO IO where
  liftIO = id
```

## MonadIO class

```hs
class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a
```

- Class of monads based on IO
- Monads in which IO computations may be embedded
- Any monad built by applying a sequence of monad transformers to the IO monad will be an instance of this class


Instances should satisfy the following **laws**:
(which state that `liftIO` is a transformer of monads)
1. liftIO . return = return
2. liftIO (m >>= f) = liftIO m >>= (liftIO . f)


## MonadIO class: liftIO method

```hs
liftIO :: (MonadIO m) => IO a -> m a
```

- Lift a computation from the `IO` monad.
- This allows us to run IO computations in any monadic stack, so long as it supports these kinds of operations, i.e. `IO` is the base monad for the stack.


### Example

```hs
import Control.Monad.Trans.State  -- from the "transformers" library

printState :: Show s => StateT s IO ()
printState = do
  state <- get
  liftIO $ print state

-- expected results
x1 = evalStateT printState "hello" -- "hello"
x2 = evalStateT printState 3       -- 3
```

Had we omitted `liftIO`, we would have ended up with this error:

  • Couldn't match type `IO` with `StateT s IO`
  • Expected type: `StateT s IO ()`
  • Actual type: `IO ()`

The important part here is the mismatch between `StateT s IO ()` and `IO ()`.Luckily, we know of a function that takes an `IO a` and returns an `m a`, i.e. the `liftIO`, enabling us to run the program and see the expected results.
