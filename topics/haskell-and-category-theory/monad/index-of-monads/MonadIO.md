# MonadIO

`MonadIO` is a type class from the `Control.Monad.IO.Class` modul that groups monads in which `IO` computations may be embedded.

Any monad built by applying a sequence of monad transformers to the `IO` monad may be an instance of this class.

Instances should satisfy the following laws, which state that `liftIO` is a transformer of monads:

* liftIO  . return = return
* liftIO (m >>= f) = liftIO m >>= (liftIO . f)


```hs
import Control.Monad.IO.Class

class (Monad m) => MonadIO m where
  -- | Lift a computation from the IO monad.
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id
```
