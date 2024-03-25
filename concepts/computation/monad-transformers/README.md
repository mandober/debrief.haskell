# MTs


- some MTs discard (state) values

- What's a monad transformer?
- Adds extra functionality to an existing monad
- Convenient way to get this functionality
- Example: ReaderT avoids needing to pass an extra argument to functions

Which transformers are we covering?
- ReaderT, StateT, ExceptT: covered explicitly
- IdentityT, WriterT, LoggingT, MaybeT: covered implicitly
- They are isomorphic to something mentioned above
- Continuation-based transformers (ContT, Conduit)

Meet the transformers

```hs
newtype ReaderT r m a = ReaderT (r -> m a)
newtype StateT  s m a = StateT  (s -> m (a, s))
newtype ExceptT e m a = ExceptT (     m (Either e a))

-- Or specialized to IO and turned into functions:
type ReaderIO r a = r -> IO a
type StateIO  s a = s -> IO (a, s)
type ExceptIO e a =      IO (Either e a)
```



`lifted-async`

```hs
-- Control.Concurrent.Async.Lifted from lifted-async
concurrently
  :: MonadBaseControl IO m
  => m a
  -> m b
  -> m (a, b)
```

- MonadBaseControl is from monad-control
- Things that can be turned into IO and back... sort of
- Lots of instances, including ReaderT, StateT and ExceptT


Implement concurrently for `StateT`
- Why does this happen? Let's implement concurrently for `StateT`

```hs
concurrentlyS
  :: StateT s IO a
  -> StateT s IO b
  -> StateT s IO (a, b)
concurrentlyS (StateT f) (StateT g) = StateT $ \s0 -> do
  ((a, s1), (b, s2)) <- concurrently (f s0) (g s0)
  return ((a, b), s1)
```

We generated two states, and have to discard one of them!

Control functions
- Arguably bad term, but it's used a lot
- Functions which take IO/m as arguments
- Aka contravariant in the monad
- Aka monad appears in negative position
- More info on nomenclature: https://www.fpcomplete.com/blog/2016/11/covariance-contravariance

But does it lift?
- Which of these functions can be converted to StateT s IO with lift?

```hs
putStrLn          :: String -> IO a
forkIO            :: IO ()  -> IO ThreadId
catch             :: Exception e => IO a -> (e -> IO a) -> IO a
try               :: Exception e => IO a -> IO (Either e a)
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
modifyMVar_       :: MVar  a -> (a -> IO a)   -> IO ()
```

* Monad transformer environment versus state

```hs
newtype ReaderT r m a = ReaderT (r -> m a)
newtype StateT s m a  = StateT  (s -> m (a,s))
newtype ExceptT e m a = ExceptT (     m (Either e a))
```

- `ReaderT` has a transformer env (`r`) but doesn't modify output `m a`
- `ExceptT` has no env, but has output state `m (Either e a)` instead of `m a`
- `StateT` has both, `s` as input, `m (a, s)` as output

Unlifting
- Unlifting is taking a control operation living in IO and moving it into a MT
- Transformers with no monadic state can safely "unlift" control operations
- Transformers with monadic state may require discarding when unlifting

ReaderT-like things
- Any transformer without state is isomorphic to ReaderT. Examples:
- `IdentityT`, pretend `()` is the environment
- `LoggingT`, the logging function is the environment
- `NoLoggingT` is just a newtype on `IdentityT`


## Unlifting without discarding

- If a control function only takes one action as input, you can get away without discarding.

```hs
tryS (StateT f) = StateT $ \s -> do
  eres <- try (f s)
  return $
    case eres of
      Left e -> (Left e, s)
      Right (a, s') -> (Right a, s')
```

Natural linear call path
- Even though `catch` has two input actions, the handler is only called after the main action completes.

```hs
catchS (StateT f) onErr = StateT $ \s ->
  f s `catch` (flip runStateT s . onErr)
```

No updated state is available from main action, since an exception was thrown - this is safe!

Finally a problem: Loses state updates in `g`:
```hs
finallyS (StateT f) (StateT g) = StateT $ \s ->
  f s `finally` g s
```

Instead have to reimplement functionality:
```hs
finallyS (StateT f) (StateT g) =
  StateT $ \s0 -> mask $ \restore -> do
    res <- try $ restore $ f s0
    case res of
      Left e -> do
        _ <- restore $ g s0
        throwIO (e :: SomeException)
      Right (s1, x) -> do
        (s2, _) <- restore $ g s1
        return (s2, x)
```

The problem cases - two categories of problem cases
1. Things like `finally`: can manually reimplement them to get the state retaining behavior desired. Problems:
  - End up with mismatched semantics between libraries (the bracket_ example).
  - Tedious and error-prone to reimplement these functions.
2. Things which cannot be solved, like `concurrently`

## Existing generic solutions

Two basic approaches today for typeclass-based control function lifting.

1. `exceptions`

Define an mtl-style typeclass for each operation:

```hs
class Monad m => MonadThrow m where
  throwM :: Exception e => e -> m a

class MonadThrow m => MonadCatch m where
  catch :: Exception e => m a -> (e -> m a) -> m a
```

Need an extra typeclass for each operation (forking, timeout, etc).

2. `monad-control`

Define a generic interface for all unlifting:

```hs
class MonadBase b m => MonadBaseControl b m | m -> b where
  type StM m a :: *
  liftBaseWith :: (RunInBase m b -> b a) -> m a
  restoreM :: StM m a -> m a
```

Difficult to understand, easy to write buggy instances, more likely to implement bad discard behavior.

3. `unliftio`
New entry in the market for control-like things

```hs
class MonadIO m => MonadUnliftIO m where
  askUnliftIO :: m (m a -> IO a)
```

- Slightly different in practice (impredicativity...)
- Only has valid instances for ReaderT-like things
- Specialized to IO for simplicity
- Can do similar things with type hackery on MonadBaseControl
- https://www.stackage.org/package/monad-unlift
- Control.Concurrent.Async.Lifted.Safe


## Providing StateT and ExceptT features
- But I want to have state and deal with failures! Practical recommendations:
- Feel free to use any monad transformer "in the small," where you're not forking threads or acquiring resources
- Keep your overall applications to ReaderT env IO (or use RIO)
- Prepare torches and pitchforks for the next two slides

Use mutable variables
- `StateT` is inherently non-thread-safe
- It also doesn't allow state to survive a runtime exception
- Use a mutable variable and keep it in a `ReaderT`
- Choose the correct mutable variable based on concurrency needs
- Recommendation: default to `TVar` unless good reason to do otherwise

Use runtime exceptions
- If you're in IO, you have to deal with them anyway
- Less type safe than `ExceptT`? Yes
- But that's the Haskell runtime system
- Also, you have to deal with async exceptions anyway
- Caveat emptor: Many people disagree with me here


## Conclusion

- We like our `StateT` and `ExceptT` transformers
- We want to naturally lift functions into them
- It simply doesn't work in many cases
- Use libraries that don't silently discard your state
- You'll sometimes get stuck using less elegant things...
- But at least they work


## References

This talk is based on a series of blog posts. Get even more gory details!

https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets
https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
https://www.fpcomplete.com/blog/2017/07/the-rio-monad
https://www.fpcomplete.com/blog/2017/07/announcing-new-unliftio-library
