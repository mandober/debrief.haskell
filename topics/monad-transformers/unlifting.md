# Unlifting

https://github.com/fpco/unliftio/


## Unlifting in 2 minutes

Let's say we have a function:

```hs
readFile :: FilePath -> IO ByteString
```

But we are in a function that uses `ReaderT Env IO`, not just plain `IO`.
>How can we call the `readFile` function in that context?

One way is to manually unwrap the `ReaderT` data ctor:

```hs
readFile' :: FilePath -> ReaderT Env IO ByteString
readFile' fp = ReaderT $ \_env -> readFile fp
```

But having to do this regularly is tedious, and ties our code to a specific monad transformer stack. Instead, many of us would use `MonadIO`:

```hs
readFile'' :: MonadIO m => FilePath -> m ByteString
readFile'' = liftIO . readFile
```


But now let's play with a different function:
```hs
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
```
We want a function with signature:
```hs
myWithBinaryFile
    :: FilePath
    -> IOMode
    -> (Handle -> ReaderT Env IO a)
    -> ReaderT Env IO a
```

If we squint hard enough, we can accomplish this directly with the `ReaderT` constructor as:

```hs
myWithBinaryFile fp mode inner = ReaderT $ \env ->
  withBinaryFile fp mode (\h -> runReaderT (inner h) env)
```

I dare you to try and accomplish this with `MonadIO` and `liftIO`. It simply can't be done. (technically, it is because IO appears in negative arg position in `withBinaryFile`).

However, with `MonadUnliftIO`, this is possible:

```hs
import Control.Monad.IO.Unlift

myWithBinaryFile :: MonadUnliftIO m
    => FilePath
    -> IOMode
    -> (Handle -> m a)
    -> m a
myWithBinaryFile fp mode inner =
  withRunInIO $
     \runInIO ->
       withBinaryFile fp mode (\h -> runInIO (inner h))
```

That's it, you now know the entire basis of this library.

## How common is this problem?

This pops up in a number of places, including
- proper exception handling, with `bracket`, `catch`, and `finally`
- working with `MVars` via `modifyMVar` and similar
- using the timeout function
- installing callback handlers (e.g. to log in a signal handler)

This also pops up when working with libraries which are *monomorphic on IO*, even if they could be written more extensibly.

## Examples


## Limitations



## Comparison to other approaches

You may be thinking "Haven't I seen a way to do `catch` in `StateT`?" You almost certainly have. Let's compare this approach with alternatives. 

For an older but more thorough rundown of the options, see [Exceptions and monad transformers](http://www.yesodweb.com/blog/2014/06/exceptions-transformers),

There are really two approaches to this problem:

1. Use a set of typeclasses for the specific functionality we care about. This is the approach taken by the exceptions package with MonadThrow, MonadCatch, and MonadMask. (Earlier approaches include `MonadCatchIO`-mtl and `MonadCatchIO`-transformers).

2. Define a generic typeclass that allows any control structure to be unlifted. This is the approach taken by the monad-control package. (Earlier approaches include `monad-peel` and `neither`).

The first style gives extra functionality in allowing instances that have nothing to do with runtime exceptions (e.g., a MonadCatch instance for Either). This is arguably a good thing. The second style gives extra functionality in allowing more operations to be unlifted (like threading primitives, not supported by the exceptions package).

Another distinction within the generic typeclass family is whether we unlift to just IO, or to arbitrary base monads. For those familiar, this is the distinction between the MonadIO and MonadBase typeclasses.

This package's main objection to all of the above approaches is that they work for too many monads, and provide difficult-to-predict behavior for a number of them (arguably: plain wrong behavior). For example, in lifted-base (built on top of monad-control), the finally operation will discard mutated state coming from the cleanup action, which is usually not what people expect. exceptions has different behavior here, which is arguably better. But we're arguing here that we should disallow all such ambiguity at the type level.

So comparing to other approaches:
