# RWST

The `RWST` data type is a monad transformer that combines the capabilities of the `ReaderT`, `WriterT` and `StateT` monad transformers into one.

```hs
newtype RWST r w s m a = RWST { unRWST :: r -> s -> w -> m (a, s, w) }
```

The "other" monad, i.e. the monad to be combined with the `RWST` monad transformer, is represented by the type parameter `m` which is placed before the return type, i.e. before the returned triple `(a, s, w)`, as `m (a, s, w)`.



## RWS type

The `RWS` type is an amalgam of the `Reader`, `Writer` and `State` types.

```hs
newtype Reader r a = Reader { runReader :: r ->     a  }
newtype Writer w a = Writer { runWriter ::      (w, a) }
newtype State  s a = State  { runState  :: s -> (s, a) }
```

And `RWST` monad transformer is an amalgam of the `ReaderT`, `WriterT` and `StateT` monad transformers.

```hs
newtype ReaderT r m a = ReaderT { runReaderT :: r ->     m a  }
newtype WriterT w m a = WriterT { runWriterT ::      m (w, a) }
newtype StateT  s m a = StateT  { runStateT  :: s -> (s, m a) }
```

- The `Reader r a` type is actually a function `r -> a`. 
- The `Writer w a` type is actually a pair `(w, a)`. 
- The `State` type is function that returns a pair, so it is already something of a combination of the `Reader` and `Writer`.
- The `RWS` is the amalgam of these 3 types, `Reader`, `Writer` and `State`.


```hs
newtype RWST r w s m a = RWST { unRWST :: r -> s -> w -> m (a, s, w) }
```
