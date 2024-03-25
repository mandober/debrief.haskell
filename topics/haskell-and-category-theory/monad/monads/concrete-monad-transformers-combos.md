# Combinations of concrete monad transformers

Concrete monad transformers:

```hs
newtype IdentityT m a = IdentityT { runIdentityT  ::           m  a) }
newtype MaybeT    m a = MaybeT    { runMaybeT     ::           m  a) }
newtype ListT     m a = ListT     { runListT      ::           m  a) }

newtype ReaderT r m a = ReaderT   { runReaderT    ::      r -> m  a        }
newtype WriterT w m a = WriterT   { runWriterT    ::           m (a, w)    }
newtype StateT  s m a = StateT    { runStateT     ::      s -> m (a, s)    }
newtype RWST  r s m a = RWST      { runRWST       :: r -> s -> m (a, s, w) }
```

Combinations of concrete monad transformers:



- Reader,                     `r ->   a`
- ReaderT,                    `r -> m a`
- Writer (Lazy, Strict, CPS),   `(a, w)`
- WriterT                     `m (a, w)`
- State  (Lazy, Strict),      `s ->   (a, s)`
- StateT                      `s -> m (a, s)`
- RWS    (Lazy, Strict, CPS)  `r -> s ->   (a, s, w)`
- RWST                        `r -> s -> m (a, s, w)`
- Identity
- Maybe
- Except
- Select
- Accum
- Cont


Kleisli arrows, `a -> m b`
- Reader    `r ->   a`                   ((->) r)
- ReaderT   `r -> m a`
- Writer          `(a, w)`               ((,) a)
- WriterT         `m (a, w)`
- State     `s ->   (a, s)`
- StateT    `s -> m (a, s)`
- RWS       `r -> s ->   (a, s, w)`
- RWST      `r -> s -> m (a, s, w)`
