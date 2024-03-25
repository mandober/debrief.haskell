# Traced comonad

The `Traced` comonad is a dual to the `Writer` monad, it's a co-Writer.

>Traced m a = (Monoid m) => m -> a

That `m` is a monoid is important because we need that neutral element `mempty` for, crucially, `extract`, but also other functions.

The trace comonad builds up a result by prepending monoidal values to each other.

The module `Control.Comonad.Traced` specifies the traced comonad transformer, `TracedT` (aka the co-writer, or the exponential comonad transformer).

```hs
newtype (Monoid m) => Traced m a = Traced { runTraced :: m -> a }

type    Traced  m   a = TracedT m Identity a
newtype Traced  m   a = Traced  { runTraced  ::    m -> a  }
newtype TracedT m w a = TracedT { runTracedT :: w (m -> a) }

-- Traced comonad is dual to the Writer monad
type    Writer  w   a = WriterT Identity a
newtype Writer  w   a = Writer  { runWriter  ::   (w, a) }
newtype WriterT w m a = WriterT { runWriterT :: m (w, a) }
```

- Traced wraps a function where the argument is a Monoid
- Traced is a query by itself
- Traced is like the `Store` but all positions are relative
- i.e. there's no access to the current position
- `m -> a` is only a comonad when `m` is a Monoid


## Traced queries

```hs
extract :: Traced m a -> a
extract (Traced f) = f mempty
```
