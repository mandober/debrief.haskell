# Accum

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Accum.html

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.Accum.html#AccumT

The lazy `AccumT` monad transformer, which adds *accumulation capabilities* (such as declarations or document patches) to a given monad. 

Each computation has access to the combination of the *input environment and outputs added* so far, and returns the outputs added.

In applications requiring only the ability to accumulate an output and to inspect the output so far, it would be considerably more efficient to use `Control.Monad.Trans.State` instead.

## The Accum monad

```hs
type Accum w = AccumT w Identity


```

An accumulation monad parameterized by the type w of output to accumulate.

The `Accum` monad is a more complex extension of both `Reader` and `Writer` monads.

- The `return` function produces the output `mempty`.
- The bind function, `m >>= k`, uses the output of `m` both to extend the initial environment of `k` and to combine with the output of `k`:

![Accum](https://hackage.haskell.org/package/transformers-0.6.1.1/docs/images/bind-AccumT.svg)

In applications requiring only the ability to accumulate an output and to inspect the output so far, it would be considerably more efficient to use a `State` monad.


## The AccumT monad transformer

```hs
newtype AccumT w m a = AccumT (w -> m (a, w))
```

An accumulation monad parameterized by:
- `w` the output to accumulate
- `m` The inner monad

The `AccumT` monad transformer is a more complex extension of both `ReaderT` and `WriterT` monad transformers.

- The `return` function produces the output `mempty`
- The bind function, `m >>= k`, uses the output of `m` both to extend the initial environment of `k` and to combine with the output of `k`:

![AccumT](https://hackage.haskell.org/package/transformers-0.6.1.1/docs/images/bind-AccumT.svg)

In applications requiring only the ability to accumulate an output and to inspect the output so far, it would be considerably more efficient to use a `StateT` monad transformer.
