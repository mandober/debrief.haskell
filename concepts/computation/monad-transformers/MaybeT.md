# MaybeT

https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Maybe.html

- module: Control.Monad.Trans.Maybe
- monad transformer: `MaybeT` in Control.Monad.Trans.Maybe.MaybeT
- precursor monad: `Maybe`
- models: fallible computation (Maybe, MaybeT)
- capability:
  - fallible computation with early exit
  - as soon as any branch returns `Nothing`, the entire computation exists
  - to yield a value, all branches (computations) must successfully finish
  - no error message - only error signalling by returning `Nothing`


```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

The `MaybeT` monad transformer extends an arbitrary monad `m` with the ability to (early) exit the computation without returning a value.

A sequence of actions produces a value iff all the actions in the sequence successfully finish. If any one fails (exits), the rest of the sequence is skipped and the overall (composite) action exits (early return).

For a variant of this monad that allows defining a range of exception cases (values, conditions), see `ExceptT` in the module `Control.Monad.Trans.Except`.

## MaybeT monad transformer

The `MaybeT` monad transformer is a parameterizable `Maybe` monad with a slot `m` intended to plug in another monad, thus obtaining the complete type that integrates the behaviour of both monads (`Maybe` and the plugged in monad).

Computations in the `Maybe` monad, and by extension in the `MaybeT` MT, are actions that either all finish producing a value or exit early if any one computation fails.

The `return` method builds a computation that returns the given value; i.e. it lift the given value into a *fallible computation*.

The `>>=` sequences two subcomputations, exiting if either computation does.
