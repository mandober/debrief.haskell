# Except

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Except.html

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.Except.html#ExceptT


The `ExceptT` monad transformer extends a monad with the *ability to throw exceptions*.

A sequence of actions terminates normally, producing a value, only if none of the actions in the sequence throws an exception. If one throws an exception, the rest of the sequence is skipped and the composite action exits with that exception.

If the value of the exception is not required, the variant in `Control.Monad.Trans.Maybe` may be used instead.

## The ExceptT monad transformer

```hs
newtype ExceptT e m a = ExceptT (m (Either e a))
```

A monad transformer that adds exceptions to other monads.

`ExceptT` constructs a monad parameterized over two things:
- `e` The exception type
- `m` The inner monad

The `return` function yields a computation that produces the given value, while `>>=` sequences two subcomputations, exiting on the first exception.
