# 3.1 Errors

Errors during expression evaluation, denoted by ⊥ (bottom"), are indistinguishable by a Haskell program from non-termination.

Since Haskell is a non-strict language, all Haskell types include ⊥. That is, a value of any type may be bound to a computation that, when demanded, results in an error.

When evaluated, errors cause immediate program termination and cannot be caught by the user. The Prelude provides two functions to directly cause such errors:

```hs
undefined :: a
error     :: String -> a
```

A call to `error` terminates execution of the program and returns an appropriate error indication to the OS. It should also display the user-supplied string in some system-dependent manner.

When `undefined` is used, the error message is created by the compiler.

Translations of Haskell expressions use `error` and `undefined` to explicitly indicate where execution time errors may occur. The actual program behavior when an error occurs is up to the implementation. The messages passed to the error function in these translations are only suggestions; implementations may choose to display more or less information when an error occurs.
