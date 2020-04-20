# Monad

A Monad wraps a computation (value, function) in a certain context (structure, contaner, box). A monad must define both, a means of wrapping a computation in a context, and a way of combining computations that are inside a context. As a difference from functors and applicatives, monads also automatically join (flatten) nested contexts.

In Haskell, monad is a typeclass, hierarchically above the Functor and Applicative classes and it requires that a type is an instance of both before becoming a monadic instance: Functor ⊆ Applicative ⊆ Monad.

The monadic context may be a data structure (tuple, list) or ADTs (like `Maybe`, `Either`), functions (functions are instances of the Monad class), a computation such as effectful `IO` type.
