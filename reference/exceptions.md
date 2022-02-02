# Exceptions

https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Exception.html

This module provides support for raising and catching both built-in and user-defined exceptions.

Exceptions may be thrown from:
- exceptions thrown by `IO` operations
- *imprecise exceptions*: exceptions thrown by pure code
- *asynchronous exceptions*: exceptions thrown by external events

However, exceptions may only be caught in the `IO` monad.

For more details, see these papers:
- `A semantics for imprecise exceptions`, Simon Peyton Jones, Alastair Reid, Tony Hoare, Simon Marlow, Fergus Henderson, 1999
- `Asynchronous exceptions in Haskell`, Simon Marlow, Simon Peyton Jones, Andy Moran, John Reppy, 2001
- `An Extensible Dynamically-Typed Hierarchy of Exceptions`, Simon Marlow, 2006
