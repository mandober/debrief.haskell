# Deriving Reader, Writer and State from the first principles

- [Purity and side effects](./purity-and-side-effects.md)
- [Kleisli arrows](./kleisli-arrows.md)
- [Reader](./reader.md)
- [State](./state.md)
- [Writer](./writer.md)
- [RWS](./rws.md)

# Haskell :: Assorted Topics :: RWS

reader-writer-state-from-first-principles

- Pure functions
- Side-effects
- Modeling side-effects with pure functions
- Observable side-effects
- Software Transactional Memory (STM)
- Mutation with
  - `IORef`
  - `MVar`
  - `STM`


Software Transactional Memory: a modular composable concurrency abstraction

See:
* `Composable memory transactions`, by Tim Harris, Simon Marlow, Simon Peyton Jones, and Maurice Herlihy, 2005, in ACM Conference on Principles and Practice of Parallel Programming
https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/

This module, `Control.Monad.STM` only defines the STM monad; you probably want to import `Control.Concurrent.STM` (which exports 
`Control.Monad.STM`).

Note that *invariant checking* (namely the `always` and `alwaysSucceeds` functions) has been removed. See ticket [#14324] and the removal proposal. Existing users are encouraged to encapsulate their STM operations in safe abstractions which can perform the invariant checking without help from the RTS (runtime system).
