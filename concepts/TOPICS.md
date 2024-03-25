# Haskell :: Concepts :: Topics

## Topics

- [Effectful computation](./effectful-computation/README.md)    
  Topics about modeling internal side effects, i.e. computations revolving around a global value that can be implemented in a pure setting. The three primary examples are identified:
  - global readonly environment (configuration): env (+r), Reader, ReaderT
  - global log, logging and tracing: log (+w), Writer, WriterT
  - global mutable state: state (+rw), State, StateT
  - combination of these three: rws (+rw), RWS, RWST





## All topics

- Haskell and logic
- Haskell and type theory
  - Quantification
    - Universal quantification
    - Universal types
    - Existential quantification
    - Existential types
  - Polymorphism
    - Abstraction
      - abstracting values
      - abstracting types
      - abstracting type ctors
    - Generics, generic types
      - Generic functions
      - Higher-kinded types
    - Parametric polymorphism
      - Parametricity
    - Ad hoc polymorphism
    - Row polymorphism
    - Rank-N polymorphism
      - Rank-N types
- Haskell and category theory
  - Functors in category theory
    - Variance
      - Covariance
      - Contravariance
      - Invariance
  - Functors in Haskell
    - Functor type class
    - Contravariant type class
    - Bifunctor type class
    - Profunctor type class
  - Applicative functors in category theory
  - Applicative functors in Haskell
    - Applicative type class
  - Monads in category theory
    - Monad as a triple
    - Kleisli arrows
  - Monads in Haskell
    - Monad type class
    - MonadPlus type class
    - MonadFail type class
- Type classes
  - The notion of type classes
  - FAM type classes
    - Functor
    - Applicative
    - Monad
  - Foldable
  - Traversable
- Monads
  - Modeling I/O
  - State
  - Reader
  - Writer
  - ST monad (state transformer monad)
  - Monad transformers
- Modeling side effects
  - Modeling mutable state
  - Modeling readonly environment
  - Modeling global variable
