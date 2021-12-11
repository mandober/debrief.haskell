# Haskell Topics: Idioms


* Haskell idioms
  - nested, helper, function
  - worker/wrapper transformation
  - CPS
  - tail recursion
  - recursion schemes
  - catamorphism
  - anamorphism

* PL concepts
  - actions
  - continuations
  - exceptions


* Theorems
  - Theorems for free
  - Fast and loose reasoning
  - Hask category
  - Referential transparency

* Computation levels
  - levels: term, type, kind
  - Syntactic levels, syntactic categories, computational levels
    - term-level, value expressions
    - type-level, type expressions
    - kind-level, kind expressions
  - all levels support polymorphism
  - and higher-order entities HOF, HKT, HRT, PolyKinds
  - Some PLs have unbounded number of computational levels:
    values, types, kinds, sorts, etc.
  - Some have an infinite type hierarchy:
    type0, type1, type2, etc.
    with each higher sort available as is needed

* Tools
  - GHC
  - GHCi
  - Cabal
  - Hackage
  - Stack
  - Stackage
  - ghcup
  - haddok

* Recursion schemes
  - recursion patterns
  - recursive data type
  - catamorphism
  - anamorphism
  - hylomorphism


* Type-level functions
  - symbol (strings at the type level)
  - natural numbers at the type level
  - type-level computation
  - type-level programming

* Existential types
  - heterogeneous list
  - rank-n type
  - rank
  - existential types
  - existential type eliminator
  - dynamic type
  - generalized constraint kinded existential
  - scoping information with existential

* Role system
  - roles and type coercions
  - 3 types of roles of type parameters
    - nominal
    - representational
    - phantom
  - phantom type
  - Proxy type

* Continuations
  - continuation
  - CPS
  - call/cc
  - continuation monad, Cont, ContT
  - delimited continuations with shift and reset

* Error handling
  - exceptions
  - ExceptT
  - ErrorT (deprecated)
  - type error
  - custom type error

* Generics
  - generics
  - generic representation
  - generic metadata
  - structured logging
