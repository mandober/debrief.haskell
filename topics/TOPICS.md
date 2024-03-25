# Haskell concepts

- purely functional languages
  - purity
  - Laziness
  - Benefits of laziness
  - Computation, mutability, side effects and purity
  - modeling "internal" side effects:
  - monad precursor types
  - Reader, Writer, State
  - monad transformers
  - transformers style
  - mtl style
  - other effects systems
- Type system
  - Abstraction opportunities
  - values, types, kinds
  - functions on values/types/kinds
  - primitive, base, flat, (un)boxed, (un)lifted types, lib types
  - Phantom types
  - Existential types
  - injectivity, generativity, matchability of type ctors
  - dependent types
    - faking DT with singletons
    - natural numbers at type level
    - vectors
  - open-world vs close-world types
  - type classes, type class derivation, standalone class derivation
  - type families, data families, fundeps
  - higher-kinded types
  - rank-N types
  - code synthesis, generating code from types
  - typed holes
  - type-level programming
    - higher-order type-level programming
    - functions from {terms,types} to {terms,types}
  - styles of programming
    - declaration vs expression style
    - pointfree vs pointfull style
    - direct vs continuation-passing style
- Algebraic data types
  - Algebra of data types
  - Products, coproducts, exponential types
  - GADTs
- Effects systems
  - Effects tracking
  - algebraic effects
  - free monads
  - freer
  - hierarchical free monads
- Software design patterns
  - Final(ly) tagless
  - Handler pattern
  - ReaderT pattern
- Curry-Howard isomorhism, BHK isomorhism,
  - propositions as types, programs as proofs
  - normalization of proofs ≅ evaluation of programs
  - natural deduction ≅ typed lambda calculus
  - type schemes ≅ HM type system
  - System F ≅ Polymorphic LC
  - modal logic ≅ monads (state, exceptinos)
  - classical-intuitionistic embedding ≅ CPS
  - linear logic ≅ session types
  - Code synthesis, generating code from types
  - typed holes
- Category theory in/and Haskell
  - Haskell and category theory
  - monads in functional programming
  - Monoids
  - Semigroups
  - Functors
    - contravariant functors
    - bifunctors
    - profunctors
    - representable functors
    - forgetful functors
    - applicative functors
    - adjunctions
  - Monads
  - NTs
  - F-algebras
  - Optics
    - Lenses
    - Folds
    - Traversals
    - Prisms
- Recursion
  - recursion
  - tail call, TCO
  - fixpoint
  - iteration
  - universality and expressiveness of fold
  - primitive rec
  - Recursion vs corecursion
    - induction vs coinduction
    - data vs codata
    - Recursion schemes
      - cata
      - ana
- Continuations
  - CPS vs direct style
  - classical-intuitionistic embedding ≅ CPS
    - CPS corresponds to embedding CL into IL
    - (a -> r) -> r ≅ LEM, False := a -> ⟘
  - tail calls
  - continuations
  - continuation monad, Cont
  - continuation monad transformer, ContT
  - callCC
- Equality
  - type equality, (`~`)
  - type conversion
  - type coercion
  - safe type coercions, Coercible class, isomorhism
  - heterogeneous type equality, (`~~`)
  - equality of functions
  - intensinal vs extensinal equality
  - propositional equality
  - definitinal equality
  - equality, equivalence, isomorhism, embedding, one-sided isomorhism
- Generic programming
  - type ctors and data ctors follow the same form (set of fields)
  - SYB (scrap your boilrplate)
  - SoP (sum od products)
- Unsorted topics
  - variance, positive and negative positions of type params
- functions as data
- GHC
  - compilation, desugaring, Core, C--, STG machine
  - GHC plugins
- equational reasoning, bottom, ref transp, fast n' loose reasoning
- (in)determinancy, amb
- coherency laws and properties (of type classes at least)
- constraints solving
- type unification
- Free monad
- comonads
- STM, STM monad, ST monad
- concurrency, parallelism
- fusion, deforestation
- Data.Functor vs Control.Functor
- Quantification, type params
- Quick Check prop testing
- Arrays, vectors, truly mutable types
- reflection
- session types
- quotient types
- liquid types, SAT resolvers
- linear types
- refinement types
- Distribution monad
- Probability monad
- Strict Haskell, space/memory leaks, laziness, bang patterns
- Async, Async exceptions, concurrency, theading
- overlapping or incoherent instances of classes











## Incoherent instances of classes

`f` is the identity for all types except integers:

```hs
class Test a where
  f :: a -> a
instance Test a where
  f = id
instance Test Int where
  f x = x + 1
```
