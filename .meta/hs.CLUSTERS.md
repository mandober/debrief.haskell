# Haskell CLUSTERS




## Higher rank and higher order
  - zeroth-order logic
  - first-order logic (FOL)
  - second-order logic (HOL)
  - higher-order function (HOF)
  - higher-order type
  - higher-order kind
  - higher-rank (polymorphic functions)
  - higher-rank type
  - higher-kinded type, higher-kind type

## Computation levels
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

## Haskell citizens
(entities, elements and constructs)

Language entities
- first-class entities
- second-class entities
  - term-level: values, expressions, functions
  - data type declaration
  - definition
  - type signature
  - class declaration
  - instance declaration
  - deriving-clause
  - where-clause
  - hiding-clause
  - as-clause (import)

## General notions
  - purity
  - memoization
  - side-effect and main-effect of functions
  - garbage-collection
  - Core lang is System F based
  - desugaring
  - type system Hindley-Milner

## Tools
  - GHC
  - GHCi
  - Cabal
  - Hackage
  - Stack
  - Stackage
  - ghcup
  - haddok

## Recursion schemes
  - recursion patterns
  - recursive data type
  - catamorphism
  - anamorphism
  - hylomorphism

## Project organisation
  - project
  - script
  - file
  - module
  - package
  - builtin constructs
  - library functions

## Syntax
  - expressions
  - comments
  - scope
  - scoping
  - top level scope
  - symbols
  - operators
  - token
  - keywords
  - reserved symbols and tokens
  - expressions
  - scope

## Declarations
  - module declaration
  - type declaration
  - class declaration
  - instance declaration
  - function declaration
  - top level declaration

## Language constructs
  - equations
  - pattern matching
  - binding
  - let bindings
  - let-in binding
  - let binding in monadic context
  - term
  - type
  - kind

## Type system
  - assurance weakening (variants, dynamics)
  - assurance strengthening (phantom types)
  - data types a la carte
  - polymorphic data type
  - polymorphically recursive datatype

## Types: basics
  - cardinality
  - type habitation
  - bottom
  - type and set isomorphism
  - type scoping
  - type applications
  - type constraint
  - ambiguous type
  - concrete
  - flat
  - ground
  - un/lifted
  - un/boxed
  - type invariant
  - static invariant
  - static restriction
  - abstract data type, ADT

## Types: algebraic
  - sum type
  - product type
  - canonical representations
  - exponential type
  - dependent types
  - GADTs
  - the curry-howard isomorphism

## Kind system
  - kind
  - kinds
  - arrow kind
  - constraint kind
  - data kind
  - promotion of built-in types

## Type-level functions
  - symbol (strings at the type level)
  - natural numbers at the type level
  - type-level computation
  - type-level programming

## Existential types
  - heterogeneous list
  - rank-n type
  - rank
  - existential types
  - existential type eliminator
  - dynamic type
  - generalized constraint kinded existential
  - scoping information with existential

## Role system
  - roles and type coercions
  - 3 types of roles of type parameters
    - nominal
    - representational
    - phantom
  - phantom type
  - Proxy type

## Continuations
  - continuation
  - CPS
  - call/cc
  - continuation monad, Cont, ContT
  - delimited continuations with shift and reset

## Curry-Howard correspondence
  - programs-as-proofs
  - functions as theorems
  - code as proof
  - automated proof
  - theorem provers
  - Djinn

## Type-level computation
  - associated type family
  - type schema
  - associated term
  - first class family

## Expressionability
  - defunctionalization
  - type-level defunctionalization
  - extensible data
  - expression problem (Wadler)

## GHC Extensions
  - overloaded label

## Error handling
  - exceptions
  - ExceptT
  - ErrorT (deprecated)
  - type error
  - custom type error

## Generics
  - generics
  - generic representation
  - generic metadata
  - structured logging

## Advanced TYPES
  - open sum
  - open product

## Polymorphism
  - polymorphism
  - structural polymorphism
  - deriving structural polymorphism
  - ad-hoc polymorphism
  - parametric polymorphism
  - parametricity
  - "Theorems for free"

## Semantics
  - semantics
  - non-strict semantics
  - laziness
  - equational reasoning
  - referential transparency
  - substitution
  - evaluation
  - operational semantics
    - imperative programming
    - turing machine
  - denotational semantics
    - functional programming
    - lambda calculus
    - SKI calculus
    - primitive resursive functions, PRF
  - axiomatic semantics

## Monads: advanced
  - indexed monads
  - monads:
    - Kleisli categories
    - monadic computation
    - strong and commutative monads
    - monadic semantics
  - combining monads:
    - monads from adjunctions
    - distributive laws
    - coproduct of monads
  - Finer and coarser:
    - Lawvere theories
    - arrows
    - Freyd categories
  - Comonadic notions of computation:
    - comonads
    - coKleisli categories
    - comonadic computation
    - dataflow computation
    - lax/strong symmetric monoidal comonads
    - comonadic semantics

## Linear types
  - linear types
  - linearity
  - linear logic
  - linear allocation
  - lollipop arrow, `-o` ≡ `%1 ->`
  - generalized arrow, `%m ->`
  - generalizing over linearity

## Dependent types
  - dependent types
  - singletons package (faking dependent types)
  - dependent pair

## Category theory
  - category theory
  - initial encoding
  - Yoneda lemma
  - codensity monad
  - representables functors
  - data and codata
  - kan extensions
  - functors
  - natural transformations
  - adjunctions
  - symmetric monoidal (closed) categories
  - Cartesian (closed) categories
  - coproducts
  - initial algebra
  - final coalgebra of a functor

## Variance
  * Variance (types)
    - variance
    - invariance
    - nonvariance
    - covariance
    - contravariance
  - canonical type representations
  - positive and negative position

## Lambda calculus
  - lambda calculus
  - Simply-typed lambda calculus
  - System F
  - Church encoding
  - Scott encoding

## Abstract algebra
  - abstract algebra
  - algebraic structures: elements
    - carrier (underlying) set
    - binary operations
    - algebraic axioms
  - algebras
    - magma
    - semigroup
    - monoid
    - group
    - abelian group

## Math background
  - mathematical model of computation
