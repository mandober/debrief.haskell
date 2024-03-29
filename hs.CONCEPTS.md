# Haskell concepts

- Modules
  - hierarchical modules
  - parameterized modules
  - qualified names of modules
  - prelude module
  - import-export
    - qualified exported named (`type` keyword)
    - re-export entire imported module (`module` keyword)
  - internal modules
  - trustworthy/safe modules

- Syntax
  - unicode syntax
  - rebindable syntax
  - operators
    - value-level operators
      - function as operator
      - data ctor operator
    - type-level operators
      - type operator, type ctor name as operator
      - type synonym name as operator
      - type class name as operator
        - associated type family name as operator
      - type family name as operator
      - data family name as operator
    - allowed naming scheme for identifiers
    - sections
      - operator section
      - right operator section
      - left operator section
    - mixfix
      - prefix
      - infix
      - postfix
  - partial application of
    - functions
      - operators
      - sections
    - type ctors
    - data ctors
    - type classes
    - type family ✘

- Types
  - type definition
  - type declaration
  - type synonyms
    - liberalised type synonyms
    - existential (lhs forall) type in a type synonym (forbidden)
    - partial application of type synonyms (allowed)

- Values
  - NF
  - WHNF
  - thunks
  - redex
  - application
  - abstraction
  - variables
  - evaluation
    - non-strict
    - lazy evaluation
  - reduction
    - normal order
    - leftmost outermost reduction
    - call-by-need
    - graph reduction
  - literals
    - numeric constants
    - char
    - string
    - list
    - pair
    - unit literal
  - lifted vs unlifted types
  - boxed vs unboxed types
  - bottom, ⊥


- Patterns
  - pattern guards
  - irrefutable patterns
  - refutable patterns
  - lazy (lazier) patterns
  - strict patterns
  - bang patterns
    - bang patterns at the module level (top level)
  - as-pattern
  - binding patterns
  - top-level binding pattern
  - ignore pattern
  - nested patterns
  - pattern synonyms
  - record pattern

- Records
  - record notation
  - record constructor
  - record constructor application
  - record update expression
  - record pattern
  - record pun

- Type classes
  - type class declaration
  - type class instance
  - instance declaration
    - type signatures in instance declaration
  - instance head
    - type variables in instance heads
    - type synonyms in instance heads
    - concrete types in instance heads
  - instance context
    - relation between instance head and context
  - Extensible superclasses

- Deriving
  - instance deriving
  - deriving strategies
  - attached deriving declarations
  - stand-alone deriving declarations

- Polymorphism
  - implicit universal quantification
  - explicit universal quantification
  - forall types
    - right hand side of a type definition
    - type signatures
    - arguments of constructor definitions
  - arbitrary-rank polymorphism
  - monomorphism restriction
  - let-bindings
    - let-polymorphism
    - type signature in let-bindings
    - recursion in let-bindings
    - monomorphic let-bindings
    - polymorphic let-bindings
    - top level bindings
    - let generalisation
    - let lifting
    - lambda lifting
    - lambda dropping
  - mutual recursion
  - mutual recursive bindings
  - generalised typing of mutual recursive bindings
  - type signature
  - type annotation

- Haskell theoretical foundations
  - Category theory
    - Hask
  - Curry-Howard-Lambek
  - Lambda calculus
    - Alpha conversion
    - Beta reduction
    - Eta conversion
    - Lambda abstraction
  - Recursion
  - Combinatory logic
  - Chaitin's construction
  - Turing machine
  - Relational algebra


- Unsorted
  - strong static type system
  - Hindley-Milner
  - higher rank types
  - non-strict/lazy evaluation guided by strictness analysis
  - pure functional
  - input/output
    - I/O via monads
    - monadic code
  - mutable values (`Ref`), `ST` encapsulation and escaping
  - runtime system
  - primitive types 
  - foreign function interface
  - concurrency model
    - green threads
    - OS threads
  - fusion
  - folding
  - strictness
  - type classes
    - type class hierarchy
    - Functor-Applicative-Monad Proposal
    - number class hierarchy
    - superclass
    - default implementation
    - context recursion
  - Roles
  - Arrows
  - higher ranked types
  - higher kinded types
  - Hindley-Milner type inference
  - Undecidability of HR type inference
  - general (principal) type, mgu
  - higher order functions
  - instantiated at a specific type
  - ST Monad (rank-2 type)
  - No assignments, no side effects, no flow of control, no null, ...
  - Lazy Functional State Threads
  - state transformer
  - mutable state
  - phantom type
  - observable effects
  action
