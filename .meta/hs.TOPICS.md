# Haskell TOPICS


* Code locations
  - top-level
  - pattern
  - constraint context
  - function
  - let-expression
  - where-expression
  - lambda function

* Variables
  - term vars
    - term-level vars range over all lang terms/expr
    - but are restricted with, at least, their type
  - type vars
  - kind vars
  - var as irrefutable pattern
  - var as part of a pattern

* Patterns
  - pattern matching
  - as pattern, `@(..)`
  - ignore pattern, `_`
  - view pattern
  - pattern synonym



* Data types
  - type
  - inferred type
  - parametric types
  - parameterized types
  - skolem type
  - ∀ type
  - ∃ type
  - base types
  - compound type
  - type signature
  - type annotation
  - type application, `TypeApplications`
  - abstract data type

* Advanced Types
  * type declarations
    - data declaration
      - sum
      - product
    - newtype declaration
    - type synonym declaration
  - type class
  - type function
  - type holes


* Kinds
  - kind
  - kind annotation
  - standalone kind signature, `StandaloneKindSignatures`

* Type families
  - indexed type families, type families
  - type-level programming
  - ad-hoc overloading of data types
  - associated data types
  - associated type synonyms
  - lang ext `TypeFamilies`
  - indexed type family
  - indexed data family
  * Type family flavors
    - data families
    - type synonym families
  * Type family openness
    - open type family
    - closed type family
  * Type family association
    - standalone, top-level declaration
    - associated with a type class


* Type Family Topics
  - named functions on types
  - type-level functions
  - associated data types
  - associated type synonyms
  - type-level programming
  - indexed type family
  - indexed data family
  - ad-hoc overloading of data types
  - parametric types w specialized representations
  - type families are data type analogue of type classes
  - type families overload data
  - type families:
    - standalone or associated
    - open or closed
  - type-indexed data types
  - let generalisation
  - type constructor flavours
  - closed type families
  - type constructor arity
  - the synergy with gadts
  - evaluation order, or lack thereof
  - open type families
  - overlapping equations
  - compatible equations
  - injective type families
  - associated types
  - data families
  - non-parametric quantification
  - non-linear patterns

* Syntax
  - keywords
  - namespaces
  - modules

* Syntactic elements
  - module declaration
    - export clause
    - qualified exports
  - import clause
    - qualified imports
    - as import
  - data declaration
    - data declaration
    - newtype declaration
    - type declaration
    - deriving clause
  - let-binding
    - let rec
  - where clause
  - functions
    - main function
    - named functions
    - lambdas
    - sections
  - type class
* Language entities
  - expression
    - value
    - literal
  - function
- Type declaration
- Type constructor
- Type parameter
* Type classes
  - class declaration
  - class instance
* Deriving
  - deriving clause in data declaration
  - standalone deriving
  - deriving strategies
    - stock
    - newtype
    - anytype
* Data types
  - primitives
  - bultins
  - privileged vs unprivileged
  - lifted vs unlifted
  - boxed vs unboxed
  - std
  - user


* Type system
  - System F
  - Hindley-Milner

  * Algebraic data types
    - Algebra of types
    - Recursive data type
    - Inductive data type
    - Sum types
    - Product types
    - Function types
    - Dependent types
    - Quotient types
    - Intersetion of types
  * Type classes
    - Elementary classes
    - Algebraic classes
    - Monadic classes
    - Arrow classes
  * Polymorphism
    * Introduction to polymorphism
      - Monomorphism
      - Polymorphism
      - Constraint
      - Type parameter
      - Type operator
    * Kinds of polymorphism
      - Parametric polymorphism
      - Ad-hoc polymorphism
      - Levity polymorphism
      - Subtyping
      - First-class polymorphism
      - Structural polymorphism
      - Higher-Rank Polymorphism
      - Impredicative polymorphism
      - Let-polymorphism
      - Row polymorphism
      - Static polymorphism
    * Polymorphic functions
      - Type-level functions
        - terms-to-terms (regular)
        - types-to-terms (parametric polymorphism)
        - types-to-types (HKT)
        - terms-to-types (dependent types)
      * Generics
        - Generic function
        - Generic programming
        - Scrap Your Boilerplate (SYB)
    * Concepts
      - Kind
      - Higher-Kinded Types (HKT)
      - Impredicativity
      - Higher-Kinded Types (HKT)
      - Higher-Rank Types (HRT)
      - Polyvariance
      - System F
      - Predicate dispatch
      - Static dispatch
      - Bounded quantification
      - Parametricity
      - Polymorphic recursion
      - Polymorphic type
      - Polymorphic value
      - Polymorphic function
      - Function overloading

* Extensions
  - GADTs
  - RankNTypes
  - FlexibleInstances
  - Linear Haskell
  - Template Haskell
  - Liquid Haskell

* Concepts
  - Actions
  - Continuations
  - Handling errors
  - Performance

* do notation
  - syntax, desugaring
  - monadic do
  - applicative do
  - qualified do
  - recursive do

* Academic
  - Theorems for free
  - Fast and Loose reasoning
