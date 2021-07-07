# Haskell Topics


* Semantics
  - non-strict semantics
  - evaluation
  - evaluation strategy
  - laziness
  - call-by-need
  - call-by-sharing
  - strictness in Haskell
  - strictness flag
  - bang pattern



* Haskell notions and elements
  - value, data, value attributes
  - datatype, type
  - Datatype taxonomy
  - classification of types
  - Type dimensions, dimensions of type categorization
  - Factors of type classification
    - Type properties
    - Primitive types
    - Compound types, Aggregate types
    - Builtin types, core types, library (redifanable) types
    - user types, custom types, mechanisms of custom type construction
    - Boxed and unboxed types
    - Lifted and unlifted types

* Language elements (concepts, entities, items)
  - syntax
  - semantics
  - type system
  - standard library
  - run-time system
  - Index of
    - language entities
    - language constructs
    - modules
    - base modules
    - prelude contents

* Data types
  - Type declaration
  - [Type constructor
  - Data constructor
  - Bottom
  - Actions
  - Arrows
  - type class hierarchy

* Type system
  - [Parametricity](./parametricity.md)
  - [Quantifiers](./quantifiers.md)
  - [Type defaulting](./type-defaulting.md)
  - [Type equality](_topics-all/type-equality.md)
  - [Type role](./type-role.md)
  - [Type functions](./type-functions.md)
  - [Heterogeneous lists](./heterogeneous-lists.md)

* Type classes
  - [Type class](_topics-all/type-class.md)
  - [Type class declaration](./class-declaration.md)
  - [Standalone deriving](./standalone-deriving.md)

* Recursion schemes
  - [Recursion schemes](./recursion-schemes.md)
  - [List origami](./list-origami.md)
  - Tying the knot


* Semantics
  - [Laziness](./laziness.md)
  - [Strict Haskell](./strict-haskell.md)
  - [seq](./seq.md)

* GHC Extensions
  - [Rank-n-types](./rank-n-types.md)
  - [Type promotion](./type-promotion.md)
  - [Type family](./type-family.md)
  - [Functional dependency](./functional-dependency.md)
  - [View Patterns](_topics-all/view-patterns.md)
  - [Template Haskell](./template-haskell.md)

* GHC
  - [Core](./core.md)
  - [Desugaring](./desugaring-haskell.md)

* Theory and Mathematical background
  - [Denotational semantics](evaluation/denotational-semantics.md)
  - [Algebra of types](./algebra-of-types.md)
  - [Category theory](./category-theory.md)
  - [Hask category](./hask-category.md)

* Haskell libraries/packages
  - [QuickCheck](./quickcheck.md)

* Haskell specs and standards
  - Haskell 98
  - Haskell 2010
  - Haskell 2020

* Performance
  - [Performance](_topics-all/performance.md)
  - [Graph reduction](_topics-all/graph-reduction.md)
  - Profiling
  - Algorithmic complexity
  - Concurrency
  - Parallelism
  * Constructs:
    - Data Types
      - Integers
      - Floating points
      - Strings
      - Functions
    - Data Structures
      - Arrays
    - Overloading
    - FFI
    - I/O
    - Modules
    - Monads
  * Techniques:
    - Strictness
    - Laziness
    - Avoiding space leaks
    - Accumulator (accumulating parameter)
