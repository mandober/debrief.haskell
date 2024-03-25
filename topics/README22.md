# Haskell :: Concepts

- Recursion schemes
  - recursion
  - data (finite)
  - corecursion
  - codata (infinite)
  - catamorphism
  - anamorphism





## More...

- GHC concepts
  - compact regions
- Data structure concepts
  - deforestation
- Data structures
  - Zipper (Huet's Zipper) ✔
  - Zipper monad
- Haskell Concepts
  - all deriving mechnisms
  - TH


## Types and type theory

* synactic realms
  * elementary classification of language items:
    - value (term) level
    - type level
    - kind level
  * classification of language items:
    - package level
    - module level
    - namespace
    - term level (value level)
    - type level
    - kind level
    - class context
      - constraint context
    - type family
    - data family
      - haskell data families
* sorts of types
  - discrete vs concrete
  - flat vs compound
  - boxed vs lifted
  - primitive vs stdlib vs user-defined
  - monomorphic vs polymorphic vs polytypic
* sorts of type variables
  - rigid type variable
  - wobbly type variable
  - ambiguous type variable
  - untouchable type variable
  - parameterized data type
  - type-indexed data type
  - uninhabited (empty) data type
  - inhabited data type
* data type
  - Algebraic types
    - 𝟘-type (empty type), 𝟙-type (unit type), 𝟚-type
    - coproduct (sum) types
    - product types
    - exponential types
    - types corresponding to a derivative
      - types with holes (and the surrounding context)
    - typed hole (type-level hole)
    - hole (value-level hole)
  - ADT
  - GADT
* type ctors
  - type-parameterized data type (type ctor)
  - type-indexed data type (type ctor)
  - saturated type ctor
    - partially applied type ctor
  - recursive data type
    - polymorphically recursive data type
* data ctors
  - nullary data ctor
  - type-parameterized data ctor
  - type-indexed data ctor
  - saturated data ctor
    - partially applied data ctor
* more type classification factors
  - by arity
  - by kind
  - by cardinality (number of inhabitants)
    - 0-type, 1-type, 2-type
