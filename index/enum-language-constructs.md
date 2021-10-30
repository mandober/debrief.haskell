# List of language constructs

* Language constructs
  * TERMS
    - terms are the entities
      - at the term-level
      - that remain at RT
      - that are first-class
      - that can be passed around
      - that can be compared (except functions, computations, and similar)
      - that can be printed (except functions, computations, and similar)
    - Is there a special relation between terms and types and kinds?
      - terms have types that are of the `Type` kind (most of the time)
      - term-level functions have the saturated kind `Type`
      - unsaturated kinds, `Type -> … -> Type`, are type-level functions
      - their types are saturated, `Type`, most of the time
      - only types of the `Type` kind classify the terms
      - only types of the `Type` kind are inhabited
      - uninhabited types may have the kind `Type` or some other
    - terms include
      - literals
      - values of primitive types
      - constant values/expressions
      - scalars
      - functions
      - data constructor
  * TYPES
      - type constructors
      - the most general Haskell type is unconstrained type var, `a`
        - it is unconstrained
        - it can be instantiated at any type (including something like `a -> b`)
        - it represents the set of all Haskell types
      - class is a subset of all types
      - `v` ⊆ `f` ⊆ `Integral α` ⊆ `Num α` ⊆ `Show α` ⊆ `α`


  * KINDS



* Haskell program
  - package
  - module
    - exporting
      - reexporting
    - importing
    - qualification
      - qualified names
      - alias
      - hiding entities
  - Datatypes
    - ADT datatype creation
      - data type, `data`
      - newtype wrapper, `newtype`
      - type synonym, `type`
    - GADT datatype creation
      - data type, `data`


* Levels
  - module-level: declarations
  - Semantic levels
    - term-level: expressions are values of static types
    - type-level
    - kind-level
  * Term-level entities
    - values of primitive types
      - Int, Char, Float, Double
    - literal values
      - constant values
      - polymorphic literals
        - `Num a`
        - `Floating a`
        - `Integral a`
    - term expression
    - data constructors
    - Can we say that terms are the `Type` kinded type of things?
      - What about function types and the kind, `Type -> Type`
      - If not, can we at least say that terms are those entities whose types
        have the kind that ends in `Type`? Like some of these:
        + Type
        + Type -> Type
        + κ -> Type
        + κ -> Type
        - κ
        - κ -> κ
