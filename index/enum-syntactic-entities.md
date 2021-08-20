# List of syntactic entities

* Program structure
  - abstract syntactic structure
    - syntactic structure (levels)
      - topmost-level: modules
      - module-level : declarations
      - value-level  : expressions are values of static types
      - bottom level : lexical structure, concrete representation
  - semantic structure

* Syntactical representation
  - abstract syntactic representation
  - concrete syntactic representation
  - surface syntax, possibly sugared
  - core syntax: The Haskell Core language (enhanced System F)

* Semantic levels
  - term level values
  - type-level values
  - kind-level values

* Term-level values
  - expression
  - literal
  - data constructor

* Type-level values
  - type constructor
  - type variable
  - type class
    - type class declaration
    - type class instances
  - type family
    - by type ctor used
      - data type family
        - `data`, `newtype`
      - `type` synonym
    - by association
      - top-level type family
      - associated with a type class
    - by openness
      - open type family
      - closed type family
  - type equality
    - heterogenous type equality (wrt kind)
  - roles
    - nominal
    - representational
    - phantom

* Kind-level values
  - kind
  - kind variable
  - promoted data constructors
  - kind polymorphism


Terms
- value
- data field
- accessor function
- data ctor

Types
- type name
- type ctor
  - bare type ctor, `Either`
  - partially applied type ctor, `Either a`
  - type ctors have kind
  - saturated type ctor: fully-applied type ctor
  - unsaturated type ctor: partially applied type ctor
  - type ctors (un/saturated) are made instances of classes
    - `Either` in case of `class (f :: * -> * -> *)`
    - `Either a` in case of `class (f :: * -> *)`
    - `Either a b` in case of `class (f :: *)`
- type parameter
  - phantom
  - universally quantified
    - Rank-0, `Int -> Int`
    - Rank-1, `∀a. a -> a`
    - Rank-2, `(∀a. a -> Int) -> Int`
    - Rank-N
  - existentially quantified
- data ctor
  - bare data ctor
  - partially applied data ctor
- data fields
- data field accessors
- higher kinded type
- higher rank type
- polymorphic type
  - Rank-1
  - Rank-2
  - Rank-N

Kinds:
- base, `*`
- arrow kinds, `* -> *`
- higher kinded types: `k -> k` where `k` is a kind
- type ctors have kind


* each term (expression, value) has a certain type, and each type has a kind.
* more precisely, each type ctor has a kind
* type ctor kind varies with type ctor saturation
* kind may begin as an arrow kind, e.g. `* -> * -> *`, and as it gets saturated, it becomes the fully saturated base kind, i.e. `*`
- can ask for a kind of `Maybe`, it is `* -> *`
- can ask for a kind of `Maybe Int`, it is `*`
- cannot aks for a kind of `Maybe a`


Hierarchy
- data term
- data ctors
- type
  - type name
  - type ctor
  - data ctor(s)
- kind
  - lifted type ctor to kind
  - lifted data ctor to type ctor


These entities include:
- value
  - literal value
  - expression
- type, data type
  - type signature
  - type annotation
- function
  - (functions only - there are no variables)
  - type signature
  - function declaration
  - function definition
  - set of equations
- kind
- expression
- declaration
- statement (?)
- module
- export declaration
- import declaration
- type class declaration
- type class definition
  - function declaration (method)
  - function definition (default method)
- type class instance
- pattern
- package
- type declaration
  - type constructor
  - data constructor
  - type variable
