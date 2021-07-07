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
