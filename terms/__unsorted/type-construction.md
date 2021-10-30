# Type construction summary

How do we go about creating a new type? What type construction facilities are at our disposal?





Means of constructing a new user type:


- type synonyms with `type`
- sum types
  - always `data`
  - type variants
  - type alternations, `|`
  - alternations polymorphism: unite multiple type into one
- product types
  - `data` or `newtype`
  - `newtype`: 1 data ctor with 1 field
  - fields
  - records
  - field accessors
    - risk of partial functions


Newtypes: even though T ≅ S
- allowed    : `Stream a = Cons (a, Stream a)`    (one field)
- disallowed : `Stream a = Cons a (Stream a)`     (two fields)

Type functions:
- type ctors (polyadic)
- `type` synonym
- type family

Overloading
- identifiers: names and symbolic name for operators
- overloading a name across all types: parametric polymorphism
- overloading a name across a set of types: ad hoc polymorphism
  - type classes
  - alternations-based polymorphism (disjoint union of types, i.e. sum types)
- overloading a name across levels: term and type levels
  - term-level names and type-level names live in a diff namespace, so reusing the same name is allowed
