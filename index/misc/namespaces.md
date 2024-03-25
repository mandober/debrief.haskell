# Haskell :: Reference :: Namespaces

Haskell has 3 disjoint namespaces.

Named entities:
- package
- module
- type declarations
  - data
  - newtype
  - type
- import/export modifiers
  - `(..)`
  - `hiding`
  - `qualified`
  - `as`
  - `type` (to disambiguate between im/ex-porting a `type` and `data`)
- type class
  - associated type family
  - associated data family
- type variables
- type constructor
- data constructor
- data family
- type family
- value
- function
- variables
- pattern
- pattern synonym


1. Names of values are in the value namespace, ns-values
  - variables           (lowercase)
  - data constructors   (UPPERCASE)
2. entities related to type-system are in types-namespace, ns-types
  - type variables      (lowercase)
  - type constructors   (UPPERCASE)
  - type classes        (UPPERCASE)
3. module names are in module-namespace, ns-modules
  - modules             (UPPERCASE)

package name

Within the same scope, the same name may be used to refer to one thing from each category.

For example, there is a package named `rio`, a module named `RIO` and a data type named `RIO`.

* ns-values  : vars, DataCtors
* ns-types   : typeParams, TypeCtors, TypeClasses
* ns-modules : Modules (packages?)
