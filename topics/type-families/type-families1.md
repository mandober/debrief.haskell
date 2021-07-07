# Type families

Indexed type families (or just type families) facilitate type-level programming.

Type families essentially provide *type-indexed datatypes* and *named functions on types*, which are useful for generic programming, for creating highly parameterised library interfaces, and for creating interfaces with enhanced static information, much like dependent types.

Type families are the data type analogue of type classes: type families are used to define overloaded data, in the same way that type classes are used to define overloaded functions.

Type families are a generalisation of *associated data types* and *associated type synonyms* from type classes.

Type families may also be regarded as an alternative to *functional dependencies*, they provide a more functional style of type-level programming than the relational style of the latter.


~ ~ ~

Indexed type family is a GHC extension supporting *ad-hoc overloading of data types*. The lang pragma `TypeFamilies` (implies ExplicitNamespaces, KindSignatures, MonoLocalBinds) enables type families and allow the use and definition of *indexed type families* and *indexed data families*.

Type families are parametric types that can be assigned specialized representations based on the type parameters they are instantiated with.

Type families come in two flavors:
* *Data families* are the indexed form of `data` and `newtype` definitions.
* *Type synonym families* are the indexed form of `type` synonyms.


Each of these flavors can be defined in a *standalone* manner or *associated* with a type class. Standalone definitions are more general, while associated types can more clearly express how a type is used and lead to better error messages.

1. What are type families?
2. Requirements to use type families
3. An associated data type example
  3.1 The class declaration
  3.2 An Int instance
  3.3 A unit instance
  3.4 Product and sum instances
  3.5 Using a generic map
  3.6 Download the code
4. Detailed definition of data families
  4.1 Family declarations
    4.1.1 Associated family declarations
  4.2 Instance declarations
    4.2.1 Associated type instances
    4.2.2 Scoping of class parameters
    4.2.3 Type class instances of family instances
    4.2.4 Overlap
  4.3 Import and export
    4.3.1 Associated families
    4.3.2 Examples
    4.3.3 Instances
5. An associated type synonym example
  5.1 The class declaration
  5.2 An instance
  5.3 Using generic collections
6. Detailed definition of type synonym families
  6.1 Family declarations
    6.1.1 Associated family declarations
  6.2 Type instance declarations
    6.2.1 Closed family simplification
    6.2.2 Associated type instances
    6.2.3 Overlap
    6.2.4 Decidability
  6.3 Equality constraints
7. Frequently asked questions
  7.1 Comparing type families and functional dependencies
  7.2 Injectivity, type inference, and ambiguity
8. References


~ ~ ~


## Type Family Topics
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


## Type constructor flavours

A type ctor may belong to these categories:
* data type declaration,  `data T a b = ...`
* newtype  declaration,   `newtype T a b = ...`
* type class declaration, `class T a b where ...`
* type synonym,           `type T a b = ...`

TypeFamilies extension introduces two more:
* type family, `type family T a b where ...`
* data family, `data family T a b = ...`

Type families are further subdivided into
- *open   type families* are top-level (standalone) or associated with a class
- *closed type families* are always associated with a class

Data families are always open and either
- top-level or
- class-bound


## Closed type families















## References

Type families in Haskell Wiki
https://wiki.haskell.org/GHC/Type_families

Type families in GHC Manual
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html

Type Families: The Definitive Guide - Vladislav Zavialov, Apr 2021
https://serokell.io/blog/type-families-haskell

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#assocdatatypes2005

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#assoctypesyn2005


https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#typefamilies2008
