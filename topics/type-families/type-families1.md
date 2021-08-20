# Type families

* Type families essentially provide *type-indexed datatypes* and *named functions on types*.

* Type families are the data type analogue of type classes: type families are used to define overloaded data, in the same way that type classes are used to define overloaded functions.

* type family : type class = overloaded data : overloaded functions

* Type families subsume *associated data types* and *associated type synonyms* from type classes.

* Type families may also be regarded as an alternative to *functional dependencies*, they provide a more functional style of type-level programming than the relational style of the latter.

* The pragma `TypeFamilies` (implies `ExplicitNamespaces`, `KindSignatures`, `MonoLocalBinds`) enables type families and allow the use and definition of *indexed type families* and *indexed data families*.
