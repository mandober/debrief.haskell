# List of namespaces

1. Names of values are in the value namespace, *ns-values*
  - variables           (lowercase)
  - data constructors   (UPPERCASE)
2. entities related to type-system are in `types-namespace`, *ns-types*
  - type variables      (lowercase)
  - type constructors   (UPPERCASE)
  - type classes        (UPPERCASE)
3. module names are in `module-namespace`, *ns-modules*
  - modules             (UPPERCASE)

Within the same scope, the same name may be used to refer to one thing from each category.

* ns-values  : vars, DataCtors
* ns-types   : typeParams, TypeCtors, TypeClasses
* ns-modules : Modules (packages?)
