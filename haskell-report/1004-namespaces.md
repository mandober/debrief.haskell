# 1.4 Namespaces

https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4

There are 6 kinds of names in Haskell: those for variables and constructors denote values; those for type variables, type constructors and type classes refer to entities related to the type system; module names refer to modules.

There are two constraints on naming:
- names for (term) variables and type variables are identifiers beginning with a lowercase letter or underscore; the other 4 kinds of names are identifiers beginning with an uppercase letter.
- an identifier must not be used as the name of a type constructor and a class in the same scope.

These are the only constraints, so, e.g., within a single scope, `Int` may simultaneously be the name of a module, a class and a data constructor.

Names in Haskell:
  * term-level values:
    - term variables
    - data constructors
  * type-level values:
    - type variables
    - type constructors
    - type classes
  * module-level values:
    - modules
  * extended names:
    - package name (used for qualifing an imported module, etc.)
    - FFI names
    - type family name
    - data family name
    - named patterns


## Special names

* A name of a data ctor must begin with an uppercase letter. If the data ctor is an operator (so it's name consists of symbols), its name must begin with a colon, `:`.

* Underscore at the begging of a name

The underscore, `_`, is considered as a lowercase letter when it is the first, but not the only, character of an identifier. A variable's name beginning with an undescore will not trigger a warning about unuded variables. If such a name is used as a term-level hole, the compiler will suggest the names that could go in that hole; a single underscore is usually used for this, but this form makes it easier to identify which hole (in case you place several) the compiler is referring to.

* Single underscore

When a name consists of a single underscore, it is context dependent:
- in a term context as a variable name it represents a term-level hole, and the compiler will suggest the variables in the scope that fit it.
- in a type context, it repr a typed hole and the compiler can suggest a type name that fits it, but stil lemitting an error; or the compiler is asked to infer the correct type for that type hole and accept it without issuing an error. The specific mode is set with a flag.
