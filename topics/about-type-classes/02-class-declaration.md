# Type classes

https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-620004

## Declarations

According to the Haskell 2010 Report, syntactically, a type class declaration is an expression.

The declarations in the syntactic category *topdecls* (from BNF definition) are only allowed at the top level of a Haskell module, whereas *decls* may be used either at the top level or in nested scopes (with `let` and `where` clauses).

Haskell2010 declarations may be divided into 3 groups:
1. *user-defined datatypes* consist of
  - `type` declarations
  - `newtype` declarations
  - `data` declarations
2. *classes and overloading* consist of
  - `default` declarations
  - `class` declarations
  - `instance` declarations
3. *nested declarations* consist of
  - value bindings (with `let` and `where` clauses)
  - fixity declarations
  - type signatures
4. *extended declarations*
  + `foreign` declarations                  FFI
  + `deriving` declarations (standalone)    StandaloneDeriving
  + kind signatures (standalone),           KindSignatures
  + role annotations (standalone),          RoleAnnotations
  + type family declarations                TypeFamilies
  + data family declarations                TypeFamilies
  + type family instance declarations       TypeFamilies
  + data family instance declarations       TypeFamilies


Haskell has several primitive datatypes that are "built-in" (such as integers and floating-point numbers), but most builtin datatypes are user-definable using normal Haskell syntax with normal data type declarations.

## Overview of type classes

Haskell uses a traditional Hindley-Milner polymorphic type system to provide a *static type semantics*, but the type system is extended with *type classes*, that provide a structured way to introduce *overloading*.



A **class declaration** introduces a new class along with a minimal set of *overloaded operations*, the so-called *methods*, that must be implemented by a datatype that is to be considered an *instance* of that class.

An **instance declaration** declares that a data type is an instance of the specified class by implementing the minimal set of methods prescribed by that class.



operations-called class methods-instantiated on the named type.

(named class)
