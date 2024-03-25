# Introduction to deriving

- datatype declaration
- class declaration
- minimal set of methods
- instance declaration
- class-type pair
- deriving clause
- derived instance
- deriving mechanism


To make your datatype, `T`, a member of the class, `C`, you need to implement all the methods the class `C` prescribes for your type, in a syntactic block called instance declaration.

An *instance declaration* is an assertion that the specified data type is applying for membership in the specified class by implementing the *minimal set of methods* the class prescribes.

There are different ways of deriving, but the general idea is described by the Haskell 2010 language report: A *derived instance* is an *instance declaration* that is generated automatically in conjunction with a data or newtype declaration. The body of a derived instance declaration is derived syntactically from the definition of the associated type.

A *deriving mechanism* is the capability of the compiler to automatically derive a requested instance declaration, generating the necessary code.

A *deriving clause* is a feature descibed by all the Haskell standards that allows users to attach a clause to the declaration of a datatype, requesting that the compiler automatically derives one or more instances of the standard classes for the enclosing datatype.

In Haskell2010, *the set of derivable standard classes* is limited to `Eq`, `Ord`, `Enum`, `Bounded`, `Ix`, `Read`, `Show`, with a set of conditions that restrict which of these classes are derivable and when. In GHC2021, many more classes are auto-derivable, but also additional deriving mechanisms are introduced, making it possible to derive just about any class.

Besides the deriving clause, automatic code generation also happens whenever the user *skips defining all the methods*. A class may declare a number of methods without requiring that each is given a definition. A class can specify the *minimal set of methods* whose definition is critical, leaving it to the compiler to fill in the definitions of those methods the user skipped to define. This is another situation, far less evident, when the code generation is triggered as well.





An instance declaration consists of
  - `instance` keyword
  - optional context, `(…) =>`, e.g. superclass constraints, inductive class
  - class name, e.g. `Ord`
  - receiver type (ctor)
  - `where` keyword
  - followed by definitions:
    - at least those methods that make the minimal set (as prescribed by class)
    + type family instance(s)
    + method signatures
