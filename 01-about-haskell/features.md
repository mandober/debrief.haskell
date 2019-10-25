# Haskell Features


* Function-level operators for composing functions. These operators allow the programmer to cleanly glue together functions within a common data path.


* Polymorphic functions. These are functions that provide a single operation on data of different types, such as lists of integers and lists of strings.

* Type classes. A type class provides a set of functions that can be defined differently for different data types belonging to that type class. For example, the function that determines whether one value is less than another must use a
different method for two integers than it does for two strings. The nature and use of all of these features will become clear as you proceed through this book. But before turning to that, this introduction concludes with a brief istory of functional languages and functional programming.


Haskell features:
- lazy evaluation
- lambda expressions
- pattern matching
- list comprehension
- type classes
- type polymorphism

It is a purely functional language, which means that functions generally have no side effects.

A distinct construct exists to represent side effects, orthogonal to the type of functions.

A pure function can return a side effect that is subsequently executed, modeling the impure functions of other languages.


Haskell has a strong, static type system based on Hindley–Milner type inference. Its principal innovation in this area is *type classes*, originally conceived as a principled way to add overloading to the language, but since finding many more uses.

The construct that represents side effects is an example of a monad. *Monads* are a general framework that can model different kinds of computation, including error handling, nondeterminism, parsing and software transactional memory. Monads are defined as ordinary datatypes, but Haskell provides some syntactic sugar for their use.


Haskell has an open, published specification, and multiple implementations exist. Its main implementation, the Glasgow Haskell Compiler (GHC), is both an interpreter and native-code compiler that runs on most platforms. GHC is noted for its rich type system incorporating recent innovations such as generalized algebraic data types and type families. The Computer Language Benchmarks Game also highlights its high-performance implementation of concurrency and parallelism.

An active, growing community exists around the language, and more than 5,400 third-party open-source libraries and tools are available in the online package repository Hackage.
