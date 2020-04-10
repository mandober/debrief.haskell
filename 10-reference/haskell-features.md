# Haskell Features

Haskell concepts and features:
- functional PL
- purely functional PL
- declarative PL
- garbage collected memory management
- lazy evaluation discipline
- non-strict evaluation strategy
- call by need
- Lambda Calculus as the Core IL
- lexical scope (optionally dynamic scoping)
- immutable values
- referential transparency
- Hindley-Milner type system (extended)
- strong typing
- static typing
- implicit typing
- complete type inference
- structural type compatibility
- type safety
- types as first-class values (GHC extension)
- type classes
- type-safe operator overloading with type classes
- type inference
- type parameters
- type annotations
- type signatures
- partial type signature
- typed hole
- type declaration
- type constructors
- data constructors
- type instance
- orphaned instance
- type polymorphism
- parametric polymorphism (parametrically polymorphic)
- levity polymorphism
- kind-polymorphic
- type kinds
- higher-rank type
- functional dependencies
- type families (GHC extension)
- type promotion (GHC extension)

- first-class functions
- lambda expressions
- let expressions
- where expressions
- operators are functions
- infix/prefix functions
- infix/prefix operators
- fixity
- symbols can be identifiers for operators
- algebraic data types
- sum types
- product types
- list
- list comprehension
- tuples
- strings are char lists
- generalized algebraic data types (GHC extension)
- modules
- namespaces
- pattern matching
- monads model error handling, nondeterminism, parsing, effects, computations
- GHC compiler
- GHCi interactive interpreter
- multicore parallelism
- literate programming




* *Haskell* is a polymorphically statically typed, lazy, purely functional programming language based on Lambda Calculus.
* *Haskell Brooks Curry*. Haskell is named after mathematician H.B.Curry, who made important contributions to mathematical logic that were used as the basis for functional programming.
* *Lambda Calculus*, invented by Alonzo Church is at the core of Haskell. Even though Haskell offers vast number of data types, constructs and expressions, all of that syntax still translates to the very small Core language which is the Lambda Calculus with additions of some typing rules.
* *Functional programming language*. Everything is done through functions. Functions are first-class data values and the compiler checks the types of functions just like those of any other data. Functions enjoy automatic currying support.
* *Purely functional language*. Functions have no side effects.
* *Purity*. Pure functions can return side effects that are subsequently executed. Haskell can model impure functions of other languages.
* *Function-level operators*. Symbolic operators are functions.
* *Polymorphic functions* that provide a single operation on different types.
* *Type classes*, originated in Haskell, declare a set of functions to be implemented by belonging type. They enable type-safe operator overloading.
* *Fixity* determines direction of associativity and precedence of ops.
* *Side effects*. Distinct construct represents side effects, orthogonal to the type of functions.
* *Type system*. Haskell has a strong, static type system based on *Hindley–Milner type inference*. Its principal innovation in this area is *type classes*, originally conceived as a principled way to add overloading to the language, but since finding many more uses.
* *Monads* are a general framework that can model different kinds of computation, including error handling, nondeterminism, parsing, software transactional memory. The construct that represents side effects is an example of a monad. However, Monads are defined as ordinary datatypes.
* *Specification*. Haskell has an open, published specification. Main implementation is GHC, both an interpreter and native-code compiler.
* *GHC*. Rich type system incorporating recent innovations such as *generalized algebraic data types* and *type families*. It has high-performance concurrency and parallelism implementation.
* *Community*. Active, growing community, vast number of third-party open-source libraries and tools in the main *package repository*, Hackage. Haskell is used in academia and industry. As of September 2019, Haskell was the 23rd most popular programming language in terms of Google searches for tutorials and made up less than 1% of active users on the GitHub source code repository.
* *Source files*. Filename ext: .hs (standard) and .lhs (literate Haskell)
* *Mutability*. Haskell maintains *referential transparency*, there is no mutation - once initialized a variable keeps its value.
* *Laziness*. Nothing is evaluated until really needed. Arguments are not evaluated on application, only when/if they are actually used. Haskell thus supports infinite types and data types.
* *Compiled and interpreted*. Haskell's de facto implementation is GHC, which is a compiler but can also act as an interpreter for interaction with Haskell.
* *Static typing*. Type-checking is performed at compile time.
* *Type inference*. Type annotations are optional as the compiler has full type-inference capability, courtesy of Hindley-Miner typing system.
* *Type safety*. The compiler performs strict type checking, disallowing operations if the types are not compatible. Moreover, this strictness means that implicit type casts and type coercions are either disallowed or reduced to a minimal set of fully documented, well-known and largely expected exemptions.
* *Pattern matching*. Pattern matching provides a means of selecting different computations depending on the structure of the data. It is succinct with a minimum of ceremony.
* *Category theory*. Haskell is heavily influenced by mathematics, especially *abstract algebra* and category theory. Monads are used as means to abstract a pattern of computation.
* *Statically typed*: the type of every subexpression is inferred and checked at compile time.
* *Polymorphic*: functions and data constructors can be defined abstracting over argument type. 
