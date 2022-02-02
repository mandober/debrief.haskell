# The Haskell 2010 Language

https://www.haskell.org/onlinereport/haskell2010/


Haskell 2010 Specs

* Introduction
  - features
  - implementations (GHC)
  - theoretical background
  - category theory
* Syntax
  - Notational Conventions
  - Lexical Syntax
  - Layout
  - Literate comments
  - Context-Free Syntax
  - Fixity Resolution
  * Lexical structure
    - The Haskell kernel
    - program structure
    - values and types
    - namespaces
    - Notational Conventions
    - Lexical Program Structure
    - Comments
    - Identifiers and Operators
    - Numeric Literals
    - Character and String Literals
    - Layout
  * Expressions
    - Errors
    - Variables, Constructors, Operators, and Literals
    - Curried Applications and Lambda Abstractions
    - Operator Applications
    - Sections
    - Conditionals
    - Lists
    - Tuples
    - Unit Expressions and Parenthesized Expressions
    - Arithmetic Sequences
    - List Comprehensions
    - Let Expressions
    - Case Expressions
    - Do Expressions
    - Datatypes with Field Labels
    - Expression Type-Signatures
    - Pattern Matching
  * Declarations and Bindings
    - Overview of Types and Classes
    - User-Defined Datatypes
    - Type Classes and Overloading
    - Nested Declarations
    - Static Semantics of Function and Pattern Bindings
    - Kind Inferenc
  * Modules
    - module structure
    - export lists
    - import declarations
    - importing and exporting instance declarations
    - name clashes and closure
    - standard prelude
    - separate compilation
    - abstract datatypes
    - smart ctors
* Input/Output
  - standard I/O functions
  - sequencing I/O operations
  - exception handling in the I/O monad
* Predefined types and classes
  - Standard Haskell Types
  - Strict Evaluation
  - Standard Haskell Classes
  - Numbers
  * Prelude
    - Prelude PreludeList
    - Prelude PreludeText
    - Prelude PreludeIO
  * specification of derived instances
    - Derived instances of Eq and Ord
    - Derived instances of Enum
    - Derived instances of Bounded
    - Derived instances of Read and Show
* FFI
  - Foreign Function Interface
  - Foreign Languages
  - Contexts
  - Lexical Structure
  - Foreign Declarations
  - Specification of External Entities
  - Marshalling
  - The External C Interface
* GHC
  * extensions
    * language pragmas
    * compiler pragmas
      - inlining
      - specialization
