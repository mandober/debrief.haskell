# Haskell 2010 Language Report: Contents

https://www.haskell.org/onlinereport/haskell2010/haskellpa1.html#x5-8000I

## I The Haskell 2010 Language

1. Introduction
  1.1 Program Structure
  1.2 The Haskell Kernel
  1.3 Values and Types
  1.4 Namespaces
2. Lexical Structure
  2.1 Notational Conventions
  2.2 Lexical Program Structure
  2.3 Comments
  2.4 Identifiers and Operators
  2.5 Numeric Literals
  2.6 Character and String Literals
  2.7 Layout
3. Expressions
  3.1 Errors
  3.2 Variables, Constructors, Operators, and Literals
  3.3 Curried Applications and Lambda Abstractions
  3.4 Operator Applications
  3.5 Sections
  3.6 Conditionals
  3.7 Lists
  3.8 Tuples
  3.9 Unit Expressions and Parenthesized Expressions
  3.10 Arithmetic Sequences
  3.11 List Comprehensions
  3.12 Let Expressions
  3.13 Case Expressions
  3.14 Do Expressions
  3.15 Datatypes with Field Labels
    3.15.1 Field Selection
    3.15.2 Construction Using Field Labels
    3.15.3 Updates Using Field Labels
  3.16 Expression Type-Signatures
  3.17 Pattern Matching
    3.17.1 Patterns
    3.17.2 Informal Semantics of Pattern Matching
    3.17.3 Formal Semantics of Pattern Matching
4. Declarations and Bindings
  4.1 Overview of Types and Classes
    4.1.1 Kinds
    4.1.2 Syntax of Types
    4.1.3 Syntax of Class Assertions and Contexts
    4.1.4 Semantics of Types and Classes
  4.2 User-Defined Datatypes
    4.2.1 Algebraic Datatype Declarations
    4.2.2 Type Synonym Declarations
    4.2.3 Datatype Renamings
  4.3 Type Classes and Overloading
    4.3.1 Class Declarations
    4.3.2 Instance Declarations
    4.3.3 Derived Instances
    4.3.4 Ambiguous Types, and Defaults for Overloaded Numeric Operations
  4.4 Nested Declarations
    4.4.1 Type Signatures
    4.4.2 Fixity Declarations
    4.4.3 Function and Pattern Bindings
    4.4.3.1 Function bindings
    4.4.3.2 Pattern bindings
  4.5 Static Semantics of Function and Pattern Bindings
    4.5.1 Dependency Analysis
    4.5.2 Generalization
    4.5.3 Context Reduction Errors
    4.5.4 Monomorphism
    4.5.5 The Monomorphism Restriction
  4.6 Kind Inference
5. Modules
  5.1 Module Structure
  5.2 Export Lists
  5.3 Import Declarations
    5.3.1 What is imported
    5.3.2 Qualified import
    5.3.3 Local aliases
    5.3.4 Examples
  5.4 Importing and Exporting Instance Declarations
  5.5 Name Clashes and Closure
    5.5.1 Qualified names
    5.5.2 Name clashes
    5.5.3 Closure
  5.6 Standard Prelude
    5.6.1 The Prelude Module
    5.6.2 Shadowing Prelude Names
  5.7 Separate Compilation
  5.8 Abstract Datatypes
6. Predefined Types and Classes
  6.1 Standard Haskell Types
    6.1.1 Booleans
    6.1.2 Characters and Strings
    6.1.3 Lists
    6.1.4 Tuples
    6.1.5 The Unit Datatype
    6.1.6 Function Types
    6.1.7 The IO and IOError Types
    6.1.8 Other Types
  6.2 Strict Evaluation
  6.3 Standard Haskell Classes
    6.3.1 The Eq Class
    6.3.2 The Ord Class
    6.3.3 The Read and Show Classes
    6.3.4 The Enum Class
    6.3.5 The Functor Class
    6.3.6 The Monad Class
    6.3.7 The Bounded Class
  6.4 Numbers
    6.4.1 Numeric Literals
    6.4.2 Arithmetic and Number-Theoretic Operations
    6.4.3 Exponentiation and Logarithms
    6.4.4 Magnitude and Sign
    6.4.5 Trigonometric Functions
    6.4.6 Coercions and Component Extraction
7. Basic Input/Output
  7.1 Standard I/O Functions
  7.2 Sequencing I/O Operations
  7.3 Exception Handling in the I/O Monad
8. Foreign Function Interface
  8.1 Foreign Languages
  8.2 Contexts
    8.2.1 Cross Language Type Consistency
  8.3 Lexical Structure
  8.4 Foreign Declarations
    8.4.1 Calling Conventions
    8.4.2 Foreign Types
    8.4.3 Import Declarations
    8.4.4 Export Declarations
  8.5 Specification of External Entities
    8.5.1 Standard C Calls
    8.5.2 Win32 API Calls
  8.6 Marshalling
  8.7 The External C Interface
9. Standard Prelude
  9.1 Prelude PreludeList
  9.2 Prelude PreludeText
  9.3 Prelude PreludeIO
10. Syntax Reference
  10.1 Notational Conventions
  10.2 Lexical Syntax
  10.3 Layout
  10.4 Literate comments
  10.5 Context-Free Syntax
  10.6 Fixity Resolution
11. Specification of Derived Instances
  11.1 Derived instances of Eq and Ord
  11.2 Derived instances of Enum
  11.3 Derived instances of Bounded
  11.4 Derived instances of Read and Show
  11.5 An Example
12. Compiler Pragmas
  12.1 Inlining
  12.2 Specialization
  12.3 Language extensions
