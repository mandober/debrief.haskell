# Haskell Report

Specs
- Haskell Report 1998
  https://www.haskell.org/onlinereport/index98.html
- Haskell Report 2010
  https://www.haskell.org/onlinereport/haskell2010/
+ GHC Haskell: Haskell 2010 plus GHC extensions
  https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts.html



Haskell Report 2010: Extensions to Haskell 98
https://www.haskell.org/onlinereport/haskell2010/haskellli2.html#x3-5000

The most significant language changes in Haskell 2010 relative to Haskell 98:
* New language features:
  - Foreign Function Interface (FFI)
  - Hierarchical module names, e.g. `Data.Bool`
  - Pattern guards
* Removed language features:
  - The (n + k) pattern syntax

---

## Part I The Haskell 2010 Language

1. Introduction
  1.1 Program Structure
  1.2 The Haskell Kernel
  1.3 Values and Types
  1.4 Namespaces
2 Lexical Structure
  2.1 Notational Conventions
  2.2 Lexical Program Structure
  2.3 Comments
  2.4 Identifiers and Operators
  2.5 Numeric Literals
  2.6 Character and String Literals
  2.7 Layout
3 Expressions
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
  3.16 Expression Type-Signatures
  3.17 Pattern Matching
4 Declarations and Bindings
  4.1 Overview of Types and Classes
  4.2 User-Defined Datatypes
  4.3 Type Classes and Overloading
  4.4 Nested Declarations
  4.5 Static Semantics of Function and Pattern Bindings
  4.6 Kind Inference
5 Modules
  5.1 Module Structure
  5.2 Export Lists
  5.3 Import Declarations
  5.4 Importing and Exporting Instance Declarations
  5.5 Name Clashes and Closure
  5.6 Standard Prelude
  5.7 Separate Compilation
  5.8 Abstract Datatypes
6 Predefined Types and Classes
  6.1 Standard Haskell Types
  6.2 Strict Evaluation
  6.3 Standard Haskell Classes
  6.4 Numbers
7 Basic Input/Output
  7.1 Standard I/O Functions
  7.2 Sequencing I/O Operations
  7.3 Exception Handling in the I/O Monad
8 Foreign Function Interface
  8.1 Foreign Languages
  8.2 Contexts
  8.3 Lexical Structure
  8.4 Foreign Declarations
  8.5 Specification of External Entities
  8.6 Marshalling
  8.7 The External C Interface
9 Standard Prelude
  9.1 Prelude PreludeList
  9.2 Prelude PreludeText
  9.3 Prelude PreludeIO
10 Syntax Reference
  10.1 Notational Conventions
  10.2 Lexical Syntax
  10.3 Layout
  10.4 Literate comments
  10.5 Context-Free Syntax
  10.6 Fixity Resolution
11 Specification of Derived Instances
  11.1 Derived instances of Eq and Ord
  11.2 Derived instances of Enum
  11.3 Derived instances of Bounded
  11.4 Derived instances of Read and Show
  11.5 An Example
12 Compiler Pragmas
  12.1 Inlining
  12.2 Specialization
  12.3 Language extensions

## Part II The Haskell 2010 Libraries

13 Control.Monad
  13.1 Functor and monad classes
  13.2 Functions
14 Data.Array
  14.1 Immutable non-strict arrays
  14.2 Array construction
  14.3 Accessing arrays
  14.4 Incremental array updates
  14.5 Derived arrays
  14.6 Specification
15 Data.Bits
16 Data.Char
  16.1 Characters and strings
  16.2 Character classification
  16.3 Case conversion
  16.4 Single digit characters
  16.5 Numeric representations
  16.6 String representations
17 Data.Complex
  17.1 Rectangular form
  17.2 Polar form
  17.3 Conjugate
  17.4 Specification
18 Data.Int
  18.1 Signed integer types
19 Data.Ix
  19.1 The Ix class
  19.2 Deriving Instances of Ix
20 Data.List
  20.1 Basic functions
  20.2 List transformations
  20.3 Reducing lists (folds)
  20.4 Building lists
  20.5 Sublists
  20.6 Searching lists
  20.7 Indexing lists
  20.8 Zipping and unzipping lists
  20.9 Special lists
  20.10 Generalized functions
21 Data.Maybe
  21.1 The Maybe type and operations
  21.2 Specification
22 Data.Ratio
  22.1 Specification
23 Data.Word
  23.1 Unsigned integral types
24-37 FFI
  24 Foreign
  25 Foreign.C
  26 Foreign.C.Error
    26.1 Haskell representations of errno values
  27 Foreign.C.String
    27.1 C strings
    27.2 C wide strings
  28 Foreign.C.Types
    28.1 Representations of C types
  29 Foreign.ForeignPtr
    29.1 Finalised data pointers
  30 Foreign.Marshal
  31 Foreign.Marshal.Alloc
    31.1 Memory allocation
  32 Foreign.Marshal.Array
    32.1 Marshalling arrays
  33 Foreign.Marshal.Error
  34 Foreign.Marshal.Utils
    34.1 General marshalling utilities
  35 Foreign.Ptr
    35.1 Data pointers
    35.2 Function pointers
    35.3 Integral types with lossless conversion to and from pointers
  36 Foreign.StablePtr
    36.1 Stable references to Haskell values
  37 Foreign.Storable
38 Numeric
  38.1 Showing
  38.2 Reading
  38.3 Miscellaneous
39 System.Environment
40 System.Exit
41 System.IO
  41.1 The IO monad
  41.2 Files and handles
  41.3 Opening and closing files
  41.4 Operations on handles
  41.5 Text input and output
42 System.IO.Error
  42.1 I/O errors
  42.2 Types of I/O error
  42.3 Throwing and catching I/O errors

---

## 1. Introduction

## 1.1 Program Structure

At the topmost level a Haskell program is a set of modules. Modules provide a way to control namespaces and to re-use software in large programs.

The top level of a module consists of a collection of declarations, of which there are several kinds, all described in Chapter 4. Declarations define things such as ordinary values, datatypes, type classes, and fixity information.
At the next lower level are expressions, described in Chapter 3. An expression denotes a value and has a static type; expressions are at the heart of Haskell programming "in the small".

At the bottom level is Haskell’s lexical structure. The lexical structure captures the concrete representation of Haskell programs in text files.
