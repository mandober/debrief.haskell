# The Haskell 2010 Language

https://www.haskell.org/onlinereport/haskell2010/


<details><summary>TOC</summary>

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
  * expressions
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
  * modules
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

* predefined types and classes
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




</details>

---




<details><summary>TOC: Links</summary>

- [Introduction](https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-90001)
  - [Program Structure](https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-100001.1)
  - [The Haskell Kernel](https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-110001.2)
  - [Values and Types](https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-120001.3)
  - [Namespaces](https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4)
- [Lexical Structure](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-140002)
  - [Notational Conventions](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-150002.1)
  - [Lexical Program Structure](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2)
  - [Comments](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-170002.3)
  - [Identifiers and Operators](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4)
  - [Numeric Literals](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-190002.5)
  - [Character and String Literals](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6)
  - [Layout](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7)
- [Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-220003)
  - [Errors](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-230003.1)
  - [Variables, Constructors, Operators, and Literals](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-240003.2)
  - [Curried Applications and Lambda Abstractions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-260003.3)
  - [Operator Applications](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-280003.4)
  - [Sections](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-300003.5)
  - [Conditionals](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-320003.6)
  - [Lists](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-340003.7)
  - [Tuples](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-360003.8)
  - [Unit Expressions and Parenthesized Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-380003.9)
  - [Arithmetic Sequences](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-400003.10)
  - [List Comprehensions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11)
  - [Let Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-440003.12)
  - [Case Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13)
  - [Do Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14)
  - [Datatypes with Field Labels](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-490003.15)
  - [Expression Type-Signatures](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-560003.16)
- [Pattern Matching](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-580003.17)
  - [Declarations and Bindings](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-620004)
  - [Overview of Types and Classes](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-630004.1)
  - [User-Defined Datatypes](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2)
  - [Type Classes and Overloading](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)
  - [Nested Declarations](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-800004.4)
  - [Static Semantics of Function and Pattern Bindings](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-880004.5)
  - [Kind Inference](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-970004.6)
- [Modules](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-980005)
  - [Module Structure](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-990005.1)
  - [Export Lists](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2)
  - [Import Declarations](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3)
  - [Importing and Exporting Instance Declarations](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1060005.4)
  - [Name Clashes and Closure](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1070005.5)
  - [Standard Prelude](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1110005.6)
  - [Separate Compilation](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1140005.7)
  - [Abstract Datatypes](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1150005.8)
- [Predefined Types and Classes](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1160006)
  - [Standard Haskell Types](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1170006.1)
  - [Strict Evaluation](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1260006.2)
  - [Standard Haskell Classes](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270006.3)
  - [Numbers](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350006.4)
  - [Basic Input/Output](https://www.haskell.org/onlinereport/haskell2010/haskellch7.html#x14-1420007)
  - [Standard I/O Functions](https://www.haskell.org/onlinereport/haskell2010/haskellch7.html#x14-1430007.1)
  - [Sequencing I/O Operations](https://www.haskell.org/onlinereport/haskell2010/haskellch7.html#x14-1470007.2)
  - [Exception Handling in the I/O Monad](https://www.haskell.org/onlinereport/haskell2010/haskellch7.html#x14-1480007.3)
- [Foreign Function Interface](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1490008)
  - [Foreign Languages](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1500008.1)
  - [Contexts](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1510008.2)
  - [Lexical Structure](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1530008.3)
  - [Foreign Declarations](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1540008.4)
  - [Specification of External Entities](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1610008.5)
  - [Marshalling](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1690008.6)
  - [The External C Interface](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1700008.7)
- [Standard Prelude](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1710009)
  - [Prelude PreludeList](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1720009.1)
  - [Prelude PreludeText](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1730009.2)
  - [Prelude PreludeIO](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1740009.3)
- [Syntax Reference](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010)
  - [Notational Conventions](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17600010.1)
  - [Lexical Syntax](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17700010.2)
  - [Layout](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3)
  - [Literate comments](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17900010.4)
  - [Context-Free Syntax](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5)
  - [Fixity Resolution](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18100010.6)
- [Specification of Derived Instances](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18200011)
  - [Derived instances of Eq and Ord](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18300011.1)
  - [Derived instances of Enum](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18400011.2)
  - [Derived instances of Bounded](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18500011.3)
  - [Derived instances of Read and Show](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18600011.4)
  - [An Example](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18700011.5)
- [Compiler Pragmas](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-18800012)
  - [Inlining](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-18900012.1)
  - [Specialization](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-19000012.2)
  - [Language extensions](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-19100012.3)

</details>
