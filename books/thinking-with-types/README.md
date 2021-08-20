# Thinking with Types

Type-Level Programming in Haskell
by Sandy Maguire, 2019
https://leanpub.com/thinking-with-types

This book aims to be the comprehensive manual for type-level programming. It's about getting you from here to there - from a competent Haskell programmer to one who convinces the compiler to do their work for them.

He regularly writes about Haskell at:
https://reasonablypolymorphic.com


## Contents

**I Fundamentals**

1. [The Algebra Behind Types](./01-type-algebra.md)
  1.1 Isomorphisms and Cardinalities
  1.2 Sum, Product and Exponential Types
  1.3 Example: Tic-Tac-Toe
  1.4 The Curry-Howard Isomorphism
  1.5 Canonical Representations

2. Terms, Types and Kinds
  2.1 The Kind System
    2.1.1 The Kind of "Types"
    2.1.2 Arrow Kinds
    2.1.3 Constraint Kinds
  2.2 Data Kinds
  2.3 Promotion of Built-In Types
    2.3.1 Symbols
    2.3.2 Natural Numbers
    2.3.3 Lists
    2.3.4 Tuples
  2.4 Type-Level Functions

3. Variance


**II Lifting Restrictions**

4 Working with Types
4.1 - Type Scoping
4.2 - Type Applications
4.3 - Ambiguous Types and Non-Injectivity

5 Constraints and GADTs
5.1 - Introduction
5.2 - GADTs
5.3 - Heterogeneous Lists

6 Rank-N Types
6.1 - Introduction
6.2 - Ranks
6.3 - The Nitty Gritty Details
6.4 - The Continuation Monad

7 Existential Types
7.1 - Existential Types and Eliminators
7.1.1 - Dynamic Types
7.1.2 - Generalized Constraint Kinded Existentials
7.2 - Scoping Information with Existentials

8 Roles
8.1 - Coercions
8.2 - Roles

**III Computing at the Type-Level**

9 Associated Type Families
9.1 - Building Types from a Schema
9.2 - Generating Associated Terms

10 First Class Families
10.1 - Defunctionalization
10.2 - Type-Level Defunctionalization
10.3 - Working with First Class Families
10.4 - Ad-Hoc Polymorphism

11 Extensible Data
11.1 - Introduction
11.2 - Open Sums
11.3 - Open Products
11.4 - Overloaded Labels

12 Custom Type Errors

13 Generics
13.1 - Generic Representations
13.2 - Deriving Structural Polymorphism
13.3 - Using Generic Metadata
13.4 - Performance
13.5 - Kan Extensions

14 Indexed Monads
14.1 - Definition and Necessary Machinery
14.2 - Linear Allocations

15 Dependent Types
15.1 - Overview
15.2 - Ad-Hoc Implementation
15.3 - Generalized Machinery
15.4 - The Singletons Package
15.5 - Dependent Pairs
15.5.1 - Structured Logging

**IV Appendices**

Glossary
Solutions
Bibliography
About the Author
