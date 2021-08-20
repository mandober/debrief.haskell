# 1. Introduction
https://www.haskell.org/onlinereport/haskell2010/haskellch1.html

- [1.1 Program Structure](#11-program-structure)
- [1.2 The Haskell Kernel](#12-the-haskell-kernel)
- [1.3 Values and Types](#13-values-and-types)
- [1.4 Namespaces](#14-namespaces)


Haskell is a general purpose, purely functional programming language incorporating many recent innovations in programming language design.

[academicrant] The invention of new Haskell features, as well as new, original, thinking and approaches to problems, are firstly published in academic papers, where they are also critiqued until reaching a public endorsement as to they validity and soundness. Often, these are general, theoretical achievements in computer science, not necessarily related to only the domain of functional programming languages. What is particularly inteseting is that a new reputable feature very quickly gets sanctioned for inclusion in the next GHC release as an extension. In brief, you get acquainted with many Haskell features through the very high-quality content of academic publications, as well as, as a particular topic reaches a wider audience, churning a number of blog posts, through the articles that deal with the matter in a less formal manner. Best of both worlds!


Haskell provides higher-order functions, non-strict semantics, static polymorphic typing, user-defined algebraic datatypes, pattern-matching, list comprehensions, a module system, a monadic I/O system, and a rich set of primitive datatypes, including lists, arrays, arbitrary and fixed precision integers, and floating-point numbers.

> Haskell is both the culmination and solidification of many years of research on non-strict functional languages.

This report defines the *syntax* for Haskell programs and an informal *abstract semantics* for the meaning of such programs. We leave as implementation dependent the ways in which Haskell programs are to be manipulated, interpreted, compiled, etc. This includes such issues as *the nature of programming environments* and the *error messages* returned for undefined programs, i.e. programs that formally evaluate to `⟘` (bottom).


..and now a few words by our sponsor:

  corroboration
  verification
  testimony
  endorsement
  authentication
  substantiation
  justification
  vindication
  ratification
  approval
  authorization
  validation
  sanction
  formalization
  certification
  accreditation
  recognition
  acceptance
  consent

  analysis
  evaluation
  assessment
  appraisal
  appreciation
  review
  criticism
  textual examination
  commentary
  treatise
  discourse
  exposition
  disquisition
  account


## 1.1 Program Structure

Program Structure
- abstract syntactic structure
- semantic structure
* syntactic structure
  - topmost level with modules
  - module top-level with declarations
  - expressions, values at static types 
  - bottom level: lexical structure, concrete representation


Here is described the *abstract syntactic structure* and *abstract semantic structure* of Haskell, as well as how it relates to the organization of the rest of the report.

1. `Top-most level` consists of a *set of modules* which provide software re-use and a way to control namespaces.

2. `Module top level` consists of a *collection of declarations*, of which there are several kinds. Declarations define things such as ordinary values, datatypes, type classes, and fixity information.

3. `The mid-lower level` consists of *expressions* which denote values with static types.

4. `The bottom level` consists of *lexical structure* which captures the *concrete representation* of programs in text files.

This report proceeds bottom-up with respect to Haskell syntactic structure.


## 1.2 The Haskell Kernel

The meaning of *syntactic sugar* is given by translation into simpler constructs. If these translations are applied exhaustively, the result is a program written in a small subset of Haskell, called `the Haskell kernel`.

Although the kernel is not formally specified, it is essentially a slightly sugared and enhanced variant of the *lambda calculus* (`System F` at worst) with a straightforward *denotational semantics*. The translation of each syntactic structure into the kernel is given as the syntax is introduced.


## 1.3 Values and Types

> An *expression* evaluates to a *value* and has a *static type*.

Term-level values and types are clearly separated in Haskell. The type system allows *user-defined datatypes* of various sorts, and permits not only *parametric polymorphism*, using a traditional *Hindley-Milner type structure*, but also *ad hoc polymorphism* or *overloading*, using *type classes*.

*Errors* are semantically equivalent to `⟘`, that is, errors are indistinguishable from *nontermination*, so Haskell has no mechanism for detecting and acting upon a particular failure.


## 1.4 Namespaces

There are 6 kinds of names in Haskell:
- names for *variables* and *data constructors* denote **values**
- names for *type variables*, *type constructors* and *type classes* refer to the **type system entities**
- names of modules refer to **modules**

Naming constraints:
- Names for variables and type variables are identifiers beginning with lowercase letters or underscore; the other 4 kinds of names are identifiers beginning with uppercase letters.
- An identifier must not be used as the name of a type constructor and a class in the same scope.

These are the only constraints. For example, `Int` may simultaneously be the name of a module, class, and constructor within a single scope.

6 kinds of names:
- values
  1. variables
  2. constructors
- types
  3. type variables
  4. type constructors
  5. type classes
- modules
  6. module names
  - package names? (sometimes used to fully-qualify a module's name)
