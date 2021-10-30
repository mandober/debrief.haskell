# 1. Introduction

https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-90001

- [1.1 Program Structure](1001-program-structure.md)
- [1.2 The Haskell Kernel](1002-haskell-kernel.md)
- [1.3 Values and Types](1003-values-and-types.md)
- [1.4 Namespaces](1004-namespaces.md)



Haskell is a general purpose, purely functional programming language incorporating many recent innovations in programming language design. Haskell provides higher-order functions, non-strict semantics, static polymorphic typing, user-defined algebraic datatypes, pattern-matching, list comprehensions, a module system, a monadic I/O system, and a rich set of primitive datatypes, including lists, arrays, arbitrary and fixed precision integers, and floating-point numbers.

> Haskell is both the culmination and solidification of many years of research on non-strict functional languages.

This report defines the *syntax* for Haskell programs and an informal *abstract semantics* for the meaning of such programs. We leave as implementation- dependent the ways in which Haskell programs are to be manipulated, interpreted, compiled, etc. This includes such issues as *the nature of programming environments* and the *error messages* returned for undefined programs, i.e. programs that formally evaluate to `⟘` (bottom).
