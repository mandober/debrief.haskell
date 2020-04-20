# Generics

https://wiki.haskell.org/Generics
https://wiki.haskell.org/GHC.Generics

https://mail.haskell.org/pipermail/generics/2008-June/000343.html

Structuring data plays an important role in software development and many programming methods and tools center around creating a datatype. Once the structure of the data has been decided, a developer adds functionality to it. There is always some functionality inherent to a datatype, which is a part of reason for creating the datatype in the first place.

However, there is some functionality that is common to many datatypes, such as
- changing all values of a complicated type where a function is applied to all occurrences of a particular data constructor while leaving the rest alone
– serializing a value of a datatype
- comparing two values of a datatype for equality in case when their equality depends only on the datatype structure
– adapting data access functions after a datatype has changed, something that often involves modifying large amounts of existing code.


**Generic programming**, or *datatype-generic programming*, addresses these high-level programming patterns. 


## Scrap Your Boilerplate 


SYB: a generic programming approach for Haskell
https://github.com/goldfirere/syb
https://code.google.com/archive/p/scrapyourboilerplate/
https://en.wikibooks.org/wiki/Haskell/SYB

*Scrap your boilerplate* (SYB) approach is a generic programming approach for Haskell. Using this approach, you can write generic functions such as traversal schemes (e.g. `everywhere` and `everything`), as well as generic read, generic show and generic equality (i.e. `gread`, `gshow`, `geq`). This approach is based on just a few primitives for type-safe cast and processing constructor applications.

Programming Languages - Application and Interpretation - Second Edition - Shriram Krishnamurthi 2017

April 14, 2017
