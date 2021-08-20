# Generics

Generics in Haskell relies on the fact that all datatypes are represented using sums and product types. Any datatype has a canonical representation as a sum of products. Canonical sum type is `Either` and canonical product is a pair.

Generics seems to be about generelizing the data ctors under a scheme so functions are then defined using these generalized names; primarily folding functions, which, for each data ctor take appropriate function that deals with it.

```hs
-- list is a sum of products
data L a = C0 | C1 a (L a)

-- fold for a list is
g :: L a                -- C0
f :: a -> L a -> L a    -- C1

fold :: (a -> b -> b) -> b -> L a -> b
fold f g C0 = g C0
fold f g C1 = f C1

fold f z (C1 x xs) = f x (fold f z xs)
```

...something like that, approximately. Generics have a lost of U1 (nullary data ctor), V1 (unary data ctor) and shit.


## Generic programming

Generics allow us to use the same code with different data types and in this regard they are very close to polymorphism.

Structuring data, i.e. creating a datatype is an important task - once the data structure is chosen, developers add functionality to it. Some functionality is inherent to a datatype, but some is common across many datatypes, e.g.
- traversals and mappings
- serialization
- comparing datatypes for structure-based equality
- adapting data accessors after a datatype has changed

Generic programming (or datatype-generic programming) addresses these concerns by high-level programming patterns.




## Scrap Your Boilerplate 

*Scrap your boilerplate* (SYB) approach is a generic programming approach for Haskell. Using this approach, you can write generic functions such as traversal schemes (e.g. `everywhere` and `everything`), as well as generic read, generic show and generic equality (i.e. `gread`, `gshow`, `geq`). This approach is based on just a few primitives for type-safe cast and processing constructor applications.

Programming Languages - Application and Interpretation (Second Edition) by Shriram Krishnamurthi, 2017


## References

SYB: a generic programming approach for Haskell
https://web.archive.org/web/20071002141529/http://www.cs.vu.nl/boilerplate
https://github.com/goldfirere/syb
https://en.wikibooks.org/wiki/Haskell/SYB
https://wiki.haskell.org/Generics
https://wiki.haskell.org/GHC.Generics
https://mail.haskell.org/pipermail/generics/2008-June/000343.html
https://markkarpov.com/tutorial/generics.html
https://web.archive.org/web/20070929223517/http://www.generic-haskell.org/
