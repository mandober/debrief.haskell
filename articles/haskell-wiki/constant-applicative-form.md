# Constant Applicative Form
(from Haskell Wiki)

A Constant Applicative Form (CAF) is a supercombinator (all its vars are bound) that is not a lambda abstraction (even if all its vars are bound).

CAFs include
- proper constant expressions
  - primitive literals, `12`
  - list literals, `[1,2,3]`, desugared to `1:2:3:[]`
  - saturated ops, `(+ 1 2)`
- sections, `(+4)`

Sections are CAFs despite the fact they are neither constant expressions, nor they lack free variables. What's even more unfair is that sections are desugared into lambda abstractions. A section like `(+ 4)` gets desugared into a lambda abstraction `\x -> x + 4` (that is not a CAF!).

Supercombinators are without free variables. CAFs are supercombinators, thus CAFs also lack free variables. Moreover, since they're not lambda abstractions, CAFs have no variables at all (no vars whatsoever, free or otherwise).

However, a CAF may still contain identifiers that refer to other CAFs, e.g. 
`(c 3) where c = (* 2)`

CAFs can always be lifted to the top level of a program. A CAF can either be compiled to a piece of a graph that will be shared by all referents; or to a shared code that will overwrite itself with a graph, the first time it is evaluated.

A CAF such as `ints = from 1 where from n = n : from (n + 1)` can grow unbounded, but can only be accessed from within a particular function (like `ints` here). In order to garbage-collect such structures, each such function (i.e. each function that references CAFs) is associated with a list of the CAFs it refers to. When the GC collects such a function, it also collects the associated list of CAFs.


## Refs

* Constant Applicative Form @HaskellWiki
https://wiki.haskell.org/Constant_applicative_form

https://web.archive.org/web/20031228175507/http://www.haskell.org:80/hawiki/ConstantApplicativeForm
