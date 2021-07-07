# Polymorphism


## Polymorphism in type theory

* Polymorphism
  * General
    - Polymorphic type
    - Polymorphic value (value of polymorphic type)
    - Polymorphic function
    - Type parameter
    - Type constraint
  * Ad hoc polymorphism
    - Function overloading
    - Operator overloading
  * Parametric polymorphism
    - Generic function
    - Generic programming
  * Subtyping
    - Virtual function
    * Dispatching
      - Single dispatch
      - Double dispatch
      - Multiple dispatch
      - Dynamic dispatch
      - Predicate dispatch


* In programming languages and type theory, **polymorphism** is the provision of a single interface to entities of different types or the use of a single symbol to represent multiple different types.

* **Polymorphic types** are types whose operations are applicable to values of more than one type. *Polymorphic value* is a value of a polymorphic type. *Polymorphic function* is a function that works with many types (or all types, unless constrained).


## Polymorphism in Haskell

In Haskell polymorphism has two flavors:
* *Parametric polymorphism* allows functions to work with all types
* *Ad-hoc polymorphism* allows type restrictions by constraints


A **polymorphic function** is *parametric* if its behavior does not depend on the type at which it is instantiated.





## References

https://en.wikipedia.org/wiki/Polymorphism_(computer_science)
https://wiki.haskell.org/Polymorphism




https://john.cs.olemiss.edu/~hcc/csci450/ELIFP/ExploringLanguages.html
https://www.haskellforall.com/2021/02/folds-are-constructor-substitution.html
https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/
https://lobste.rs/t/haskell

https://en.wikipedia.org/wiki/Kind_(type_theory)
https://www.seas.upenn.edu/~cis500/cis500-f13/sf/Poly.html
https://www.cs.cornell.edu/courses/cs3110/2017fa/l/06-hop/notes.html
https://courses.cs.washington.edu/courses/cse341/02wi/functional/higher-order.html
https://timodenk.com/blog/notes-on-haskell-programming-from-first-principles/
https://timodenk.com/blog/making-slice-pointfree/

https://www.seas.upenn.edu/~cis500/cis500-f13/sf/
