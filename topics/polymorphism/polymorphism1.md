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

- https://en.wikipedia.org/wiki/Kind_(type_theory)
- https://en.wikipedia.org/wiki/Polymorphism_(computer_science)
- https://john.cs.olemiss.edu/~hcc/csci450/ELIFP/ExploringLanguages.html
- https://lobste.rs/t/haskell
- https://timodenk.com/blog/making-slice-pointfree/
- https://timodenk.com/blog/notes-on-haskell-programming-from-first-principles/
- https://wiki.haskell.org/Polymorphism
- https://www.cs.cornell.edu/courses/cs3110/2017fa/l/06-hop/notes.html
- https://www.haskellforall.com/2021/02/folds-are-constructor-substitution.html
- https://www.seas.upenn.edu/~cis500/cis500-f13/sf/
- https://www.seas.upenn.edu/~cis500/cis500-f13/sf/Poly.html
- https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/
- https://courses.cs.washington.edu/courses/cse341/02wi/functional/higher-order.html



## Polymorphism

* Polymorphism is a concept in PL theory. It means "many shapes", and the shapes here are types. Actually, types are more like casts, as they mold their terms; so, it is more that all the terms of particular type have a particular "shape" that the type imprinted on them.

* Many language entities can be polymorphic, but certainly this term comes most often when discussing functions.

* Polymorphism is about making a function (from now on all polymorphic language entities will be represented by considering functions as their representative) work with more than one type.

Dynamic languages can be consider polymorphic by default - all values in a dynamic PL are polymorphic, that is, you cannot specify types (e.g. in a function's signature) even if you wanted to. Any function will accepts a value of any types, and within the function the received value can be examined and cast between the types.

Static languages have a type system, which is active until after the compile-time. This, in turn, means that many IDEs will rely on the type system to inform you about the CT errors as you code, at design-time. However, there are type systems and then there are "type systems".

The `C` PL has the later; its has a very rudementary type system that can only prevent some elementary mistakes. On the other hand, C lack polymorphism, meaning the type system must be respected, which often is quite a nuissance. For example, the lack of polymorphism, but the presence of types, means your function that just adds the two inputs is typed to receive two `int`s and to return an `int`. For any other combination of numeric types, you have to write a dedicated function; in fact, you only have to write a dedicated *function header* by specifing the involved types, while the *function body* can remain pretty much identical for each *instance*. The term "instance" can be consider just that: virtually the same implementation (function body) with varying signature (function header).

This is exactly what polymorphism has made immensily convenient. Instead of a multitude, you only write one implementation (definition), but you specify the most general types, i.e. *type variables*, rather than some concrete ones.


* Every type system excludes some correct programs, and permits some incorrect ones. This is the famous gap between the capabilities of a type-checker and all the expressable but untypable things in a language. The reaserch of type system aims to bridge this gap, so that everything that can be expressed in a PL can also be type checked for correct behaviour.

For example, a language that lacks polymorphism will reject this program:

```hs
f :: [Int] -> [Bool] -> Int
f is bs = length is + length bs
```

because the `length` function cannot apply to both a list of Ints and a list of Bools. The solution is to use a more sophisticated type system in which we can give `length` a polymorphic type.

```hs
-- monomophic:
length :: [Int] -> Int
-- polymophic enough:
length :: [a] -> Int
-- utterly polymophic:
length :: (Traversable t, Num n) => t a -> n
```

Conversely, most languages will accept the expression `s + d`, where `s` is a variable representing speed, and `d` represents distance, even though adding a speed to a distance is nonsensical as adding a character to a boolean.

The type-system designer wants to accommodate more good programs and exclude more bad ones, without going overboard and losing the virtues mentioned above.
