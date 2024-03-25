# Existential types

# Existential types in general

**Existential types**, or existentials for short, are a way of encapsulating behavior and hiding it from the consumers of the type. 

Existentials allow "squashing" a group of types into a single type.


A **data algebra** is a packaged type along with operations on it. Outside its definition, the type is abstract - it can only be manipulated using the provided operations.

The types of data algebras are *existential types*, which were originally developed in constructive logic and are closely related to infinite sums (as in category theory, for example).

Although the basic calculus we use has been known for some time, we believe that the analysis of data abstraction using existential types originates with this paper: `Abstract Types Have Existential Type`, 1988, John C. Mitchell, Gordon D. Plotkin.



# Existential types in Haskell

In Haskell, existential quantification is done through the use of nested universal quantification. For example, the ST type uses an existential type `s` that holds the thread.

```hs
runST :: forall a. (forall s. ST s a) -> a
```

This type is a rank 2 type (it has 'forall' on the left of arrow, inside another forall). The Haskell98 standard only supports types of rank 1 at most, so their use requires enabling the `ExistentialQuantification` pragma,  since they are part of GHC's *type system extensions*.

In Haskell98, existential quantification is not supported at all, and universal quantification is not first class - values can have universal types if and only if they are bound by `let`. You cannot pass universally typed values to functions.


## forall keyword

The `forall` keyword explicitly brings a fresh type variable into scope.



## 6.4.6. Existentially quantified data constructors
(from GHC 9.8.1 User Guide)

- pargma:  ExistentialQuantification
- implies: ExplicitForAll
- since:   ghc-6.8.1
- status:  included in GHC2021

Allow existentially quantified type variables in types.

The idea of using existential quantification in data type declarations was suggested by Perry, and implemented in Hope+: `The Implementation of Practical Functional Programming Languages`, 1991, Nigel Perry (PhD Thesis).

It was later formalised by Laufer and Odersky in `Polymorphic type inference and abstract data types`, 1994.

It's been in Lennart Augustsson's `hbc` Haskell compiler for several years, and proved very useful.


Here is the idea of existentials. Consider the declaration:
