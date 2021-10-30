# Type Functions

(from: Fun with Type Functions by SPJ)

To *prove properties of programs automatically*, the most widely used technology today is by far the ubiquitous *type checker*. However, even the most advanced static type systems are not yet sophisticated enough to permit all correct programs to compile, while excluding the incorrect ones. Because of that, the type checkers choose to error on the safe side and disallow some correct programs, rather then to allow some incorrect ones to pass through. This gap between the correct programs that are recognized as such and the correct programs that are not, is constantly getting smaller with continuous advances in type theory that eventually get implemented in type systems of PLs. Nevertheless, as a consequence, every type system has an escape hatch that allows a programmer to side-step the type checker and compile the program anyway.

This paper describes an extension to Haskell's type system that makes it more expressive without losing the benefits of automatic proof and compact expression. Specifically, we describe *type families* that allows us to express *functions on types* as easily as *functions on values*.

This facility makes it easier for programmers to effectively *extend the compiler by writing functional programs that execute during type-checking*.

## Introduction

The type of a function specifies (partially) what the function does. Although weak as a specification language, static types have compensating virtues:
- lightweight, so programmers use them
- machine-checked with minimal programmer assistance
- ubiquitous, so programmers cannot avoid them

As a result, static type checking is by far the most widely used verification technology today.




## Associated types

Haskell offers two ways to express relations on types:
* **multiparameter type classes** express arbitrary, many-to-many relations
* **type constructors** express functional relations, where one type (the arg) uniquely determines the other.

For example, the relation between the type of a list and the type of list's elements is a functional relation, expressed by the type ctor `[] :: * -> *`, which maps an arbitrary type `a` to the type `[a]`.

```hs
-- list type decl
data [] a = [] | a : ([] a) ≅ data List a = Nil | Cons a (List a)

-- kind of list type ctor
[] :: * -> * ≅ List :: * -> *
[] Bool :: * ≅ List Bool :: *

-- type of Cons
(:) :: a -> [a] ≅ Cons :: a -> List a
([]) :: [a]     ≅ Nil

-- type of list elements
a
-- type of list of a's
[a] ≅ List a
```

A **type ctor maps its arg types uniformly**, incorporating them into a more complex type *without inspecting them*.

```hs
-- List is a type ctor that expects another type as a type arg
List :: * -> *

-- List is now saturated for it was given Bool type as a type arg
List Bool :: *
```

In the example above, `List` is a unary type ctor, so it expects another type as its type arg and thus its kind is `* -> *`. When applied to another type, here to `Bool` that is its type arg, `List` becomes saturated, expressing the concrete type that is a list of Booleans, `List Bool :: *`. However, type ctors do not inspect their type args - they are uses directly as a part of a more complex type.

Unless declared with a fixed type, for example,

```hs
data ListOfInts = Nil | Cons Int ListOfInts
```


**Type functions** also establish functional relations between types, but a type function may perform *case analysis on its arg types*.
