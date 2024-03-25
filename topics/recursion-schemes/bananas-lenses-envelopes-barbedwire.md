# Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire

`Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire`, 1991, Erik Meijer, Maarten Fokkinga, Ross Paterson

## Abstract

We develop a calculus for lazy functional programming based on *recursion operators* associated with data type definitions. For these operators we derive various algebraic laws that are useful in deriving and manipulating programs. We shall show that all example Functions in Bird and Wadler's "Introduction to Functional Programming" can be expressed using these operators.

## 1. Introduction

It is not hard to state, prove and use laws for well-known operations such as addition, multiplication and - at the function level - composition. It is, however, quite hard to state, prove and use laws for arbitrarily recursively defined functions, mainly because it is difficult to refer to the recursion scheme in isolation. The algorithmic structure is obscured by using unstructured recursive definitions. We crack this problem by treating various recursion schemes as separate higher order functions giving each a notation of its own, independent of the ingredients with which it constitutes a recursively defined function.

This paper gives an extension of the theory to the context of lazy FP, i.e. for us a *type is ω-cpo* and we consider only continuous functions between types; categorically, we are working in the category `CPO`.

Working in the category `Set` means that *finite data types* defined as *initial algebras*, and *infinite data types* defined as *final co-algebras*, constitute two different worlds.

In this case it is not possible to define functions by induction (catamorphisms) that are applicable to both finite and infinite data types, and arbitrary recursive definitions are not allowed.

Working in `CPO` has the advantage that the carriers of initial algebras and final co-algebras coincide, thus there is a single data type that comprises both finite and infinite elements. The price to be paid however is that partiality of both functions and values becomes unavoidable.

## 2.The data type of lists

We illustrate the recursion patterns by means of the specific data type of *cons-lists*. So, the definitions given here are actually specific instances of those given in "Constructive functional programming", Richard Bird, 1989.

Modern FPLs allow the definition of cons-lists over some type `A` by putting:

`A* ::= Nil | Cons (A, A*)`

```hs
-- | cons list
data List a = Nil | Cons (a, List a)

foldr :: (a -> b -> b) -> b -> [a] -> b
```

The recursive structure of this definition is employed when writing functions of the type `[a] -> b` that destruct a list, in genral called **catamorphisms** (from the greek preposition κατα meaning downwards), but in terms of lists called *folds* (of which `foldr` is the primary example).

**Anamorphisms** (from the greek preposition ανα meaning "upwards" as in "anabolism") are functions with sig `b -> [a]` that generate a list of type [a] from a seed of type `b`.

Functions of type `a -> b` whose call-tree has the shape of a cons-list are called *hylomorphisms* (from the Aristotelian philosophy that form and matter are one, υλoϛ meaning "dust" or "matter").

### Catamorphisms

Let `b : B` and `⨁ : (A,B) -> B`, then a list-catamorphism `h : A* -> B` is a function of the following form:

```hs
h Nil = b
h (Cons (a, as)) = a ⨁ (h as)         (1)
```

In the notation of Bird & Wadler, one would write `h = foldr b (⨁)`. We write catamorphisms by wrapping the relevant constituents between so called banana brackets:

>h = ⦇ b, ⨁ ⦈                         (2)

⦑ ⦒  ⦓ ⦔  ⦕ ⦖  ⦃ ⦄  ⦉ ⦊  ⦇ ⦈  ⦅ ⦆  ⟮ ⟯  ⦗ ⦘  ⟬ ⟭

Countless list processing functions are readily recognizable as catamorphisms.

```hs
length :: A* -> Num
length = ⦇ 0, ⨁ ⦈
  where
  a ⨁ n = 1 + n

filter :: (A -> Bool) -> A* -> A*
filter p = ⦇ Nil, ⨁ ⦈
  where
  a ⨁ as = Cons (a, as), p a
         = as,           ¬p a
```
