# Denotational semantics

from Haskell Wiki book, `Haskell > Wider Theory > Denotational semantics`    
https://en.wikibooks.org/wiki/Haskell/Denotational_semantics

## Contents

1. Introduction
  1.1 What is Denotational Semantics and what is it for?
  1.2 What to choose as Semantic Domain?
2. Bottom and Partial Functions
  2.1 ⊥ Bottom
  2.2 Partial Functions and the Semantic Approximation Order
  2.3 Monotonicity
3. Recursive Definitions as Fixed Point Iterations
  3.1 Approximations of the Factorial Function
  3.2 Convergence
  3.3 Bottom includes Non-Termination
  3.4 Interpretation as Least Fixed Point
4. Strict and Non-Strict Semantics
  4.1 Strict Functions
  4.2 Non-Strict and Strict Languages
  4.3 Functions with several Arguments
5. Algebraic Data Types
  5.1 Constructors
  5.2 Pattern Matching
  5.3 Recursive Data Types and Infinite Lists
  5.4 Haskell specialities: Strictness Annotations and Newtypes
6. Other Selected Topics
  6.1 Abstract Interpretation and Strictness Analysis
  6.2 Interpretation as Powersets
  6.3 Naïve Sets are unsuited for Recursive Data Types
7. Notes
8. External Links


## Introduction

This page deals with the way to formalize the meaning of Haskell programs, that is, its denotational semantics.

It may seem to be nit-picking to formally specify that the program `square x = x * x` means the same as the mathematical square function that maps each number to its square, but what about the meaning of a program like `f x = f (x + 1)` that loops forever?

In the following, we will exemplify the approach first taken by Scott and Strachey to this question and obtain a foundation to reason about the correctness of functional programs in general and recursive definitions in particular.

We also illustrate the notions of strict and lazy evaluation that capture the idea that a function may or may not evaluate its argument. This is a basic ingredient to predict the course of evaluation of Haskell programs. It is interesting that these notions can be formulated concisely with denotational semantics alone, without a reference to an execution model.

### 1.1 What is Denotational Semantics and what is it for

The denotational semantics of Haskell determines the meaning of a Haskell program.

In general, the denotational semantics of a PL maps each of its programs to a mathematical object called *denotation*, that represents the meaning of the program.

For example, the mathematical object for the Haskell programs `10`, `9+1`, `2*5`, `sum [1..4]`, etc. can be represented by the integer `10`. We say that all those programs denote the integer `10`.

A **semantic domain** is a collection of such mathematical objects.

The mapping from program code to a semantic domain is commonly written down with double square brackets, e.g. `⟦ 2 * 5 ⟧ = 10`

**Denotations are compositional**, i.e. the meaning of a program only depends on the meaning of its elemnts: `⟦ a + b ⟧ = ⟦ a ⟧ + ⟦ b ⟧`

The same notation is used for types, `⟦ Integer ⟧ = ℤ`

It is one of the key properties of purely FPL like Haskell that a direct mathematical interpretation like "1+9 denotes 10" carries over to functions: in essence, the denotation of a program of type `Int -> Int` is a mathematical function `ℤ -> ℤ`.

While we will see that this expression needs refinement, generally, to include non-termination, the situation for imperative languages is clearly worse: a procedure with that type denotes something that changes the state of a machine in possibly unintended ways. Imperative languages are tightly coupled to the operational semantics which describes the way such programs are executed. It is possible to define a denotational semantics for imperative programs and to use it to reason about such programs, but the semantics often has operational nature and sometimes must be extended in comparison to the denotational semantics for FPL (monads are one of the most successful ways to give denotational semantics to imperative programs). In contrast, the meaning of purely FPL is by default completely independent from the way of execution. The Haskell98 standard even goes as far as to specify only Haskell's non-strict denotational semantics, leaving open how to implement them.

In the end, denotational semantics enables us to develop formal proofs that programs indeed do what we want them, mathematically. Ironically, for proving program properties in day-to-day Haskell, one can use *equational reasoning*, which transforms programs into equivalent ones without seeing much of the underlying mathematical objects. *The denotational semantics shows up whenever we have to reason about non-terminating programs*.

Since denotational semantics only states what a program is, it cannot answer questions about how long a program takes to run or how much memory it consumes; this is governed by the evaluation strategy which dictates how the computer calculates the normal form of an expression.

On the other hand, the implementation has to respect the semantics, and to a certain extent, it is the semantics that determines how Haskell programs must be evaluated on a machine.

## 1.2 Choosing semantic domains

We are now looking for suitable mathematical objects that we can attribute to every Haskell program. In case of the example `10`, `2*5` and `sum [1..4]`, it is clear that all expressions should denote the integer `10`.

Generalizing, every value `x` of type `Integer` is likely to denote an element of the set ℤ.

The same can be done with values of type `Bool`. For functions like 
`f :: Integer -> Integer`, we can appeal to the mathematical definition of "function" as a set of ordered pairs of (arg, value) i.e. its graph.

But interpreting functions as their graph was too quick, because it does not work well with recursive definitions. Consider the definition

```hs
shaves :: Integer -> Integer -> Bool
1 `shaves` 1 = True
2 `shaves` 2 = False
0 `shaves` x = not (x `shaves` x)
_ `shaves` _ = False
```

We can think of 0, 1 and 2 as being male persons with beards and the question is "who shaves whom"? Person 1 shaves himself, but 2 gets shaved by the barber (0) because evaluating the third equation yields `shaves 0 2 == True`. In general, the third line says that the barber 0 shaves all persons that do not shave themselves.

What about the barber himself, is `shaves 0 0` true or not? If it is, then the third equation says that it is not. If it is not, then the third equation says that it is.

Puzzled, we see that we just cannot attribute True or False to such equation; the graph we use as interpretation for the function shaves must have an empty spot. This means that the semantic objects must be able to incorporate partial functions (that are undefined for some values of their domain).

It is well known that this famous example gave rise to serious foundational problems in set theory. It's an example of an *impredicative definition*, a definition which uses itself, creating a logical circle. Unfortunately for recursive definitions, the vicious circle is not the problem but the feature.

## Bottom and Partial Functions

To define partial functions, we introduce a special value called bottom and say that ⟘ is the completely "undefined" value or function.

Every lifted type in Haskell contains a bottom that denotes a diverging computation. So the values of integer type are actually `ℤ ⋃ ⟘`. The type unit, `()`, has one normal inhabitant, `()`, but also `⟘`.

In Haskell, the value `undefined` denotes botom and it is defined as

```hs
undefined :: forall a. a
undefined = error "Prelude.undefined"
```

It follows from the Curry-Howard isomorphism that any value of the polymorphic type `forall a. a` must denote ⟘.

## Partial Functions and the Semantic Approximation Order
