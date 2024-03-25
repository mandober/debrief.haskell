# Expression problem

## TL/DR

In computer science, **the expression problem**, as postulated by Philip Wadler, is a *coding challenge* serving to determine the level of expressiveness of a programming language.

## The challenge

The problem is phrased as a coding challenge: given an implementation of a simple DSL, consisting of a few simple expression types, along with an interpreter of the language's expressions, the taks is to add a new expression type and to write another interpreter, with the restriction that the existing code must not be altered.

~~PLs in which this task cannot be met are deemed inexpressive, that's easy. The problem is ranking the expressiveness of PLs in which the task can be completed successfully. I imagine this is achievable in any self-respecting language, even if it involves, say, `eval`, monkey-patching, or s³ (Some Such Shit)™.~~

~~We need to introduce some notion of the cost and see how the cost of some sick, overly complex, but working solution fares against some baseline cost (which could be the cost of refactoring pumped up by the accompaying bellyaches, induced addiction, demise of health, plumetting well-being, rehab bill, instilled angst on the upstream producers, radiating disguist of the downstream consumers, API breakdown, 19th nervous breakdown, varicose, and s³).~~

~~Expressivity of a PL can only be intuitively understood, and the same holds for describing and understanding the full scope of intricacies this task implies (if it is at all possible to complete it successfully in a given PL), things like code complexity, readability, maintainability, and such, along with the other factors which all increase "the costs" (in some sense of the cost; which is definitely money-wise, but also cost of reputation, cost in face of long-term commitements, and some such shit). Best to roll all these into one notion of cost and compare it against the cost that a simple refactoring from the get-go would incurr; also pumping the notion of cost there~~



A PL that permits such extension in a relatively straightforward manner is deemed highly expressive. The term "relatively straightforward manner" suggests that a more straightforward (in some sense, perhaps simpler, easier) way to achieve the task should affect the expressiveness more favorably (perhaps because it will be easier to introduce new coders to the codebase)

is deemed better than a complicated way, of course. Some sense of the cost should also influence this estimation, perhaps comparing the cost of refactoring with the cost of maintainability of the solution.



It evaluates the support a PL provides for extending the existing functionality of a program without having to update and refactor the exisiting code.

PLs are evaluated across two primary dimensions: the support for adding new objects (new terms, data structures, data types), and the possibility for adding new behaviours (new methods) to existing objects.

The problem can be phrased by considering a simple expression evaluator. Namely, such a program deals with different *expression types* and has implementations of different *interpreters* (for evaluation of expressions, for pretty printing, for stringifying the results, for serialization, etc.).

>The central question is how easy is to extend such a program to support new expression types and new interpreters over the expressions?

Ideally, extending both expression types and evaluators should be possible without touching the existing code.


## Dimensions of expressiveness

First, the two opposing dimensions of program functionality are recognized:
1. dimension of *expression types* (objects of the modeled domain)
2. dimension of *interpreters* (operations on those types)

In Haskell, it is common to first come up with the data types that represent the objects from the domain we're modeling, before we start implementing the operations on these types. Most of the time, we use ADT or GADT declarations to define these types, while we use functions, possibly also involving some type classes, to define their functionality. The types are then called *expression types* and the functions that define their behavior are called *interpreters* (since, in a sense, they interpret the types).

>The expression problem evaluates languages according to how easy is to add new features with regards to the exisiting code base.

It is about the amount of refactoring (in the sense of repairing the affected code) that a programmer will have to perform after they augment the program by adding new expression types and/or new operations (interpreters).

That is, the expression problem ultimately asks whether it is possible to extend a program in both dimensions: to add new expression types and/or new interpreters, *without* touching (repairing) the existing code.


## Expression problem

The expression problem, as formulated by Philip Wadler, is about extending a program (best seen as a DSL implementation, e.g. an interpreter for some language) across two dimensions: dimension of *expression types* and dimension of *interpreters* (operations on those types) without refactoring the existing code base (particularly for it is not entirely owned/authored by the programmer).

The expression problem addresses the augmentation of the implementation in order to include new expression types, **without breaking the API**.

>The expression problem asks whether it is possible to extend a language in both of these dimensions without needing to modify the existing code.

We want to be able to augment an implementation without breaking the API. If we take as an example a simple DSL of arithetic expressions consisting only of integer literals and addition, then extending this DSL in the dimension of expression types means adding new language items, i.e. adding new data ctors, like multiplication, and extending in the dimension of interpreters means adding new methods, such as a pretty printing function.

In FP (fronted by Haskell), the addition of interpreters is straightforward and requires no modification of the existing code, while the addition of new expression types requires adjusting the existing code.

In OOP (fronted by Java), the situation is reversed: the addition of new expression types is straightforward and requires no modification of the existing code, while the addition of new interpreters requires adjusting the existing code.

The expression problem asks whether it is possible to expand the code in both dimensions, regardless of the language paradigm, without the need to touch the existing code. Solutions to expression problem would enable developers to augment the upstream code base without breaking the existing APIs; they would be able to declare new APIs that work on the augmented code base, but the code base would still remain compliant with the old APIs.

In our case-study, this means that the initial version of the code, with the DLS only having integer literals and addition, while the only interpreter is the `eval` function, works as expected and makes up the API v.1.

Augmenting the code by adding a new expression type (multiplication) and a new interpreter (`view` for pretty priniting) would then make the API v.2. The benefit is that both APIs would work over the same code base - API v.1 would keep referring to the laguage items (types, operations) it always had, but the API v.2 would be able to refer to the original laguage items and to the added laguage items as well.

Getting this to work implies that we have to delineate and structure the code from the get go to allow for such easy expansion. It implies that the correct approach is coding-to-iterface instead of coding-to-types directly.

## Expression DSL

To illustrate this, we consider a simple expression language consisting only of integer literals and addition.

FP and OOP are in some sense dual to each other.




new kinds of expressions (which in Haskell amounts to adding new data ctors), and, on the other hand, adding new functions that manipulate these expressions (which in Haskell amounts to adding new interpreters for such expressions). by adding new DSL items (like Mul) and by adding new functions over the DSL - to consider which addition implies. without refactoring the existing code but only adding to it.



## Expression language in Haskell

In Haskell, expressions in this language are described by the `Exp` ADT:

```hs
data Exp = Lit Int | Add Exp Exp
-- this type is isomorphic to a Tree with values at leaves only
```

We can now represent an expression like `1 + (2 + 3)` by

```hs
-- 1 + (2 + 3) = (+) 1 ((+) 2 3)
e1 = Add (Lit 1) (Add (Lit 2) (Lit 3))
e1 :: Exp
```

And we can write the main interpreter of this DSL as the `eval` function; but we first have to decide on the type of the resulting values - and here it only makes sense for it to be an `Int`.

```hs
eval :: Exp -> Int
eval (Lit n) = n
eval (Add n m) = eval n + eval m

-- Evaluating a sample exp
x1 = eval e1 -- 6
x1 :: Int
```

Nothing remarkable so far, this just establishes the base setup that we'll try to extend across two dimensions. One dimension represent the language items of the DSL, which, in Haskell, are represented by the data ctors of the `Exp` type, which are, at the moment, just `Lit` and `Add`. The other dimension represents the functions that work on the `Exp`, such as `eval`.

### Extending the DSL

To extend the DSL with a multiplication operation, we must introduce a new data ctor `Mul` by refactoring the definition of the `Exp` type, which, in turn, forces us to refactor every function over `Exp`.

```hs
data Exp = Lit Int | Add Exp Exp | Mul Exp Exp

eval :: Exp -> Int
eval (Lit n) = n
eval (Add n m) = eval n + eval m
eval (Mul n m) = eval n * eval m
```

Functional and object-oriented paradigms are in a sense *duals*. Consider a table whose columns are labeled by operations and whose rows are labeled by expression types, and a cell at the intersection of a given column and row contains a definition of the corresponding operation for the corresponding expression type.
