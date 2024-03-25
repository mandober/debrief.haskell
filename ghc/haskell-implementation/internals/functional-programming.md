# FP in Haskell

futurelearn.com course:
*Functional programming in Haskell The University of Glasgow*
https://www.futurelearn.com/courses/functional-programming-haskell/7/steps/726678


## Expressions

Reducing an expression `e` to a result `r`, sometimes denoted by a squiggly arrow, `e ⇝ r` or `e ~~> r`, means evaluating the terms until the expression reaches its simplest form, which is commonly referred to as the normal form.

## Reduction

We need a concrete model of a program's execution; imperative languages model the execution through statements that are executed one after another. They use the stack to keep track of values of variables, and they employ the program counter to track the location of an executing program. But FP languages don't have statements; all computation is achieved by reducing expressions.

An expression is reduced by simplifying a reducible expression, called a **redex**, at a time. Each step is called a **reduction**.

An expression is converted from a redex into the **normal form** when it cannot be reduced further, at which time we say that it's irreducible or that it has finished evaluating. The resulting expression is what we referred to as **value**, although it might not be a value in the tradional, imperative, sense.

When a reduction is performed, there may be a single reduction path, but often multiple reduction paths are possible. The model of execution of a programmng language dictates the reduction strategy in such cases. **The applicative order** first reduces the outermost leftmost redexes, but **normal order** begins reduction with innermost leftmost redexes.

Haskell uses normal order evaluatiuon strategy in its lazy incarnation which means that the expressions are not evaluated until absolutely necessary; once an expression gets evaluated, it is cached (memoized) such that the computation happens only once. This is contrasted by imperative languages which cannot afford this system because they have to deal with values that might change at any time.

> The Church-Rosser theorem states that every terminating reduction path produces the same result.

So, as long as it terminates, the applicative and normal order strategy eventually yield the same result. This implies that correctness of a program does not depend on the evaluation order. The compiler (or a programer) can rearrange program's blocks freely to improve performance, without affecting the result. The greatest benefit of this is that different expressions can be evaluated in parallel, since they don't affect the result. This puts FP languages as the leading contenders for programming parallel, multi-core, systems.


## Functions

Haskell is a functional language so the function concept is essential to the language. A function takes one or more arguments and computes a result. Given the same arguments, the result will always be the same. This is similar to a mathematical function and it means that in Haskell there are no side-effects. There are two fundamental operations on functions: function definition (creating a function) and function application (using a function to compute a result).

A function is defined by an equation, which may be multi-piece equation.


# Functional programming

- functional, functionality
- functional programming
- functional paradigm
- comparision
- immutability
- evaluation strategy


## Functional

The two central phylosophical points of FP are abstraction and composition. 

The term functional:

* *Functional Design* is a paradigm used to simplify the design of hardware and software devices such as computer software. A *functional design* assures that each modular part of a device has only one responsibility and performs that responsibility with the minimum of side effects on other parts. Functionally designed modules tend to have low coupling. Systems with functionally designed parts are easier to modify because each part does only what it claims to do. Since maintenance is more than 3/4 of a successful system's life, this feature is a crucial advantage. It also makes the system easier to understand and document, which simplifies training. The result is that the practical lifetime of a functional system is longer. In a system of programs, a *functional module* will be easier to reuse because it is less likely to have side effects that appear in other parts of the system.

* (computer science) Having semantics defined purely in terms of mathematical functions, without side-effects.

*Functional requirement*
In software engineering and systems engineering, a functional requirement defines a function of a system or its component, where a function is described as a specification of behavior between outputs and inputs.


* *functional root*
(mathematics) A function which, when applied a given number of times, equals another given function. x^2 + 1 is the functional 2nd root of x^4 + 2x^2 + 2

f(x) = x^2 + 1
g(x) = x^4 + 2x^2 + 2

(x^2 + 1) (x^2 + 1) = x^4 + 2x^2 + 2

f(x) = x^2 + 1
f(2) = 2^2 + 1 = 5
f(5) = 5^2 + 1 = 26

g(x) = x^4 + 2x^2 + 2
g(2) = 2^4 + 2*2^2 + 2 = 16+8+2=26
