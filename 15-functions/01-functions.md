# Functions

- functions
- purity
- honest
- side-effects
- referential-transparency, inlining
- arity
- currying, auto-currying, uncurry
- multi-line definition as equations
- let
- where
- lambdas
- bindings
- patter matching



# Function

In Functional Programming Languages (FPL), functions are the shit. They are the basic building blocks and the most fundamental means of abstraction. Data flows through functions from the start to the end of a program.

## Purity

In Haskell, all functions are **honest** - to do their job, they use only the things they've explicitly required through their parameters, never referring to items in an external or global scope.

Functions in Haskell are like **math functions** - a function receives input through the formal params it declares (one or more parameters, or none at all) and it must always return an output value. For the same input a math function always returns the same output and so do Haskell's functions.

This property powers **referential transparency** which signifies that a variable can always be replaced with the value it was initialized with (variables are immutable) and that a call to a function can always be replaced with that function's return value (functions are pure). Compilers can make useful and more advanced optimizations (such as function **inlining**) knowing that this property is always uphold. And people tend to reason about such code more easily.

Math functions don't deal with **side-effect**, which are the actions that mutate the external environment, mutate the external state. For example, printing a string to the console or fetching a record from a DB is a side-effect. Side-effects break referential transparency, but Haskell has put in place a mechanism to control them in an efficient manner. The litmus test for **purity** of a function is **memoization** - if a function can be memoized, it is pure.
