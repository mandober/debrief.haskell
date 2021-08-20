# Effect Systems

Weak forms of effect tagging are found in some PLs, but programming with a complete effects system is still an open area in language research. At a minimum, an effects system would have the syntax for defining and marking out the regions of effectfull code.

A static type system should be integrated with an effects system, such that all possible effects are clearly denoted in the type signature of expressions (functions).

A pure function is a unit of code, a procedure whose output depends solely on its input. Besides *the main effect* - returning the calculated output - no other, *side-effect*, is permissable.

The side-effects include any change made to the external environment (the environment outside the function), like printing to the console, getting user input, connecting to a db, and many more. However, it cannot be expected that a function could perform any calculation without interacting with the outside world, because it must do things like reading and writing to CPU registers, memory, cache, manipulating the stack, and many other necessary hardware low-level manipulations. These are all very effectful, but only from the aspect of the underlying implementation - we cannot observe these effects from the surface language. That's why functions can still be considered pure even with all these dirty effects because they are not observable from the outside.

> A function is pure is it lacks *observable* side-effects.

One of the central notions in pure FP is that programming inevitably includes both pure and effectful (impure) logic. It is also considered useful to be able to distinguish and classify effectful and pure code, as it would improve reasoning about programs. The research into effect systems is fundamentally about canonising our intuition about the correct program effect synthesis, and formalizing a model that compilers can consult in order to generate better code.

## Algebraic effect handlers

A promising system for handling effects comes in the form of **algebraic effect handlers** (AEH). AEH admits a tractable effect-inference algorithm for checking effectful logic.

### Koka

There is a academic language out of Microsoft Research lab called `Koka` which presents the most developed implementations of these ideas.
- https://koka-lang.github.io/koka/doc/book.html
- https://www.microsoft.com/en-us/research/project/koka/

Koka is a strongly typed FPL with effect types and handlers. The core of Koka consists of a small set of well-studied language features, like first-class functions, *polymorphic type system*, *polymorphic effect system*, algebraic data types, and effect handlers. Each of these is composable and avoid the addition of "special" extensions by being as general as possible.

Koka tracks the (side) effects of every function in its type, where pure and effectful computations are distinguished. The precise *effect typing* gives Koka rock-solid semantics *backed by category theory*, which makes Koka particularly easy to reason about for both humans and compilers.

*Effect handlers* let you define advanced control abstractions, like exceptions, async/await, or probabilistic programs, as a user library in a typed and composable way.

`Perceus` is an advanced compilation method for reference counting. Together with *evidence translation*, this lets Koka compile directly to `C` code *without needing a garbage collector or runtime system*. Perceus also performs *reuse analysis* and optimizes FP-style programs to use *in-place updates* when possible.

First-class Named Effect Handlers, 2021
https://www.microsoft.com/en-us/research/publication/first-class-named-effect-handlers/
