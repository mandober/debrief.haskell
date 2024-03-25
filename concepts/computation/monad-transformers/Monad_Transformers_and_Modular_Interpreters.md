# Monad transformers and modular interpreters
Sheng Liang, Paul Hudak, Mark Jonest, 1995

## Abstract

We show how a set of building blocks can be used to construct *programming language interpreters*, and present implementations of such building blocks capable of supporting many commonly known features, including simple expressions, three different function call mechanisms (call-by-name, call-by-value and lazy evaluation), references and assignment, nondeterminism, first-class continuations, and program tracing.

The underlying mechanism of our system is *monad transformers*, a simple form of abstraction for introducing a wide range of computational behaviors, such as state, I/O, continuations, and exceptions.

Our work is significant in the following respects. First, we have succeeded in designing a fully *modular interpreter based on monad transformers* that includes features missing from Steele's, Espinosa's, and Wadler's earlier efforts.

[1] Modular denotational semantics, 1993, David Espinosa
[2] Building interpreters by transforming stratified monads, David Espinosa, 1994

Second, we have found new ways to lift monad operations through monad transformers, in particular difficult cases not achieved in Moggi's original work. Third, we have demonstrated that interactions between features are reflected in liftings and that semantics can be changed by reordering
monad transformers.

Finally, we have implemented our interpreter in Gofer, whose constructor classes provide just the added power over Haskell's type classes to allow precise and convenient expression of our ideas. This implementation includes a method for constructing extensible unions and a form of subtyping that is interesting in its own right.
