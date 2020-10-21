# GLOSSARY

## Denotational semantics
What a program computes.

## Cabal
Cabal is a build tool, a part of a larger infrastructure for distributing, organizing and cataloging Haskell libraries and tools.

## Call-by-need
Expressions (particularly function arguments) are evaluated only once and only when needed. The first time an expression is evaluated, the result is cached to be readily available for the subsequent calls. Haskell's evaluation strategy is *call-by-name with call-by-sharing*, but Haskell provides two additional ways to force evaluation: a single bang evaluates to weak head normal form, and a double bang evaluates an expression completely.

## Evaluation strategy
Evaluation strategy determines when to evaluate function's arguments, what kind of value to pass to the function (fully/partially evaluated arguments, thunks, etc.). The evaluation strategy is specified by the language specs, not implementation. Haskell uses call-by-need evaluation strategy.

## GHC
Glasgow Haskell Compiler is the de facto Haskell implementation.

## GHCi
GHCi is the interactive version of GHC.

## ghcup
Shell utility for managing multiple GHC installations and updates.

## Hackage
Haskell's central package repository of libraries in the Cabal package format.

## Haddock
Utility to automatically generate documentation from annotated Haskell sources.

## Higher-rank type
A Higher-Rank Type (HRT) is, e.g., a function that takes polymorphic functions as arguments (as opposed to taking functions with concrete types).

## Implicit typing
All expressions will eventually be annotated, whether explicitly by the user himself or (from the user's perspective) implicitly by the compiler.

## Lazy evaluation
Haskell uses a lazy evaluation strategy, which means that an expression is not evaluated when it is bound to a variable, but it gets deferred until its result is needed by another computation.

## Lifted type
All types in Haskell include the bottom value i.e. `undefined`.

## Non-strict semantics
Non-strictness allows bypassing undefined (bottom) values (resulting from e.g. infinite loops). This enables Haskell to process data that is formally infinite.

## Operational semantics
How is a program evaluated.

## Rigid type
A type that is entirely specified by the user via a type annotation/signature.

## Stack
Haskell's modern project, build and package manager, posed to succeed Cabal.

## Stackage
Curated repository of selected packages from Hackage, consisting of extensively verified important and crucial libraries.

## Typed hole
Partially annotated type in signature or annotations. It is the intermediate option between a compiler-inferred and a user-annotated type, selected by replacing a subtype with an underscore. It may be useful if the subtype is easily inferred but verbose. (one of many uses of the underscore char).

## Type inference
A method implemented in the compiler/interpreter to help it resolve the types of all expressions, especially those that are not user annotated. Haskell uses the type inference system based on the Hindley-Milner approach but heavily extended.

## Type Family
Type Family


## Type Kind
Kind is the type of a type. A higher-order type. Scalar types (Int, Double, Bool) have the nullary kind, denoted by `*` (star) becasue they are complete, they do not need any other type to be complete unlike, e.g. `Maybe a` type which has a unary kind `* -> *` because it needs its type param provided in order to become complete. And `Either a b` has a kind `* -> * -> *`, i.e. it needs two additional types to complete him.


## Unlifted type
Unlifted types do not include bottom value. These types live in kind `#` rather than kind `*`.

## Weak head normal form
Expressions (particularly arguments) that are evaluated just enough so that their data constructor can be determined (especially in pattern matching).

## Wobbly type
A type that the compiler has to infer (as opposed to a rigid type).
