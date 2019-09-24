# Glossary


## Stackage
Haskell's repository of curated packages that contains a carefully selected subset of libraries from Hackage.

## Hackage
Haskell's central package repository of libraries and applications in the Cabal package format.

## GHC
Glasgow Haskell Compiler, nowadays de facto standard.    
http://www.haskell.org/ghc/

## GHCi
GHCi is the interactive version of the Glasgow Haskell Compiler, a REPL.

## Cabal
*Common Architecture for Building Applications and Libraries* (CABAL) is a framework defining a common interface for building portable Haskell applications. Haskell's Cabal is part of a larger infrastructure for distributing, organizing and cataloging Haskell libraries and tools.    
https://www.haskell.org/cabal/

## Stack
Haskell's project manager.    
http://docs.haskellstack.org/

## ghcup
Shell utility for managing multiple GHC installations and updates.

## Haddock
Utility to automatically generate documentation from annotated Haskell sources.

## Rigid type variable
User-specified type variable. A type that is completely specified by a programmer-supplied type annotation.

## Wobbly types
Types that the compiler has to guess (contrasted with user-specified fully annotated types).

## Typed hole
Partially annotated type in signature or annotations. Adds a middle option between compiler inferred and user annotated typings. Specified by writing `_` in place of a subtype, which may be useful if the subtype is easily inferred and the programmer wants to cut down on the "noise". For example, 
`(a -> b -> [b]) `

## Higher-rank types
Higher-rank types are e.g functions that take polymorphic functions as arguments.

## Implicitly typed
The programmer may skip writing the type annotating because compiler can infer them. Still, one may choose to annotate the types explicitly whenever desired.

## Type inference
Compiler's capability to infer the types of expressions without the assistance from the programmer.

## Evaluation strategy
Evaluation strategy determines when to evaluate the arguments of a function application and what kind of value to pass to the function. The evaluation strategy is specified by the language specs, not implementation. Purely functional languages like Haskell, as well as some non-purely functional languages like R, use *call-by-need*.

## Operational semantics
How is a program evaluated

## Denotational semantics
What a program computes

WHNF
## Weak head normal form
Arguments are eval just enouhg to determine their type/data ctors.

## Lazy evaluation
Haskell uses a lazy evaluation strategy, meaning an expression is not evaluated when it is bound to a variable, but it gets deferred until its result is needed by another computation. As a consequence, arguments are not evaluated before they are passed to a function, only when their values are actually used. Technically, Haskell's evaluation strategy is *call-by-name with call-by-sharing*. However, Haskell has an escape hatch - it provides two ways to force evaluation: force to WHNF (`!`), or all the way (`!!`).

## Non-strict semantics
Non-strictness allows bypassing undefined (bottom) values (resulting from e.g. infinite loops). This enables Haskell to process data that is formally infinite.

## Unlifted data types
Unlifted types do not include bottom value. These types live in kind `#` rather than kind `*`.

## Lifted data types
All types in Haskell include the bottom value (undefined).

## Levity polymorphism
Polymorphism over "liftedness" of types. Allows defining functions which work for lifted and unlifted types. Unlifted types are machine integers and floats; these types have `#` suffix and their kind is `#`. And the kind of lifted (boxed) types that was `*` previously is renamed to `Type`. The new kind `TYPE` is the most general kind including other kinds.

```hs
TYPE :: Rep -> Type

data Rep = LiftedRep  -- pointer to heap obj
         | IntRep     -- no ptr, machine int
         | DoubleRep
         | ...

type Type = TYPE LiftedRep

Int  :: Type
Int  :: TYPE LiftedRep
Int# :: TYPE IntRep
Dbl# :: TYPE DblRep
Maybe:: Type -> Type

(+) :: ∀(r :: Rep). ∀(a :: TYPE r). (Num a) => a -> a -> a
-- so now (+) works for both boxed and unboxed sorts
3 + 4
3# + 4#

```

sort > kind > type > values
