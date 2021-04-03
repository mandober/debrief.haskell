# Glossary

## Cabal
Cabal is a project and build manager, a part of a larger infrastructure for distributing Haskell packages.

## Call-by-need
Evaluating a function's argument only when actually needed and only once, after which the result is memoized (and shared - immediately, for any duplicated args, as well as later). Therefore, Haskell's evaluation strategy is call-by-name with call-by-sharing.

## Haskell Core
The Core is the name of a miniature language that the surface Haskell code is desugared and translated into. The Core is a very small, System-F based, proper subset of the whole Haskell language, consisting of just a dozen ctors, type descriptions, et al., that all of Haskell fits into.

## Denotational semantics
Semantics describes what is meant by the program, its meaning and denotation is the way this information is presented - as math equations and math proofs.

## Deforestation
Short cut fusion on list structures.

## Evaluation strategy
Evaluation strategy (at least) determines *when* and *how much* to evaluate an arg to a function. The "when" is related to the moment before or after entering the function. The "how much" is justified by the number of reduction steps, with each step resulting in a new redex, some of which are significant enough to warrent a distinguishing name. For example, when a value is in a weak-head normal form, it means that it was evaluated just enough to detect the presence of a data ctor (so WHNF is the form a pattern match against the data ctor will leave a value in).

## Forcing
Haskell also provides a way to force evaluation: an expression prefixed by `!` forces it to WHNF, while `!!` force complete evaluation.

## Fusion
Merging several operations, that operate on the same data, into one. GHC often explores opportunities to fuse multiple list traversals and operations into a single traversal with operations merged.

## Higher-rank type
A Higher-Rank Type (HRT) is, e.g., a function that takes polymorphic functions as arguments (as opposed to taking functions with concrete types).

## Implicit typing
Implicitly typed expressions are the expressions whose type is left for the compiler to infer. Between expressions that are type-annotated by a user and those that are typed by the compiler, there is some middle ground in the form of partial annotations (types with holes).

## Kind
Kind is the type of a type. A higher-order type. Scalar types (Int, Double, Bool) have the nullary kind, denoted by `*` (star) becasue they are complete, they do not need any other type to be complete unlike, e.g. `Maybe a` type which has a unary kind `* -> *` because it needs its type param provided in order to become complete. And `Either a b` has a kind `* -> * -> *`, i.e. it needs two additional types to complete him.

## Lazy evaluation
Haskell uses a lazy evaluation strategy, which means that an expression is not evaluated when it is bound to a variable, but it gets deferred until its result is needed by another computation.

## Lifted type
All types in Haskell include the bottom value i.e. `undefined`.

## Non-strict semantics
Non-strictness allows bypassing undefined (bottom) values (resulting from e.g. infinite loops). This enables Haskell to process data that is formally infinite.

## Operational semantics
How is a program evaluated.

## Rigid type
Rigid types are types that are completely and directly specified by the author, e.g. via signatures or type annotations. Rigid type variables are usually mentioned in the compiler's error messages related to polymorphic functions. The thing with polymorphic functions is that the broader the type, the less things a function can perform. That is, the more the types it works with, the less the behaviour they all have in common. Therefore, a type variable, like `a`, is very flexible for it stands for any imaginable type - but only from the aspect of the function's caller! Form within the function, it is actually quite rigid.

## Singleton
A single element set. By set ≅ type, a singleton is a type with a single inhabitant, e.g. `() :: ()`. Also refers to a structure that has a single element.

## Stack
Haskell's project manager (build tool, package manager) established to save us all from "the cabal hell" (package version clashes). It works on top of the cabal, and it doesn't fail to cause me grievances every goddamn day.

## Stackage
Curated repository of selected packages from Hackage, consisting of extensively verified important and crucial libraries.

## Typed hole
Partially annotated type in signature or annotations. It is the intermediate option between a compiler-inferred and a user-annotated type, selected by replacing a subtype with an underscore. It may be useful if the subtype is easily inferred but verbose. (one of many uses of the underscore char).

## Type inference
A method implemented in the compiler/interpreter to help it resolve the types of all expressions, especially those that are not user annotated. Haskell uses the type inference system based on the Hindley-Milner approach but heavily extended.

## Type Family
Type-level function

## Unlifted type
Unlifted types do not include bottom value. These types live in kind `#` rather than kind `*`.

## Weak Head Normal Form
WHNF is a stage in evaluating an expression. It is usually related in pattern matching against a particular data ctor. An expression that is evaluated just enough to reveal its data ctor is in weak-head normal form.

For example, a function that expects an arg of type `Val x`, where `Val` is the data ctor, and it declares the corresponding parameter as `x` i.e. using an irrefutable pattern, then the arg is not even smelled - no evaluation of the arg happens (it may even be undefined). However, if the param binds the arg using a pattern match against the data ctor, as `(Val x)` then the arg has to undergo evaluation until it reveals the expected `Val` data ctor, or it fails to pattern-match (maybe because the arg was some other type). If the pattern match fails, the pattern in the next equation is tried (and so on), but, at that point the arg has already lost a few layers, it got peeled just enough to reveal whether it was a data ctor, so the evaluation might proceed from there or the arg may be evaluated enough at that point to determine if it matches.

## Wobbly type
If a type is user-annotated, then it's *rigid*, otherwise it's *wobbly*.
