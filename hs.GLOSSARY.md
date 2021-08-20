# Glossary

## Cabal

`Cabal` is a project and build manager, and a part of a larger infrastructure for distributing Haskell packages.

## Cabal hell

The Cabal is the Haskell's build tool and project manager, whose earlier versions were known to raise hell by installing the packages globally, which, of course, had cause frequent clashes of different versions of the same package. Now, a thing of the past.

## Call-by-X

This term should denote the various approaches and technics regarding the time and manner of evaluation of arguments in a function application. The most common instances for X are: value, name, sharing, need, reference.

## Call-by-need

The *call-by-need* is an approach to evaluation of function's arguments in which the arguments are evaluated only when actually needed (JIT) and only once, after which the result is memoized and shared, immediately (e.g. among the duplicated arguments) or later when the need arises. Haskell's evaluation strategy is call-by-name with call-by-sharing, otherwise known as call-by-need.

## CUSK

Complete user-supplied kind signatures. This is a legacy feature replaced by the modern `StandaloneKindSignatures` approach.

## Defunctionalization

Defunctionalization is a program transformation that aims to turn a higher-order functional program into a first-order one, that is, to eliminate the use of functions as first-class values. Its purpose is thus identical to that of closure conversion. It differs from closure conversion, however, by storing a tag, instead of a code pointer, within every closure. Defunctionalization has been used both as a reasoning tool and as a compilation technique. Defunctionalization is commonly defined and studied in the setting of a simply-typed λ-calculus, where it is shown that semantics and well-typedness are preserved. 

## Dynamic binding

A variable is called dynamically bound when it is bound by the calling context of a function, and statically bound when bound by the callee's context.

## Haskell Core

The Core is the name of the basic language that normal Haskell code is desugared and translated into. The Core is a very small, System-F based, fully type-annotated language. It is a proper subset of Haskell, consisting of a dozen type ctors, type descriptions, and such, and all of the complicated surface Haskell syntax, including those introduced by extensions, must be expressable in Core. The Core is one of the several intermediate code representations in the compilation process.

## Higher-kinded polymorphism

Polymorphism abstracts types, just as functions abstract values. Higher-kinded polymorphism takes things a step further, abstracting both types and type constructors, just as higher-order functions abstract both first-order values and functions.

## Denotational semantics

Semantics describes the meaning of a program, and denotational semantics is a manner of expressing it in a way that is close to the mathematical way of expressing proofs.

## Deforestation

Shortcut fusion of structures such as lists and trees. *Fusion* of operations on a data structure in a compositional pipeline minimizes the need to generate intermediate structures, this improving performance and decreasing memory consumption.

## Evaluation strategy

Evaluation strategy (at least) determines *when* and *how much* to evaluate an arg to a function. The "when" is related to the moment before or after entering the function. The "how much" is justified by the number of reduction steps, with each step resulting in a new redex, some of which are significant enough to warrent a distinguishing name. For example, when a value is in a weak-head normal form, it means that it was evaluated just enough to detect the presence of a data ctor (so WHNF is the form a pattern match against the data ctor will leave a value in).

## Forcing

In Haskell, unlike in set theory, the term "forcing" denotes some way to force evaluation; e.g. an expression prefixed by `!` forces a value into WHNF, while `!!` forces complete evaluation. Similar "enforcers" are `seq`, `deepseq` functions.

## Fusion

Merging several operations, that operate on the same data, into one. GHC often explores opportunities to fuse multiple list traversals and operations into a single traversal with operations merged.

## Higher-rank type

A Higher-Rank Type (HRT) is a language entity (usually a function) that takes (parametrically) polymorphic functions as args (as opposed to taking functions with concrete types).

## Implicit typing

Implicitly typed expressions are those whose type is left for the compiler to infer. Between expressions that are type-annotated by a user and those that are typed by the compiler, there is some middle ground in the form of partial annotations (types with holes).

## Kind

A kind is a type of a type. It is a higher-order type that classifies types. Almost all saturated type ctors usually have the kind `*`. Unsaturated type ctors have different function-shaped kinds, e.g. `Maybe`, `IO` or `[]` have kind `* -> *`, `Show :: * -> Constraint`, `MonadTrans :: ((* -> *) -> * -> *) -> Constraint`. The *DataKinds* extensions promotes all type ctors to kinds, and `GHC.TypeLit` enables type literals, such that type-level natural number have the kind `Nat`, and type-level string have the kind `Symbol`. The primitive, unlifted and mostly unboxed types have the kind `#`.

## Kind polymorphism

Abstraction over kinds. Writing type-level functions in such a way that they accept types of various kinds.

## Lazy evaluation

Haskell uses a lazy evaluation strategy, which means that an expression is not evaluated when it is bound to a variable, but it gets deferred until its result is needed by another computation.

## Levity polymorphism

Polymorphism is the name for abstraction over similar structures, e.g. having a single function that can operate on various similar data structures. Levity polymorphism expands its polymorphic domain to also include unlifted primitive types of the kind `#`.

## Lifting

Making a pure function available to some (monadic, computational) context.

## Lifted type

Unlifted types are the basic primitive types, not commonly encountered in code; these are the most primitive machine types like integers and floats, which have kind `#`. So the most primitive machine type of numbers is `Int#` which is usually a 64-bit values. When a type is expanded to include the bottom value, it becomes a so-called lifted type. So, There are the unlifted integers, `Int#`, and the lifted integers, `Int`. All the usual types (e.g. of the kind `*`) in Haskell include the special value called *bottom* (`⟘`) that denotes divergence.

## Name equality

Haskell uses name equality (not structural equality) when determining whether two types are equal: if they have the same name, they are the same type, they are equal.

In Haskell, `data` and `newtype` keywords are used to define new datatypes. You can make an abstract datatype by hiding its data constructors (leaving them out from a module's export list), but the association between a type name and the datatype it denotes cannot be abstracted. Therefore, it is straightforward (for the type checker) to determine whether two type names denote the same datatype: after expanding synonyms, two types are the same if they have the same name.

## Non-strict semantics

Non-strictness allows bypassing undefined (bottom) values (resulting from e.g. infinite loops). This enables Haskell to process data that is formally infinite.

## Operational semantics

deals with the operational aspects of evaluation. It describes a PL by using an abstract or virtual machine.

## Polytypic functions

A polytypic program is a program that behaves uniformly over a large class of datatypes. This uniformity is achieved by parameterizing functions over type constructors to obtain polytypic functions. A polytypic function may be defined by induction on the structure of *regular type constructors*, or in terms of other polytypic functions.

## Regular type constructor

A type constructor `d` is regular if the data type `d a` contains no function spaces and if the `d`'s type argument is the same on both sides of its type declaration.

## Rigid type

A rigid type is a type variable specified by the programmer. The term is used to describe types that were completely specified by a user (they were initially called "user-specified types"). Rigid type variables are usually mentioned in error messages associated with polymorphic functions. The dual of rigid types are (not co-rigid types but) wobbly types which are the types inferred by the compiler. The type of an expression, that lacks a user-supplied type-annotation or type signature, wobbles around, with every change being re-inferred by the compiler. But once a user stamps it with a type signature, it stops wobbling and becomes rigid.

## Singleton

In PLT, a singleton is a type with exactly one inhabitant. For example, the unit type, `() :: ()`, nicely showcases the essential property of singletons: the type is isomorphic to its (only) value. This means that knowing a type allows you deduce its value, and vice versa. Singletons, like all other values, go through the process of type erasure; comes the runtime, they have no associated type information left. But! Being singletons, it is easy to recover their types for various purposes. For example, a variable "length" whose type is a natural number (with natural numbers being encoded as singletons) will loose its type at RT, but the natural number encoded in its type will be recoverable from its value. This information can then be used to deduce, e.g., the lenght of some vector-type of value.

## The Stack

The Stack is the Haskell's project manager and package manager, created when the Cabal hell was a thing. It is not independant from Cabal, however, but it works on top of it; it creates its own project files as the convenient "views" into the cabal files. Also, the stack is associated with a curated repository of Haskell packages, called Stackage, where it pulls the dependencies from by default.

## Stackage

Curated repository of selected packages from Hackage, consisting of extensively verified important and crucial libraries.

## Static binding
A variable is called dynamically bound when it is bound by the calling context of a function, and statically bound when bound by the callee's context.

## Typed hole

A typed hole is a part of type signature that is left unspecified. The "hole" is formed when a type-level subexpression, that is a part of the overall type signature, is annotated with an underscore (either placed there by itself or prefixing an existing name). For example, instead of writing the entire type `StateT Integer IO`, you may leave out the (easily inferrable) middle part, writing `StateT _ IO` instead. Typed holes are half-way between a compiler-inferred and a user-annotated type. Typed holes may be useful when dealing with a particularly long and verbose signature (especially in type aliases). A *partially annotated type* is a type signature or a type annotation with a typed hole.


## Partial type annotation

A partially annotated type is a type signature/annotation with a typed hole.

## Type signature

A type signature is a standalone complete type-level expression describing a function. Type signatures are, by convention, welcomed at a module's top level, but they may also appear nested inside a function, as a part of a `let` or `where` clause. Although type signatures and type annotations may express the same thing in two different ways, the biggest difference between them is that type signatures are independent standalone (type) expressions. Most commonly they appear (on their own line) above the definition of the function they are associated with (associated by having the same identifier).

## Type annotation

Type annotations are type-level expressions, sort of ad hoc type expressions attached to various term-level (sub)expressions. They are more required in the absence of standalone type signatures, although it can happen that type annotations are needed to describe parts of an expression even when there is a full type signature. Often, a type annotation is somewhat of a quick bandaid.

## Type inference

A method implemented in the compiler/interpreter to help it resolve the types of all expressions, especially those that are not user annotated. Haskell uses the type inference system based on the Hindley-Milner approach but heavily extended.

## Type Family

*Indexed type families*, or type families for short, are type-level functions that take types as arguments and return types as a result. They are to data what type classes are to functions, i.e. a means to overload data types.

## Unlifted type

Unlifted types do not include bottom value. These types live in kind `#` rather than kind `*`.

## Weak Head Normal Form

WHNF is a stage in evaluating an expression. It is usually related in pattern matching against a particular data ctor. An expression that is evaluated just enough to reveal its data ctor is in weak-head normal form.

For example, a function that expects an arg of type `Val x`, where `Val` is the data ctor, and it declares the corresponding parameter as `x` i.e. using an irrefutable pattern, then the arg is not even smelled - no evaluation of the arg happens (it may even be undefined). However, if the param binds the arg using a pattern match against the data ctor, as `(Val x)` then the arg has to undergo evaluation until it reveals the expected `Val` data ctor, or it fails to pattern-match (maybe because the arg was some other type). If the pattern match fails, the pattern in the next equation is tried (and so on), but, at that point the arg has already lost a few layers, it got peeled just enough to reveal whether it was a data ctor, so the evaluation might proceed from there or the arg may be evaluated enough at that point to determine if it matches.

## Wobbly type
If a type is user-annotated, then it's *rigid*, otherwise it's *wobbly*.
