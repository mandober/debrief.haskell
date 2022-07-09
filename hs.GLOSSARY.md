# Glossary: ῾ασκϵλλ λεξικόν

<!-- TOC -->

- [Algebraic effects](#algebraic-effects)
- [Ambiguous type](#ambiguous-type)
- [Associated type family](#associated-type-family)
- [Applicative-monad proposal](#applicative-monad-proposal)
- [Bifunctor](#bifunctor)
- [Bidirectional typechecking](#bidirectional-typechecking)
- [Cabal](#cabal)
- [Call-by-need](#call-by-need)
- [Canonical representation](#canonical-representation)
- [Constant Applicative Form](#constant-applicative-form)
- [Carrier](#carrier)
- [Closed type family](#closed-type-family)
- [Constraint synonym](#constraint-synonym)
- [Constraint trick](#constraint-trick)
- [continuation-passing style](#continuation-passing-style)
- [contravariance](#contravariance)
- [covariance](#covariance)
- [Constraint](#constraint)
- [Core](#core)
- [Complete User-Supplied Kind](#complete-user-supplied-kind)
- [Defunctionalization](#defunctionalization)
- [Dynamic binding](#dynamic-binding)
- [defunctionalization](#defunctionalization)
- [dependent pair](#dependent-pair)
- [dependent type](#dependent-type)
- [endomorphism](#endomorphism)
- [Evaluation strategies](#evaluation-strategies)
- [first class family](#first-class-family)
- [functional dependency](#functional-dependency)
- [Higher-kinded polymorphism](#higher-kinded-polymorphism)
- [Denotational semantics](#denotational-semantics)
- [Deforestation](#deforestation)
- [Evaluation strategy](#evaluation-strategy)
- [Forcing](#forcing)
- [Functions](#functions)
- [Fusion](#fusion)
- [higher rank](#higher-rank)
- [higher-kinded type](#higher-kinded-type)
- [Higher-kinded type](#higher-kinded-type)
- [Higher-rank type](#higher-rank-type)
- [Implicit typing](#implicit-typing)
- [Impredicative polymorphism](#impredicative-polymorphism)
- [indexed monad](#indexed-monad)
- [instance head](#instance-head)
- [invariant](#invariant)
- [Isomorphism](#isomorphism)
- [Jump table](#jump-table)
- [Kind](#kind)
- [Kind polymorphism](#kind-polymorphism)
- [Language entity](#language-entity)
- [Lazy evaluation](#lazy-evaluation)
- [Levity polymorphism](#levity-polymorphism)
- [Lifting](#lifting)
- [Lifted type](#lifted-type)
- [Monad transformer](#monad-transformer)
- [Name equality](#name-equality)
- [Non-strict semantics](#non-strict-semantics)
- [Normal forms](#normal-forms)
- [Nominal role](#nominal-role)
- [Non-injectivity](#non-injectivity)
- [Overloaded labels](#overloaded-labels)
- [Operational semantics](#operational-semantics)
- [Phantom type](#phantom-type)
- [Profunctor](#profunctor)
- [Promoted data constructor](#promoted-data-constructor)
- [Polytypic functions](#polytypic-functions)
- [Partial type annotation](#partial-type-annotation)
- [Precursor monad](#precursor-monad)
- [Program termination](#program-termination)
- [Rank polymorphism](#rank-polymorphism)
- [Regular type constructor](#regular-type-constructor)
- [Resource](#resource)
- [Rigid type](#rigid-type)
- [Rigid skolem](#rigid-skolem)
- [role signature](#role-signature)
- [role system](#role-system)
- [Rigid type](#rigid-type-1)
- [seq](#seq)
- [Singletons](#singletons)
- [Stack](#stack)
- [Stackage](#stackage)
- [Static binding](#static-binding)
- [sigma type](#sigma-type)
- [singleton](#singleton)
- [Skolem's variable](#skolems-variable)
- [ST trick](#st-trick)
- [Thunk](#thunk)
- [Typed hole](#typed-hole)
- [Type role](#type-role)
- [Type signature](#type-signature)
- [Type annotation](#type-annotation)
- [Type inference](#type-inference)
- [Type instantiation](#type-instantiation)
- [Type Family](#type-family)
- [Thread State Object](#thread-state-object)
- [Unlifted type](#unlifted-type)
- [Variance](#variance)
- [Values](#values)
- [Weak Head Normal Form](#weak-head-normal-form)
- [Wobbly type](#wobbly-type)
- [Zero-cost coercions](#zero-cost-coercions)

<!-- /TOC -->

## Algebraic effects
Algebraic effects are computational effects that can be represented by an equational theory, or algebraic theory, whose operations produce the effects at hand. They provide an alternative to using monads for modelling effects in FP. This approach has been realized in the programming language Eff.

## Ambiguous type
A type is ambiguous when the compiler is unable to infer it from the call site. The pragma `AllowAmbiguousTypes` permits defining ambiguously typed functions, and the pragma `TypeApplications` permits calling them.

## Associated type family
A type family that is associated with a class. Normally, they are used to emphasize the association between a class and a type family, but that type family could also be defined outside of the class.

## Applicative-monad proposal
Applicative-monad proposal (AMP) was the accepted RFC aimed to add the `Applicative` class logically positioned between the `Functor` and `Monad` classes. This entailed the subsequent reorganization of constraints: `Functor` became the superclass of `Applicative`, `Applicative` became the superclass of `Monad`. Thus, before you can define a Monad instance for your type, first you have to make the type a member of Functor and Applicative classes, the fact that the compiler will happily remind you of.

## Bifunctor
In Category Theory, a bifunctor (binary functor) is a functor whose domain is a product category. For example, `Hom` functor is of the type `Cᵒᵖ ⨯ C → Set`, and it can be seen as a functor in two arguments. The `Hom` functor is a natural example - it is contravariant in one argument, covariant in the other.

In Haskell, the `Bifunctor` is the class of types that are Functors over the two type parameters (generally, over the last two type parameters). A bifunctor is a type constructor that takes two type arguments and is a functor in both arguments. 

The `Functor` class collects the type ctors that are Functors over one of their type parameters. So, to expose the appropriate type parameter, a type ctor may either be partially applied (e.g. `Either a b` data type must be partialy applied, as `Either a`, so it becomes applicable for a Functor's instance; `Either` or `Either a b` are not suitable), or, if that is not possible (because you want to expose another type parameter but the first), then further maneuvers must be undertaken (scrambling the order of the type params by defining a newtype alias, for example).

However, unlike the Functor class that requires a single free type paramater, the `Bifunctor` class requires two free type paramaters, so `Either` type constructor is good to go in that form. The methods of the Bifunctor class permit mapping over the `Left` value, or over the `Right` value, or over both values at the same time.

## Bidirectional typechecking

Bidirectional typechecking has become popular in advanced type systems because it works in many situations where inference is undecidable. The research shows parametric polymorphism can be cleanly handled in bidirectional settings.

## Cabal
`Cabal` is a project and build manager, and a part of a larger infrastructure for distributing Haskell packages. The earlier versions of cabal were known to cause the problem nicknamed "cabal hell", manifested because cabal not only installed packages globally without versioning: incompatible versions of the same package would compete in rewriting each other over.

## Call-by-need
The call-by-need evaluation strategy of function's arguments in which the arguments are evaluated only when actually needed (JIT) and only once, after which the result is memoized and shared, immediately (e.g. among the duplicated arguments) or later when the need arises. Haskell's evaluation strategy is call-by-name with call-by-sharing, otherwise known as call-by-need.

## Canonical representation
In Haskell, every ADT has a canonical representation in which it is defined as a, possibly recursive, sum of products. Every type is isomorphic to its canonical representation.

## Constant Applicative Form
In Haskell, CAF is any supercombinator that is not a lambda abstraction. This includes truly constant expressions such as `12`, `((+) 1 2)`, `[1,2,3]`, as well as partially applied functions such as `((+) 4)`; Even though `(\x -> (+) 4 x)` is an expression equivalent to the section `((+) 4)`, the former is not a CAF merely because it is a lambda abstraction.

## Carrier
In Haskell, a carrier is an informal name for a typeclass whose only purpose is to carry ad-hoc polymorphic implementations for generic methods.

## Closed type family
In Haskell, a closed type family is a type family with all of its instances provided in its definition. Closed type families are a close analogue of functions at the type-level.

## Constraint synonym
In Haskell, a technique for turning a type synonym of CONSTRAINTs into something partially-applicable. Performed by making a new typeclass with a superclass constraint of the synonym, and giving instances of it for free given the superclass constraint. For example, class c a => Trick a and instance c a => Trick a.

## Constraint trick
In Haskell, the transformation of a multiparameter typeclass instance from instance Foo Int b to instance (a ∼ Int) => Foo a b. Useful for improving type inference when working with MPTCs.

## continuation-passing style
CPS is the technique of taking (and subsequently calling) a callback, rather than directly returning a value.

## contravariance
a type T a is contravariant with respect to a if it can lift a function a -> b into a function T b -> T a.

## covariance
a type T a is covariant with respect to a if it can lift a function a -> b into a function T a -> T b. Another name for a Functor.

## Constraint
A constraint is a relation between variables which limits the values these variables can take simultaneously. Constraints are the conditions (predicates) that restrict the types a type variable can be instantiated to. A comma in a list of constraints is playing the role of conjunction: each constraints is satisfied by some subset of types, and a variable can only be instantiated to a type belonging to the intersection of these subsets. If the constraints cannot be satisfied, the instantation of the variable fails.

## Core
The Core is the name of the basic language that normal Haskell code is desugared and translated into. The Core is a very small, System-F based, fully type-annotated language. It is a proper subset of Haskell, consisting of a dozen type ctors, type descriptions, and such, and all of the complicated surface Haskell syntax, including those introduced by extensions, must be expressable in Core. The Core is one of the several intermediate code representations in the compilation process.

## Complete User-Supplied Kind
Complete User-Supplied Kind (CUSK) signatures are a legacy feature replaced by the modern `StandaloneKindSignatures` approach.

## Defunctionalization
Defunctionalization is a program transformation that aims to turn a higher-order functional program into a first-order one, that is, to eliminate the use of functions as first-class values. Its purpose is thus identical to that of closure conversion. It differs from closure conversion, however, by storing a tag, instead of a code pointer, within every closure. Defunctionalization has been used both as a reasoning tool and as a compilation technique. Defunctionalization is commonly defined and studied in the setting of a simply-typed λ-calculus, where it is shown that semantics and well-typedness are preserved.

## Dynamic binding
A variable is called dynamically bound when it is bound by the calling context of a function, and statically bound when bound by the callee's context.
## defunctionalization
a technique for replacing a family of functions with an opaque symbol, and moving the original logic into an eval function. Used by First Class Families.

## dependent pair
a type that pairs a singleton with a value indexed by the singleton.

## dependent type
a type which isn't known statically, which depends on term-level values.

## endomorphism
a function of the form a -> a.

## Evaluation strategies
Various approaches and technics regarding the time and manner of evaluation of arguments in a function application. Some common evaluation strategies are: call-by-value, call-by-name, call-by-sharing, call-by-need, call-by-reference.

## first class family
FCF is a technique for building reusable, higher-order type families via defunctionalization.

## functional dependency
an additional invariant added to a multiparameter typeclass declaration saying that some of its type varaibles are entirely determined by others. Primarily
used to improve type inference.

## Higher-kinded polymorphism
Polymorphism abstracts types, just as functions abstract values. Higher-kinded polymorphism takes things a step further, abstracting both types and type constructors, just as higher-order functions abstract both first-order values and functions.

## Denotational semantics
Semantics describes the meaning of a program, and denotational semantics is a manner of expressing it in a way that is close to the mathematical way of expressing proofs.

## Deforestation
Shortcut fusion of structures such as lists and trees. *Fusion* of operations on a data structure in a compositional pipeline minimizes the need to generate intermediate structures, this improving performance and decreasing memory consumption.

## Evaluation strategy
Evaluation strategy (at least) determines *when* and *how much* to evaluate an arg to a function. The "when" is related to the moment before or after entering the function. The "how much" is justified by the number of reduction steps, with each step resulting in a new redex, some of which are significant enough to warrent a distinguishing name. For example, when a value is in a weak-head normal form, it means that it was evaluated just enough to detect the presence of a data ctor (so WHNF is the form a pattern match against the data ctor will leave a value in).

## Forcing
In Haskell (unlike in set theory), the term "forcing" denotes some way to force evaluation; e.g. an expression prefixed by `!` forces a value into WHNF, while `!!` forces complete evaluation. Similar "enforcers" are `seq`, `deepseq` functions.

## Functions
Haskell provides functions that may be classified according to at least a dozen of factors, including arity (nullary, unary, binary, n-ary), polymorphic rank (rank-1, rank-2, rank-K, rank-N), genericity (parametrically polymorphic, overloaded), syntactic level (term-level, type-level, kind-level), term-level association (standalone function, data ctor, field accessor), syntacic position (prefix, infix, suffix), associativity, type of identifier (alphabetical, symbolic), general shape (multi-part equations, sections, lambdas).

## Fusion
Merging several operations, that operate on the same data, into one. GHC often explores opportunities to fuse multiple list traversals and operations into a single traversal with operations merged.

## higher rank
another name for a rank-n type.

## higher-kinded type
a type which is parameterized by something other than `TYPE`.


## Higher-kinded type
A higher kinded type (HKT) is a kind-polymorphic type variable that can be instantiated at type constructors of many different kinds.

```hs
-- Type variable `a` has the base kind `Type`, `a :: Type`
type BaseKind :: Type -> Type
type BaseKind (a :: Type) = a

-- Type variable `a` has the higher kind, `a :: k`, for any kind `k`
type HKT :: forall k. k -> k
type HKT (a :: k) = a
```

Higher-kinded types are those which have type variables. Fully saturated HKTs in everyday Haskell always have kind `Type`, which means that their type constructors do not.

## Higher-rank type
A Higher-Rank Type (HRT) is a language entity (usually a function) that takes (parametrically) polymorphic functions as args (as opposed to taking functions with concrete types).

## Implicit typing
Implicitly typed expressions are those whose type is left for the compiler to infer. Between expressions that are type-annotated by a user and those that are typed by the compiler, there is some middle ground in the form of partial annotations (types with holes).

## Impredicative polymorphism
Generally, GHC will only instantiate polymorphic type variables at a monomorphic type, i.e. a type without `forall`s. Instantiating polymorphic type variables at polymorphic types is called impredicative polymorphism.

## indexed monad
a monadic structure which carries a piece of static state along with it. Indexed monads allow you to enforce protocols in the type system.

## instance head
the part of a typeclass instance that comes after the context arrow (=>).

## invariant
a higher-kinded type is said to be invariant in a type parameter if that parameter is in neither positive nor negative position.

## Isomorphism
An isomorphism is a structure-preserving map between two objects (types), usually expressed as a pair of functions, `from` and `into`, used to convert one object into the other.

The terms "isomorphism" and "isomorphic" are related to the concept of equality and the problems with it, e.g. the difficulty to determine whether two functions are the same. In this sense, isomorphism denotes a somewhat relaxed level of equality, away from the insistence on the strict equality. However, it is often the only (or the best) degree of equality we're ever gonna get (e.g. equality of functions).

Two object are isomorphic when they compare as equal when some specific subset of their properties and attributes is considered. When we say that two objects are isomorphic, i.e. equal "up to an isomorphism", we express the intent to treat them as equal because, at least for our purposes, they are. For example, books comes in all shapes and sizes, but we might say that two books are equal if they have the same ISBN number.

## Jump table
A jump table (branch table) is used to transfer program control (branching) to another part of the program by storing a table of branch instructions.

## Kind
Kinds classify types. Haskell 98 defines only 2 kinds: `κ := Type | κ -> κ`. All nullary and saturated type ctors have the kind `Type`. Unsaturated type ctors have different function-shaped kinds, e.g. `Maybe`, `IO` or `[]` have kind `* -> *`. The kind `Constraint` is the kind of saturated classes. The `DataKinds` extensions promotes all type ctors to kinds, and `GHC.TypeLit` enables type literals, such that type-level natural number have the kind `Nat`, and type-level string have the kind `Symbol`. The primitive, unlifted and mostly unboxed types have the kind `#`.

## Kind polymorphism
Abstraction over kinds. Writing type-level functions in such a way that they accept types of various kinds.

## Language entity
The Haskell 2010 Report uses the term "entity" to refer to a *value*, *type*, *class*, defined in, imported into or (re)exported from a module.

## Lazy evaluation
Haskell uses a lazy evaluation strategy, which means that an expression is not evaluated when it is bound to a variable, but it gets deferred until its result is needed by another computation.

## Levity polymorphism
Polymorphism is the name for abstraction over similar structures, e.g. having a single function that can operate on various similar data structures. Levity polymorphism expands its polymorphic domain to also include unlifted primitive types of the kind `#`.

## Lifting
Making a pure function available to some (monadic, computational) context.

## Lifted type
Unlifted types are the basic primitive types, not commonly encountered in code; these are the most primitive machine types like integers and floats, which have kind `#`. So the most primitive machine type of numbers is `Int#` which is usually a 64-bit values. When a type is expanded to include the bottom value, it becomes a so-called lifted type. So, There are the unlifted integers, `Int#`, and the lifted integers, `Int`. All the usual types (e.g. of the kind `*`) in Haskell include the special value called *bottom* (`⟘`) that denotes divergence.

## Monad transformer
A monad transformer is a data type, usually defined as a newtype. It is a monad that is based on another monad (referred to as the precursor monad); this fact is reflected in the similarity of their names: the transformers have the same name as their corresponding precursor only suffixed with a `-T` (e.g. `State` vs `StateT`, `Maybe` vs `MaybeT`, `IO` vs … gotcha! there's no `IOT` because the `IO` monad, if used, must be the base monad). Different monads are combined and their effects composed in a stack-like arrangement, with one monad nested inside another. The base monad is the most deeply nested monad, i.e. the one at the base of the monad stack. The outer monad is the exposed monad, in terms of which the whole monad stack is typed. However, for one monad to host another monad (which itself may be wrapping yet another monad, etc.), while retaining the capability to pinpoint and address any monad in this structure, we don't use vanilla monads but their t-suffixed transformer versions, all of which have a type parameter `m`, a slot for a monad they wrap. For example, the State monad transformer,`StateT s m a`, is parameterized by a state `s`, a monad `m`, and a value type `a`. Its precursor is the `State` monad, which is actually based on its transformer (with `m` filled by the `Identity` monad), rather than the other way around (which is the case with monads like Maybe, Either, list, etc.).

## Name equality
Haskell uses name equality (not structural equality) when determining whether two types are equal: if they have the same name, they are the same type, they are equal. In Haskell, `data` and `newtype` keywords are used to define new datatypes. You can make an abstract datatype by hiding its data constructors (leaving them out from a module's export list), but the association between a type name and the datatype it denotes cannot be abstracted. Therefore, it is straightforward (for the type checker) to determine whether two type names denote the same datatype: after expanding synonyms, two types are the same if they have the same name.

## Non-strict semantics
Non-strictness allows bypassing undefined (bottom) values (resulting from e.g. infinite loops). This enables Haskell to process data that is formally infinite.

## Normal forms
Laziness implies many fine-grained evaluation stages, called normal forms, between an unevaluated and a fully evaluated expression. For example, a WHNF is usually associated with pattern-matching when an expression is evaluated just enough so that its initial data ctor is revealed, thereby making sure that we deal with the value of the approapriate shape. The form of a fully evaluated expression is called the normal form. Confusingly, normal forms are all the intermediate forms of evaluating an expression, but the normal form is just the final one.

## Nominal role
In Haskell, a type variable is assigned a nominal role to prevent it from being coerced (wrt newtypes).

## Non-injectivity
In Haskell, a property of type families. A non-injective function cannot have an inverse (really, a non-bijective function cannot have an inverse).

## Overloaded labels
In Haskell, syntax for converting `SYMBOL`s into values. Used via the syntax `mySymbol`, and desugared in terms of the `GHC.Overloadedlabels.fromLabel` function. Enabled via `-XOverloadedLabels`.

## Operational semantics
deals with the operational aspects of evaluation. It describes a PL by using an abstract or virtual machine.

## Phantom type
In Haskell and other PLs, a type variable is at role phantom if it may be safely coerced into any other type. Type parameters are called phantom if they aren't represented as values (at the term-level).

## Profunctor
a type T a b is a profunctor if it is contravariant in a and covariant with respect to b.

## Promoted data constructor
the type that results from a data constructor when lifting its type to the kind level. Enabled via -XDataKinds.


## Polytypic functions
A polytypic program behaves uniformly over a large class of data types. This uniformity is achieved by parameterizing functions over type constructors to obtain polytypic functions. A polytypic function is defined by induction on the structure of a regular type constructor (of a user-defined data type) or in terms of other polytypic functions.

## Partial type annotation
A partially annotated type is a type signature/annotation with a typed hole.

## Precursor monad
In the context of monad transformers, a *precursor monad* refers to a "plain" non-transformer monad (e.g. `Maybe`, `State`), its monad transformer is based on (e.g. `MaybeT`, `StateT`). A *base monad* refers to a nested monad (e.g. `IO` in `MaybeT IO`), the monad to which the transformer is applied.

## Program termination
Program termination is an instance of the more general decision problem (Entscheidungsproblem) that Turing proved in the negative. Turing's proof implies that the existence of a (super) program, capable of deciding whether another program halts or not, is impossible. Still, program termination is a very active discipline in CS, invested in analyzing the specific restrictions and conditions under which the termination of an algorithm can be guaranteed. For instance, instead of employing a general (unrestricted) recursion in a function, perhaps a more tame form of recursion would work as well. For example, a function on integers, recursive in its first argument, could employ the well-founded recursion that places a strong restriction on the recursive argument: with each iteration through the recursive case, it must keep on decreasing, guaranteeing to hit the base case (in a finite number of steps).

## Rank polymorphism
Array-oriented PLs are primarily concerned with manipulation of array-like structures, which includes: include 0-rank scalars (scalar values), rank-1 vectors (sequences of values), rank-2 matrices (sequences of sequences), rank-3 cuboids (sequences of sequences of sequences), and so on. One consequence of this unification is the *rank polymorphism* - the type of polymorphism in which a scalar function is automatically lifted to act element-wise on a higher-ranked data structure such as array; and a scalar binary operator is lifted to act, point-wise, on pairs of arrays, and so on.

## Regular type constructor
A type constructor `d` is regular if the data type `d a` contains no function spaces and if the `d`'s type argument is the same on both sides of its type declaration.

## Resource
The Haskell 2010 Report uses the term "resource" to refer to the language items brought into scope from other modules.

## Rigid type
a type that was explicitly specified by a programmer. A type that was not inferred.

## Rigid skolem
a type variable that is both rigid and a skolem.

## role signature
the declared roles for a data type's type parameters.

## role system
the system that ensures role annotations are not violated.


## Rigid type
Rigid types originally went under the name "user-specified types", but the change was made [probably] because the current term unambiguously and uniquely pinpoints the issue. The term is the most comfortably used in GHC errors (the "typefucker" Easter egg is said to be triggered when the user provokes GHC to issue exactly 23 error msgs pertaining to a single type expresion), especially in situations involving type variables in polymorphic functions. They are dual to wobbly (co-rigid :) types, i.e. GHC-inferred types. [A/N] The division to rigid and wobbly types is perhaps significant from the aspect of the type-checker, which must makes sure all expressions are assigned an explicit type, procured one way or the other. The names chosen for these two sorts of types seem (to me) to imply that a type-less expression may wobble around a bit, making the type-checker busy re-infering its type (the type annotation is in the "flow"). But when the user slaps it with a type- annotation, the wobbling stops since an explicit type fox the expression in place, preventing any modification that is not accompanied by the corresponding type adjustment.

## seq
The `seq` function works by returning its second argument, while it ties to evaluate the first one into the normal form. In `seq a b`, the value of `a` is always evaluated before `b`.

## Singletons
In PLT, a singleton is a type with exactly one inhabitant. For example, the unit type, `() :: ()`, nicely showcases the essential property of singletons: the type is isomorphic to its (only) value. This means that knowing a type allows you deduce its value, and vice versa. Singletons, like all other values, go through the process of type erasure; comes the runtime, they have no associated type information left. But! Being singletons, it is easy to recover their types for various purposes. For example, a variable "length" whose type is a natural number (with natural numbers being encoded as singletons) will loose its type at RT, but the natural number encoded in its type will be recoverable from its value. This information can then be used to deduce, e.g., the length of some vector-type of value.

## Stack
Stack is Haskell's project and package manager, created back when using Cabal wasn't only inconvenient (complex configuration files), but rather a horrible experience ("cabal hell" was a thing). Stack doesn't work independently of Cabal, but on top of it. Stack creates own project files, which are just more convenient "views" into cabal's (changing a stack configuration file prompt Stack to recreate the corresponding cabal file). More originally, Stack is associated with Stackage, which is a curated repository of Haskell packages (Hackage is the central community repository).

## Stackage
The curated repository of essential Haskell packages (important libraries that many packages in the Haskell ecosystem depend on) that are verified before being imported from Hackage.

## Static binding
A variable is called dynamically bound when it is bound by the calling context of a function, and statically bound when bound by the callee's context.

## sigma type
another name for dependent pair.

## singleton
a type with a single inhabitant. Can be abused to create an isomorphism between types and terms.

## Skolem's variable
A Skolem's variable (or just Skolem) is an existentially quantified variable.

## ST trick
In Haskell, the `ST` trick is a technique for scoping the lifetime of a piece of data via an existential variable.


## Thunk
In strict languages, a thunk usually refers to a function that is used as a way to delay the evaluation of an expression. Thunks are more effective in PLs with first-class function support. For example, in JS, as in all strict PLs, function application triggers a full evaluation of function's args; to prevent an argument from being evaluated, JS programmers can wrap that arg in a thunk, e.g. `let thunk = () => arg`, and pass that into function instead. Later, that arg can be retrieved and evaluated by calling the `thunk()`. In Haskell, thunks have a similar purpose, but here they are realized as highly specialized function-like objects adjusted for use in a non-strict setting.

## Typed hole
A typed hole is a part of type signature that is left unspecified. The "hole" is formed when a type-level subexpression, that is a part of the overall type signature, is annotated with an underscore (either placed there by itself or prefixing an existing name). For example, instead of writing the entire type `StateT Integer IO`, you may leave out the (easily inferrable) middle part, writing `StateT _ IO` instead. Typed holes are half-way between a compiler-inferred and a user-annotated type. Typed holes may be useful when dealing with a particularly long and verbose signature (especially in type aliases). A *partially annotated type* is a type signature or a type annotation with a typed hole.

## Type role
A type role indicates whether a type has representational or nominal type equality.

a property of a type variable that describes how a data constructor that owns it is allowed to be coerced.


## Type signature
A type signature is a standalone complete type-level expression describing a function. Type signatures are, by convention, welcomed at a module's top level, but they may also appear nested inside a function, as a part of a `let` or `where` clause. Although type signatures and type annotations may express the same thing in two different ways, the biggest difference between them is that type signatures are independent standalone (type) expressions. Most commonly they appear (on their own line) above the definition of the function they are associated with (associated by having the same identifier).

## Type annotation
Type annotations are type-level expressions, sort of ad hoc type expressions attached to various term-level (sub)expressions. They are more required in the absence of standalone type signatures, although it can happen that type annotations are needed to describe parts of an expression even when there is a full type signature. Often, a type annotation is somewhat of a quick bandaid.

## Type inference
A method implemented in the compiler/interpreter to help it resolve the types of all expressions, especially those that are not user annotated. Haskell uses the type inference system based on the Hindley-Milner approach but heavily extended.

## Type instantiation
This context prescribes that certain Type variables can only be instantiated with those types that belong to certain (in the context defined) type classes."

## Type Family
Indexed type families, or type families for short, are type-level functions that take types as arguments and return types as a result. They are to data what type classes are to functions, i.e. a means to overload data types.

## Thread State Object
A Thread State Object (TSO) object is only ~18words + stack.

## Unlifted type
Unlifted types do not include bottom value. These types live in kind `#` rather than kind `*`.

## Variance
In Haskell, variance is a property of a type ctor `T` in relation to one of its type parameters, `T a`. Briefly, the question of variance is: if we can transform an `a` into `b`, does that necessarily mean we can transform a `T a` into `T b`? If these two are not correlated (a change in one doesn't affect the other) the relation is *invariant*. Otherwise, the relation is *variant*: if a change in one triggers a change in the same direction in the other, the relation is *covariant*; if a change in one elicits a change in the other, but in the opposite direction, the relation is *contravariant*.

## Values
In Haskell, as in all PLs, new values are created either using literals or constructor functions.

*Literals* are hard-coded constant expressions used to denote values of language primitives (language primitive types usually have some level of correspondence to machine primitive types). There are *numerical literals* (Int, Integer, Double, Float, etc.), *textual literals* (Char, String, Text). The *unit* value is denoted by `()`.

A few compound types (lists, tuples, functions) have also been endowed with a literal form.

Literal forms for *lists* come in several variants: `[1,2]` (roster notation), `[0..9]` (range), `[x | x <- xs ]` (list comprehension).

In GHC, enabling the `OverloadedLists` language extension, makes it possible to overload the list literal form and use it as a literal for collections type, including *sets*, `[1,2] :: Set Int`, *maps*, `[(97,'a'), (98,'b')] :: Map Int Char`, *arrays* `[1,2] :: Array Int`, and similar types that can express their content with enumeration, more-less stretching the list literal notation. In fact, `Map` already stretches the list literal: it is not merely a list, but a list of tuples, `[(k₀,v₀), (k₁,v₁), …]` (each first component is a key that maps to a value in the second component).

*Tuples* also have literal notation, the form of which depends on their arity, `n`, with `n >= 2`; because, when `n = 0` the type is called unit (it makes no difference if it's rather seen as an empty tuple); `n = 1` is an invalid arity for tuples (it is just a parenthesized expression). For n >= 2, the tuples have expected literal forms: `(1,1)` (pair), `(1,True,'a')` (triple), and so on for other n-tuples.

Since *functions* are also values, and frequently used values at that, also have a literal form, `\ x -> sqrt x`.

Values of other types are created using the corresponding constructor function, thus called *value constructors* (or data constructors).

## Weak Head Normal Form
As opposed to strict PLs, where an expression is either unevaluated or fully evaluated, Haskell's evaluation process is far more segmented. Haskell expressions undergo several intermediary forms as they progress from virginal (untouched) to fully evaluated values. Expressions can be considered as if having several value layers, and the way these layers are stripped (whether all at once, or ever so gently) depends on numerous factors, ranging from the surrounding context to the type of the value itself. WHNF is a stage in the process of evaluating an expression. It is usually related in pattern matching against a particular data ctor. An expression that is evaluated just enough to reveal its data ctor is in weak-head normal form (WHNF). For example, a function that expects an arg of type `Val x`, where `Val` is the data ctor, and it declares the corresponding parameter as `x` i.e. using an irrefutable pattern, then the arg is not even smelled - no evaluation of the arg happens (it may even be undefined). However, if the param binds the arg using a pattern match against the data ctor, as `(Val x)` then the arg has to undergo evaluation until it reveals the expected `Val` data ctor, or it fails to pattern-match (maybe because the arg was some other type). If the pattern match fails, the pattern in the next equation is tried (and so on), but, at that point the arg has already lost a few layers, it got peeled just enough to reveal whether it was a data ctor, so the evaluation might proceed from there or the arg may be evaluated enough at that point to determine if it matches.

## Wobbly type
The type annotations written by the user are referred to as *rigid*, while types left for GHC to infer are called wobbly. For instance, a type variable that's a part of the user-specified signature is a rigid type variable, while the inferred one is wobbly. Unlike the rigid type, the wobbly types are rarely mentioned in the type error messages.

## Zero-cost coercions
Haskell supports zero-cost coercions, a mechanism where types that share the same run-time representation may be freely converted between. To make sure such conversions are safe and desirable, a role system is put in place to manage and prohibit invalid coercions.
