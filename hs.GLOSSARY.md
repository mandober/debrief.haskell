# Glossary: ῾ασκϵλλ λεξικόν

<!-- TOC -->

- [Abstract data type](#abstract-data-type)
- [Action](#action)
- [Algebraic data types](#algebraic-data-types)
- [Algebraic effects](#algebraic-effects)
- [Ambiguous type](#ambiguous-type)
- [Applicative-monad proposal](#applicative-monad-proposal)
- [Associated type family](#associated-type-family)
- [Bindings](#bindings)
- [Bifunctor](#bifunctor)
- [Bidirectional typechecking](#bidirectional-typechecking)
- [Cabal](#cabal)
- [Call-by-need](#call-by-need)
- [Canonical representation](#canonical-representation)
- [Constant applicative form](#constant-applicative-form)
- [Carrier](#carrier)
- [Closed type family](#closed-type-family)
- [Common subexpression elimination](#common-subexpression-elimination)
- [Concrete data type](#concrete-data-type)
- [Constraint synonym](#constraint-synonym)
- [Constraint trick](#constraint-trick)
- [Constructive type theory](#constructive-type-theory)
- [Continuation-passing style](#continuation-passing-style)
- [Contravariance](#contravariance)
- [Covariance](#covariance)
- [Constraint](#constraint)
- [Core](#core)
- [Complete User-Supplied Kind](#complete-user-supplied-kind)
- [Defunctionalization](#defunctionalization)
- [Dynamic binding](#dynamic-binding)
- [Dependent pair](#dependent-pair)
- [Dependent type](#dependent-type)
- [Endomorphism](#endomorphism)
- [Evaluation strategies](#evaluation-strategies)
- [Fast and loose reasoning](#fast-and-loose-reasoning)
- [First class family](#first-class-family)
- [Functional dependency](#functional-dependency)
- [Higher-kinded polymorphism](#higher-kinded-polymorphism)
- [Denotational semantics](#denotational-semantics)
- [Deforestation](#deforestation)
- [Evaluation strategy](#evaluation-strategy)
- [Forcing](#forcing)
- [Functions](#functions)
- [Functional dependency](#functional-dependency-1)
- [Fusion](#fusion)
- [higher rank](#higher-rank)
- [Higher-kinded type](#higher-kinded-type)
- [Higher-order function](#higher-order-function)
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
- [Parametrized data type](#parametrized-data-type)
- [Phantom type](#phantom-type)
- [Profunctor](#profunctor)
- [Promoted data constructor](#promoted-data-constructor)
- [Polytypic functions](#polytypic-functions)
- [Partial type annotation](#partial-type-annotation)
- [Precursor monad](#precursor-monad)
- [Program termination](#program-termination)
- [Rank polymorphism](#rank-polymorphism)
- [Regular type constructor](#regular-type-constructor)
- [Reification](#reification)
- [Resource](#resource)
- [Rigid type](#rigid-type)
- [Rigid skolem](#rigid-skolem)
- [role signature](#role-signature)
- [role system](#role-system)
- [Rigid type](#rigid-type-1)
- [Run-time system](#run-time-system)
- [seq](#seq)
- [Singletons](#singletons)
- [Stack](#stack)
- [Stackage](#stackage)
- [Static binding](#static-binding)
- [sigma type](#sigma-type)
- [singleton](#singleton)
- [Skolem's variable](#skolems-variable)
- [ST trick](#st-trick)
- [Symmetric function](#symmetric-function)
- [Thunk](#thunk)
- [Type class](#type-class)
- [Typed hole](#typed-hole)
- [Type role](#type-role)
- [Type signature](#type-signature)
- [Type annotation](#type-annotation)
- [Type inference](#type-inference)
- [Type instantiation](#type-instantiation)
- [Type Family](#type-family)
- [Thread State Object](#thread-state-object)
- [Unlifted type](#unlifted-type)
- [Variable](#variable)
- [Variance](#variance)
- [Values](#values)
- [Visible type application](#visible-type-application)
- [Weak Head Normal Form](#weak-head-normal-form)
- [Wobbly type](#wobbly-type)
- [Zero-cost coercions](#zero-cost-coercions)

<!-- /TOC -->

## Abstract data type
https://wiki.haskell.org/Abstract_data_type

>An abstract data type is a type with exposed associated operations (API), but hidden representation (hidden implementation).

Common examples of abstract data types are the built-in primitive types. Haskell supports the definition of custom abstract data types via the module system. In many cases it is not necessary to completely hide the representation of data - it is sufficient to hide its data ctors, instead providing custom *smart constructors*. *Parametrized types* can be viewed as a kind of abstract type, because they leave some parts of the data type undefined, i.e. abstract.

## Action
An action is a value that, if performed, may have an effect upon some context in order to produce its result. Actions are sometimes referred to as computations. Actions usually have an abstract monadic type. For example, `IO a`, the type of I/O actions, `ST s a`, the type of encapsulated-state actions. There is really only one safe way to "perform" an I/O action: bind it to `Main.main` in the program; when the program is run, the I/O will be performed. `Main.main` is also an action, so it requires a context as well. But unlike `ST s a`, that is provided by the Haskell implementation itself. Actions exist as a separate group between imperative procedures and ordinary Haskell values, having some properties of each: like other Haskell values, they can be inertly passed from definition to definition; like imperative procedures, they can have effects, but only when used in the appropriate context.

## Algebraic data types
Algebraic data types (ADT) are data types resulting from the use of specific methods of type construction that resamble algebraic operations, in most part in terms on the cardinality of the produces types. In algebra, fixing the set of numbers to ℕ, the number 0 is the additive unit, i.e. the identity element of addition. In Haskell, the `Void` ADT plays that role, with the addition corresponding to sum types, e.g. enums, tagged unions, disjoint unions; in general, the types that correspond to the logical disjunction (OR types) - because sums are compound types, but to construct a value of a sum type, you only need a single value of any of its constituent types. This is contrast to product types (resords, tuples, structs) which require all the values (of its component types) in order to construct a value; and they use the singletone type `()` as the identity (unit plays the role of multiplicative identity, 1).

- 0 type: `Void`
- 1 type: `()`
- Canonical sum type:       `Either a b` ≅ `a + b`
- Canonical product type:     `Pair a b` ≅ `a ⨯ b` ≅ `(a, b)`
- Exponental type: `(->) a b` = `a -> b` ≅ `a ^ b`


```
Maybe a = Nothing | Just a` ≅ `Either () a
       m = 1 + a            ≡         1 + a
```


Namely, 0 is represented by the empty type `Void`, 1 is represented by the unit type `()`, the addition is represented by the sum types (tagged union), multiplication by the product types (tuples, records), and exponentiation by the function types. The algebra of the ADTs is justified by the 

isomorphisms such as `aᵇ ⨯ aᶜ = aᵇᐩᶜ`, which translates into the type signature `(b -> a, c -> a) -> (a -> Either b c)`, where pair `(x,y)` is the canonical product type, (⨯) ≅ x ⨯ y, `Either x y` is the canonical sum type, (+) ≅ x + y, and function type `x -> y` is the canonical exponentiation type, (^) ≅ yˣ. So, e.g. the type `Maybe a` is canonically expressed as `Either () a`.

(b -> a, c -> a) -> a -> Either b c
(a -> b, a -> c) -> a -> (b, c)


## Algebraic effects
Effects may be handled in different ways in FP. Haskell uses monads for modelling the effects - they provide an elegant, but still not a perfect solution, because they are often not explicit enough about the type of an effect they handle. For example, the value `doSomething :: IO ()` does not tell us anything about what it might actually do, which is somewhat similar to the situation from imperative languages where we have no idea what a function actually does looking at its signature. Algebraic effects is the approach to solve this problem by naming all the effects a function may exert. The effects are collected in an open union type (as constraints or as data types), so a function's signature lists each effect associated with it. Algebraic effects are represented by an equational theory or algebraic theory, whose operations produce the effects. This approach has been realized in the programming language "Eff".

## Ambiguous type
A type is ambiguous when the compiler is unable to determine a unique instantiation of a type variable. The extension `AllowAmbiguousTypes` permits defining ambiguously-typed functions, and the extension `TypeApplications` permits calling them.

## Applicative-monad proposal
Applicative-monad proposal (AMP) was the accepted RFC aimed to add the `Applicative` class logically positioned between the `Functor` and `Monad` classes. This entailed the subsequent reorganization of constraints: `Functor` became the superclass of `Applicative`, `Applicative` became the superclass of `Monad`. Thus, before you can define a Monad instance for your type, first you have to make the type a member of Functor and Applicative classes, the fact that the compiler will happily remind you of.

## Associated type family
A type family that is associated with a class. Normally, they are used to emphasize the association between a class and a type family, but that type family could also be defined outside of the class.

## Bindings
A binding is the association between a name and an expression. Bindings may occur at the top level, where they resamble variables in other languages, e.g. `x = 5`. A binding is also the association between a functions's name and its defining equations. Other bindings are not obvious and they may occur at many different places. Bindings almost always occur in pattern matching. A binding has a scope, so there are module-level, function-level, expression-level, and other bindings. A nested binding will *shadow* the same binding from an outer level.

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

## Constant applicative form
In Haskell, CAF is any supercombinator that is not a lambda abstraction. This includes truly constant expressions such as `12`, `((+) 1 2)`, `[1,2,3]`, and partially applied functions such as `((+) 4)`; Even though `(\x -> (+) 4 x)` is an expression equivalent to the section `((+) 4)`, the former is not a CAF merely because it is a lambda abstraction.

## Carrier
In Haskell, a carrier is an informal name for a typeclass whose only purpose is to carry ad-hoc polymorphic implementations for generic methods.

## Closed type family
In Haskell, a closed type family is a type family with all of its instances provided in its definition. Closed type families are a close analogue of functions at the type-level.

## Common subexpression elimination
Common Subexpression Elimination (CSE) is a compiler optimization. In GHC, it is controlled with the flag `-f-cse` (on by default). The compiler flag `-fno-cse` prevents common subexpression elimination being performed on the module (e.g. CSE may combine side effects, induced by a common subexpression, that were meant to be separate).

## Concrete data type
A concrete data type is an opposite of an abstract data type. It is a specialized solution-oriented data type that represents a well-defined single solution domain concept. A concrete data type is rarely reusable beyond its original use, but can be embedded or composed with other data types to form larger data types. Concrete data types can be introduced with the `data` construct (without parameters), or by specializing a parametrized data type to a specific situation. For example, `Maybe Integer`, `Bool`, `[(String,String)]` and `Tree String` are concrete data types.

## Constraint synonym
In Haskell, a technique for turning a type synonym of CONSTRAINTs into something partially-applicable. Performed by making a new typeclass with a superclass constraint of the synonym, and giving instances of it for free given the superclass constraint. For example, class c a => Trick a and instance c a => Trick a.

## Constraint trick
In Haskell, the transformation of a multiparameter typeclass instance from instance Foo Int b to instance (a ∼ Int) => Foo a b. Useful for improving type inference when working with MPTCs.

## Constructive type theory
Haskell is said to be based on Constructive Type Theory (CTT).

## Continuation-passing style
CPS is the technique of taking (and subsequently calling) a callback, rather than directly returning a value.

## Contravariance
A type T a is contravariant with respect to a if it can lift a function a -> b into a function T b -> T a.

## Covariance
A type T a is covariant with respect to a if it can lift a function a -> b into a function T a -> T b. Another name for a Functor.

## Constraint
A constraint is a relation between variables which limits the values these variables can take simultaneously. Constraints are the conditions (predicates) that restrict the types a type variable can be instantiated to. A comma in a list of constraints is playing the role of conjunction: each constraints is satisfied by some subset of types, and a variable can only be instantiated to a type belonging to the intersection of these subsets. If the constraints cannot be satisfied, the instantation of the variable fails.

## Core
The Core is the name of the basic language that normal Haskell code is desugared and translated into. The Core is a very small, System-F based, fully type-annotated language. It is a proper subset of Haskell, consisting of a dozen type ctors, type descriptions, and such, and all of the complicated surface Haskell syntax, including those introduced by extensions, must be expressable in Core. The Core is one of the several intermediate code representations in the compilation process.

## Complete User-Supplied Kind
Complete User-Supplied Kind (CUSK) signatures are a legacy feature replaced by the modern `StandaloneKindSignatures` approach.

## Defunctionalization
A technique for replacing a family of functions with an opaque symbol, and moving the original logic into an evaluation function. Used by first class type families.

Defunctionalization is a program transformation intended to turn a higher-order functional program into a first-order one by eliminating the use of higher order functions (i.e.the use of functions as first-class values).

Its purpose is thus identical to that of closure conversion. It differs from closure conversion, however, by storing a tag, instead of a code pointer, within every closure. Defunctionalization has been used both as a reasoning tool and as a compilation technique. Defunctionalization is commonly defined and studied in the setting of a simply-typed λ-calculus, where it is shown that semantics and well-typedness are preserved.


## Dynamic binding
A variable is called dynamically bound when it is bound by the calling context of a function, and statically bound when bound by the callee's context.

## Dependent pair
A type that pairs a singleton with a value indexed by the singleton.

## Dependent type
A type which isn't known statically, which depends on term-level values.

## Endomorphism
In category theory, an endomorphism is a morphism on the same object. Each object has at least one endomorphism - its identity morphism. In general, "endo" prefix indicates adjectives like "homogeneous".

## Evaluation strategies
Various approaches and technics regarding the time and manner of evaluation of arguments in a function application. Some common evaluation strategies are: call-by-value, call-by-name, call-by-sharing, call-by-need, call-by-reference.

## Fast and loose reasoning
Functional programmers often reason about programs as if they were written in a total language, expecting the results to carry over to a partial languages. This phrase stands for the justification of such reasoning. It comes from the title of the 2006's paper `Fast and loose reasoning is morally correct` by Nils Anders Danielsson, John Hughes, Patrik Jansson, Jeremy Gibbons in which they justify such reasoning. The phrase is directly related to justifying categorical reasoning in Haskell in term of the "Hask" category, despite it being no category at all (due to, at least, the presence of the bottom).

## First class family
FCF is a technique for building reusable, higher-order type families via defunctionalization.

## Functional dependency
A form of special constraint, added to a multiparameter class declaration, that asserts an additional invariant expressed as determination (dependency) relation between type variables in a class' head. Often used to help drive type inference. For example, a multiparameter class' head is normally declared as, e.g. `class Multi a b c where …`, but with a functional dependency (fundep) added, it may look like e.g. `class Multi a b c | a, b -> c where …`, where the fundep states that the type var `c` is completely determined by the type vars `a` and `b`.

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

## Functional dependency
A functional dependency, aka *fundep* or *fd*, usually occurs in class definitions where it is used to express additional constraints on the type parameters. That is, it expresses the dependencies between the type parameters, e.g. `r -> a` means that the type param `a` is completely determined by the type param `r`. It requires enabling the `FunctionalDependencies` GHC pragma.

## Fusion
Merging several operations, that operate on the same data, into one. GHC often explores opportunities to fuse multiple list traversals and operations into a single traversal with operations merged.

## higher rank
Another name for a rank-n type.

## Higher-kinded type
A higher-kinded type is a type which is parameterized by something other than `TYPE`.

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

## Higher-order function
the use of functions as first-class values

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
A monad transformer is a data type that combines the capabilities of multiple monads into a single one. Monad transformers are composed into a *monad stack* which allows us to interleave the effects and capabilities of multiple monads.

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

## Parametrized data type
A parametrized data type is a type whose type ctor has at least one type parameter, e.g. `List a` which is a list of any type. It may be *instantiated* at any type, so it becomes a list specialized to that type; e.g. `List Int` gets us a list of integers. Type parameters are used to parameterize a type. A function with type params is called a generic function, e.g. `map :: (a -> b) -> List a -> List b`. More complex types are parameterized or indexed over types that belong to a certain kind; e.g. the type of vectors, `Vec a Nat` is said to be parameterized over any type `a`, but *indexed* over the type of natural numbers, `Nat`. In dependently-typed languages, indexing is done using the actual values (i.e. term-level natural numbers), instead of the awkward (in Haskell, by the runtime, all types get erased) type-level naturals.

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

## Reification
Reification is the observation of underlying structure.

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

## Run-time system
Run-time system (RTS) is the program that actually executes compiled Haskell code. RTS is impure, thus allowing Haskell functions to remain pure. Within Haskell, an effectful function is pure because side effects are still represented by datatypes that only describe various effects. It is only when RTS executes a program that effects are executed, affecting the environment. RTS schedules and manages the execution of Haskell programs. It takes care of threading, inlcuding concurrency and parallelism, manages memory, garbage collection, etc.

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

## Symmetric function
A function is symmetric is it can be composed with itself.

## Thunk
In strict languages, a thunk usually refers to a function that is used as a way to delay the evaluation of an expression. Thunks are more effective in PLs with first-class function support. For example, in JS, as in all strict PLs, function application triggers a full evaluation of function's args; to prevent an argument from being evaluated, JS programmers can wrap that arg in a thunk, e.g. `let thunk = () => arg`, and pass that into function instead. Later, that arg can be retrieved and evaluated by calling the `thunk()`. In Haskell, thunks have a similar purpose, but here they are realized as highly specialized function-like objects adjusted for use in a non-strict setting.

## Type class
A type class is a language entity similar to an interface, in that it defines a set of behaviors that members of the class must implement.

Type classes divide types into subclasses, like a class of number types, printable types, mappable types, etc.

The type of polymorphism type classes give rise to is *ad hoc polymorphism*; it allows using the same name for a function that does a similar thing across many different types. More concretely, the 'Functor' class groups mappable types. Each member type had to implement the main mapping function "map" to be granted membership to Functor. This means that many different types have the same name for the mapping function ('map'), even though each type has (even drastically) different implementation of it.

The immediate advantage is that thenceforth, we can use the same, easily remembered name, for a mapping function at any type. Ad hoc polymorphism enables generic programming in exactly the same way - common functionality (behavior, like mapping) has the same name everywhere. If we have a set of such behaviors, we can write highly generic functions that don't even know what concrete type they are dealing with - that gets resolved later at call sites; a function then sees that its arg is, e.g. an integer, so it gets reveled that all generic behaviors (functions) should be specialized to integers (so the 'map' name is resolved with, say, 'mapInt' function).

- type class declaration
- type class implementation
- defining an class instance for a type
- deriving type class instances automatically
- a type class instance
- type class head
  - receiver type ctor of a class
- type class body
  - type class methods
  - method signatures
  - minimal definition
  - methods with default implementation
  - default (specialized) methods


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

## Variable
The notion of variable in Haskell is the same as the notion of variable in math or lambda calculus. Variables are preferably called **bindings** as they are names that bind values and expressions.

Once defined, the binding cannot be changed - there is no notion of reassignment, like reassigning a different expression to a previously declared variable. In fact, declaration and initialization - aka definition - must happen at the same time (on the same line), i.e. it is not possible to declare a variable of some type (declaration), but only later to assign a value/exp to it (initialization). So binding are variables in the sense that their value won't (and connot) be changed in the same module (or during the exactution of the same program), but another module may associate a different exp to the same variable name. On the other hand, this is not the case with true constants, which *never* change (not across modules, programs, not even across languages). For example, the name `pi` could be called a variable, but is a sort of variable that is always associated with some approximation of the number `π` expressed in terms of the `Double` type. So, `pi` may be called a variable, especially in PL that allow mutation (so nothing prevents users from reassigning it), but it really should be considered a proper constant - one that has the same value even across programming languages.

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

## Visible type application
When calling a polymorphic function, Haskell, unlike System F, does not require you to pass it the type arguments since GHC can infer them from their value arguments. 

This is called visible type applications because you can only explicitly specify the types of type parameters that are not hidden (using the `{p}` syntax).

However, should you wish to specify the type argument explicitly, GHC provides the special syntax to do so, enabled via the pragma `TypeApplication`. 

Then, in a call to a polymorphic function, you can specify the types, after the function's name but before the value arguments, by prefixing each type argument by `@`.

The syntax is

```hs
         function call
┌─────────────┴──────────────────────────┐
fun   @τ₀ @τ₁ … @τₙ  t₀ t₁ … tₙ  x₀ x₁ … xₖ
└┬┘   └───┬───────┘ └──┬─────┘  └──┬─────┘
 │        │            │        value args
 │        │         value args
 │      type args
name

τᵢ - type args
tᵢ - value args corresponding to the type args
xᵢ - value args corresponding to concrete types
```

For example, `fmap` is a polymorphic function that declares the type parameters in the order `f`, `a`, `b`, where `f` must be a type ctor `f :: Type -> Type`. So the visible type applications must be specified respecting this order.

```hs
-- the type of fmap
fmap :: forall (f :: Type -> Type) a b. Functor f => (a -> b) -> f a -> f b

fmap @[] @(Map Int Char) @Char

```

`fmap @[] @(Map Int Char) @Char` completely monomorphisizes `fmap` to work over these concrete types

  :: (Map Int Char -> Char) -> [Map Int Char] -> [Char]

For example, `fmap @[]` specializes the `fmap` function to only work over lists (otherwise it works over all foldable types). The types are specified in the order of declaration of their corresponding type varaibles, including the type varaibles mentioned in the constraint context. To skip an type in the explict type application use `@_`. For example, `fmap @_ @Int` specializes fmap to work over foldable types that hold integers.


With the move that equalizes types and kinds, the same syntax is also used to explicitly spacify a kind.

## Weak Head Normal Form
As opposed to strict PLs, where an expression is either unevaluated or fully evaluated, Haskell's evaluation process is far more segmented. Haskell expressions undergo several intermediary forms as they progress from virginal (untouched) to fully evaluated values. Expressions can be considered as if having several value layers, and the way these layers are stripped (whether all at once, or ever so gently) depends on numerous factors, ranging from the surrounding context to the type of the value itself. WHNF is a stage in the process of evaluating an expression. It is usually related in pattern matching against a particular data ctor. An expression that is evaluated just enough to reveal its data ctor is in weak-head normal form (WHNF). For example, a function that expects an arg of type `Val x`, where `Val` is the data ctor, and it declares the corresponding parameter as `x` i.e. using an irrefutable pattern, then the arg is not even smelled - no evaluation of the arg happens (it may even be undefined). However, if the param binds the arg using a pattern match against the data ctor, as `(Val x)` then the arg has to undergo evaluation until it reveals the expected `Val` data ctor, or it fails to pattern-match (maybe because the arg was some other type). If the pattern match fails, the pattern in the next equation is tried (and so on), but, at that point the arg has already lost a few layers, it got peeled just enough to reveal whether it was a data ctor, so the evaluation might proceed from there or the arg may be evaluated enough at that point to determine if it matches.

## Wobbly type
The type annotations written by the user are referred to as *rigid*, while types left for GHC to infer are called wobbly. For instance, a type variable that's a part of the user-specified signature is a rigid type variable, while the inferred one is wobbly. Unlike the rigid type, the wobbly types are rarely mentioned in the type error messages.

## Zero-cost coercions
Haskell supports zero-cost coercions, a mechanism where types that share the same run-time representation may be freely converted between. To make sure such conversions are safe and desirable, a role system is put in place to manage and prohibit invalid coercions.
