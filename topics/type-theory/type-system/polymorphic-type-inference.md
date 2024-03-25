# Polymorphic type inference

Basic polymorphic type-checking, Luca Cardelli, 1987

## Introduction

Polymorphic means to have many forms. As related to PLs, it refers to data or programs which have many types, or which operate on many types. There are several arbitrary ways in which programs can have many types; we are mostly interested in a particularly orderly form of polymorphism called *parametric polymorphism*. This is a property of programs which are parametric with respect to the type of some of their identifiers.

There are two major ways of achieving parametric polymorphism which are conceptually related but pragmatically very different: explicit and implicit polymorphism.

**Explicit parametric polymorphism** is when the parametrization is obtained by explicit type parameters in procedure headings, and corresponding explicit applications of type arguments when functions are called. In this case, parametric polymorphism reduces to the notion of having parameters of type `Type` (without necessarily adopting the notion that `Type` has itself type `Type`). Here is a definition of the polymorphic identity function with explicit type parameters (where `fun` stands for lambda-abstraction, both at the term, `λ`, and type level, `Λ`), followed by its application to an `int` and a `bool`:

```
id :: forall (a :: Type). a -> a

let id = fun (t: type) fun (a: t) a
id int  3
id bool true
```

**Implicit parametric polymorphism** is when type parameters and type applications (as in the example above) are not admitted; instead, types can contain type vars which are as-of-yet-unknown, to be determined types. If a function's parameter has a type var or a term containing type vars as its type, then that function can be applied to args of many different types. Here is the implicit version of the polymorphic identity function (where `α` is a type var), and its application to an integer and a boolean.

```
id :: a -> a

let id = fun (a: α) a
id 3
id true
```

Implicit polymorphism can be considered as an abbreviated form of explicit polymorphism, where the type parameters and applications have been omitted and must be rediscovered by the type inference. Omitting type parameters leaves some type-denoting identifiers unbound; and these are precisely the type vars. Omitting type args requires type inference to recover the lost information.

In fact, in implicit polymorphism, one can totally omit type information by interpreting the resulting programs as having type variables associated to parameters and identifiers. The programs then appears to be type-free, but rigorous type-checking can still be performed. This is one of the most appealing properties of implicit polymorphism, which makes it particularly appealing for interactive systems and for naïve users. Here is the type-free definition of the polymorphic identity, where all the type information has been omitted.

```
id :: a -> a

let id = fun(a) a
```

Explicit polymorphism is more expressive, in that it can type programs which cannot be typed by implicit polymorphism, but it is more verbose. In practice, even in explicit polymorphism one may want to omit some type information, and this creates a grey region between fully explicit and fully implicit polymorphism. In this grey region, the type-inference techniques used for implicit polymorphism can be useful, and this is a good reason for studying implicit polymorphism even in the context of explicit polymorphism. For example, a reasonable compromise could be to adopt explicit-style function declarations, but then use implicit-style function applications, using inference to recover the missing information:

```
id :: forall (a :: Type). a -> a

let id = fun(t: type) fun(a: t) a
id 3
id true
```

Implicit polymorphism can be understood in its own right, both at the semantic and type-inference levels. But it is, in a sense, ambiguous: the same implicitly polymorphic program may correspond to different explicitly polymorphic programs. This ambiguity can be critical in some extensions of the basic type system, noticeably in presence of side-effects. An understanding of relations between implicit and explicit polymorphism may be necessary when extending implicit polymorphic systems in certain ways.

## History

Polymorphic typing of programs was envisioned by C. Strachey; his lecture notes on fundamental concepts in PLs [Strachey67] already contains much of the notation and terminology used today.

Polymorphic types were already known as *type schemas* in combinatory logic [Curry58].

Extending Curry's work, and collaborating with him, R. Hindley introduced the idea of a *principal type schema*, which is the most general polymorphic type of an expression; he also showed that if a combinatorial term has a type, then it has a principal type [Hindley69].

In doing so, Hindley used a result by Robinson about the existence of *the most general unifiers* in the *unification algorithm* [Robinson64]. These results contained all the germs of polymorphic typechecking, including the basic algorithms. The existence of principal types means that a type inference algorithm will always compute a unique "best" type for a program; moreover, *unification* can be used to perform this computation. However, these results did not immediately influence the community, because of their theoretical setting.

Influenced by Strachey, and independently from Hindley, Milner rediscovered many of these ideas in the context of the `LCF` proof generation system [Gordon79], which included the first version of the ML language [Milner84]. Milner has introduced a crucial extension to Hindley's work: *the notion of generic and non-generic type variables*, essential for handling declarations of polymorphic functions. Milner has also implemented the first practical polymorphic typechecker, and proved the soundness of the type system [Milner78].

Milner and Damas have proved the *principal-type property* of Milner's extended system [Damas82], theresult which implied the decidability of the type system. This style of polymorphic typechecking was soon adopted by the language `Hope` [Burstall80], with many FPLs following suite.

During that initial development of `ML`, it was discovered that the introduction of side-effects made the type system unsafe [Gordon79]. This was resolved in a rather ad-hoc way. The situation was later somewhat improved by Damas, but smooth merging of side-effects and implicit polymorphic typechecking remained an open problem.

Much theoretical work followed, with Coppo showing how to define a coherent, albeit undecidable, type system that is more flexible than ML's [Coppo80].

The ideal model of types [MacQueen84] came to be the model which would offer stronger support for implicit polymorphic types, founded on the ideas in the original papers of Scott and Milner.

The explicit kind of polymorphism also had its share of milestones, extensively analyzed in [Bruce84], with many examples by [Cardelli86].

The relations between implicit and explicit polymorphism are actively being investigated still, see [McCracken84].


## Pragmatic motivation

Parametrically polymorphic type systems share with `Algol68` the features such as *static type inference*, *static type checking*, and the treatment of *higher-order functions*, but are more flexible in their ability to define functions which work uniformly on arguments of many types.

Polymorphism in PL comes from the interaction of two contrasting design goals: static typing and reusability.

*Static typing* is the ability to determine the absence of certain classes of run-time faults by inspecting the program at compile-time. Static typing is firmly established as a fundamental tool in building large, highly structured and reliable software systems.

*Reusability* is the ability to write reusable code with an open-ended collection of applications in mind. Particularly, we'd like our functions to work without modification on newly defined types. Reusability is also important in building large projects as it helps define abstractions and leads to a better system structuring.

These two design goals are in contrast because static typing tends to prevent reusability, and reusable programs are not easy to check statically. A routine in `Pascal` to sort integers cannot be generalized to sort strings and other ordered sets, since Pascal's type system doesn't allow their parameterization. On the other hand, a `Lisp`'s routine to sort integers can be reused on many different kinds of ordered sets, but can also be mis-applied to any arbitrary data structure, with unpredictable results. Polymorphic type systems try to reconcile these two goals by providing all the safety of statically typed languages, and desirable properties of the dynamically typed PLs.

In this paper we discuss Milner's polymorphic typechecking algorithm, which has proved very successful: it is sound, efficient, and supports a very rich and flexible type system.

Great progress has been made recently in polymorphic languages, but one feature remains unique to Milner's algorithm: its ability to infer types in the absence of type declarations. This feature comes for free. In the attempt to deal with programs which can be reused on many types, the algorithm searches for the best (most abstract) type of a program. Such best type is independent of type declarations, which can only be used to reduce the generality of the most abstract type.

This property makes Milner's algorithm particularly suitable for interactive languages (ML itself is an interactive compiled language). Interactive users rarely have to bother writing down type information, which is automatically inferred and checked. This strongly contributes to ML's feel of care-free, quick-turnaround language, which is wrongly associated only with interpretive,
untyped languages.

The pragmatics of polymorphic typechecking has so far been restricted to a small group of people. The only published description of the algorithm is the one in [Milner 78] which is ratherPage 5 technical, and mostly oriented towards the theoretical background. In the hope of making the algorithm accessible to a larger group of people, we present an implementation (in the form of a Modula-2 program) which is very close to the one used in LCF, Hope and ML [Gordon 79,Burstall 80, Milner 84]. Although clarity has sometimes been preferred to efficiency, this implementation is reasonably efficient and quite usable in practice for typechecking large programs.

Only the basic cases of typechecking are considered, and many extensions to common PL constructs are fairly obvious. The major non-trivial extensions which are known so far (and not discussed here) concern overloading, abstract data types, exception handling, updatable data, and labeled record and union types. Many other extensions are being studied.

We present two views of typing, as a system of type equations and as a type inference system, and attempt to relate them informally to the implementation. 

## A simple applicative language

We do not deal here with ML directly, which is a full-size programming language; instead we considered a simple typed λ-calculus with constants, constituting what can be considered the kernel of the ML language. (The evaluation mechanism (call-by-name or call-by-value) is immaterial for the purpose of typechecking). The concrete syntax of expressions is given below, where Ide are identifiers, Exp are expressions, Decl are declarations and fun stands for λ. All identifiers declared in the same Decl must be distinct. The corresponding abstract syntax is given by the types Exp and Decl in the program in appendix (parsers and printers are not provided).

```
data Exp
  = Ide
  | "if" Exp "then" Exp "else" Exp
  | "fun" "(" Ide ")" Exp
  | Exp "(" Exp ")"
  | "let" Decl "in" Exp
  | "(" Exp ")"

data Decl
  = Ide "=" Exp
  | Decl "then" Decl
  | "rec" Decl
  | "(" Decl ")"
```

Data types can be introduced into the language simply by having a predefined set of identifiers in the initial environment; this way there is no need to change the syntax or, more importantly, the typechecking program when extending the language. As an example, the following program defines the factorial function and applies it to zero, assuming that the initial environment contains integer constants and operations:

```
let rec factorial =
fun(n)
if zero(n)
then succ(0)
else times(n)(factorial(pred(n)))
in factorial(0)
```

## Types

A type can be either a type variable α, β, etc., standing for an arbitrary type, or a type
operator. Operators like int (integer type) and bool (boolean type) are nullary type operators.
Parametric type operators like → (function type) or × (cartesian product type) take one or more
types as arguments. The most general forms of the above operators are α → β (the type of any
function) and α × β, (the type of any pair of values); α and β can be replaced by arbitrary types to
give more specialized function and pair types. Types containing type variables are called
polymorphic, while types not containing type variables are monomorphic. All the types found in
conventional programming languages, like Pascal, Algol 68 etc. are monomorphic.
Expressions containing several occurrences of the same type variable, like in α → α, express
contextual dependencies, in this case between the domain and the codomain of a function type.
The typechecking process consists in matching type operators and instantiating type variables.
Whenever an occurrence of a type variable is instantiated, all the other occurrences of the same
variable must be instantiated to the same value: legal instantiations of α → α are int → int, bool →
bool, (β × γ) → (β × γ), etc. This contextual instantiation process is performed by unification,
[Robinson 65] and is at the basis of polymorphic typechecking. Unification fails when trying to
match two different type operators (like int and bool) or when trying to instantiate a variable to a
term containing that variable (like α and α → β, where a circular structure would be built). The
latter situation arises in typechecking self-application (e.g. fun(x) x(x)), which is therefore
considered illegal.
Here is a trivial example of typechecking. The identity function Id = fun(x) x has type α → α
because it maps any type onto itself. In the expression Id(0) the type of 0 (i.e. int) is matched to
the domain of the type of Id, yielding int → int, as the specialized type of Id in that context. Hence
the type of Id(0) is the codomain of the type of Id, which is int in this context.
In general, the type of an expression is determined by a set of type combination rules for the
language constructs, and by the types of the primitive operators. The initial type environmentPage 7
could contain the following primitives for booleans, integers, pairs and lists (where → is the
function type operator, × is cartesian product, and list is the list operator):
true, false : bool
0, 1, … : int
succ, pred : int → int
zero : int → bool
pair : α → (β → (α × β))
fst : (α × β) → α
snd : (α × β) → β
nil : α list
cons : (α × α list) → α list
hd : α list → α
tl : α list → α list
null : α list → bool
The type α list is the type of hom
