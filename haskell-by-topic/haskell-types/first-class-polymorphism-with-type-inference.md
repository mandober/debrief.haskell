# First-class Polymorphism

* "First-class Polymorphism with Type Inference", Mark P. Jones, 1997
http://web.cecs.pdx.edu/%7Empj/pubs/fcp.html

## Abstract

Languages like ML and Haskell encourage the view of values as first-class entities that can be passed as arguments or results of functions, or stored as components of data structures. The same languages offer parametric polymorphism, which allows the use of values that behave uniformly over a range of different types. But the combination of these features is not supported - *polymorphic values are not first-class*. This restriction is sometimes attributed to the dependence of such languages on type inference, in contrast to more expressive, explicitly typed languages, like System F, that do support first-class polymorphism. This paper uses relationships between types and logic to develop a type system, `FCP`, that supports *first-class polymorphism*, type inference, and also *first-class abstract datatypes*. The immediate result is a more expressive language, but there are also long term implications for language design.


## 1. Introduction

Programming languages gain flexibility and orthogonality by allowing values to be treated as first-class entities. Such values can be passed as arguments or results of functions, or be stored and retrieved as components of data structures. In FP, the former often implies the latter; values are stored in a data structure by passing them as arguments to constructor functions, and retrieved using selector functions.

In a language like Haskell, new types of first-class value are specified by giving the names and types for their constructors. For example, a definition:
```hs
data List a = Nil | Cons a (List a)
```

introduces a new, unary type constructor, `List`, with two data ctor functions:
```hs
Nil :: forall a. List a
Cons :: forall a. a -> List a -> List a.
```

Pattern matching allows the definition of *selectors* (`head` and `tail`), and other useful operations on lists (such as `length`):

```hs
head :: forall a. List a -> a
head (Cons x _) = x

tail :: forall a. List a -> List a
tail (Cons _ xs) = xs

length :: forall a. List a -> Int
length Nil         = 0
length (Cons x xs) = 1 + length xs
```

All of these functions have *polymorphic types*, which indicate that they work in a *uniform manner*, independently of the type of elements in the lists.

An important property of languages based on the *Hindley-Milner type system* is that *the most general (principal) types* for functions like these can be inferred automatically from the types of the constructors `Nil` and `Cons`; there is no need for further type annotations.

At the same time, the typing discipline provides a guarantee of soundness or type security; the execution of a well-typed program will not "go wrong". However, the HM type system has a significant limitation: *polymorphic values are not first-class values*.

Formally, this is captured by making a distinction between *monomorphic types* and *polymorphic type schemes*.

Universal quantifiers, signalling polymorphism, can only appear at the outermost level of a type scheme, and quantified variables can only be instantiated with monomorphic types. To put it another way,
>first-class values have monomorphic type.

In many applications, this limitation is considered a reasonable price to pay for the convenience of type inference.

For example, it is often enough to be able to pass particular monomorphic instances of polymorphic values to and from functions, rather than the polymorphic values themselves.

On the other hand, a comparison with an explicitly typed language, like System F, that supports first-class polymorphism, reveals significant differences in expressiveness.

We'll see a number of practical examples in later sections that use first-class polymorphism in essential ways, but cannot be coded in a standard HM system.

### 1.1 This paper

In this paper we show that polymorphic values can be used as first-class objects in a language with an effective type inference algorithm, provided that we are prepared to package them up as datatype components.

The constructs used to build and extract these components serve as an alternative to type annotations; they allow us to define and use first-class polymorphic values without sacrificing the simplicity and convenience of type inference.

Although the notation is different, the use of special constructs to package and unwrap polymorphic values is not new. System F uses type abstraction to build polymorphic values, and type application to instantiate them.

However, in contrast with our approach, the standard presentation of System F requires explicit type annotations for every λ-bound variable, and the type inference problems for implicitly and partially typed variations of System F are undecidable.

Our approach is inspired by rules from predicate calculus that are used to convert logical formulae to *prenex normal form*, with all quantifiers at the outermost level.

This leads to a system that allows both universal and existential quantifiers in the types of datatype components. The former provides support for *first-class polymorphism*, the latter for *first-class abstract datatypes*.

## 2. Quantified component types

Consider a simple datatype definition:

```hs
data T a = C τ
```

which introduces a new type ctor `T`, the data ctor `C`, and the selector function `unC` with types

```hs
C   :: forall a. τ → T a
unC :: forall a. T a → τ
```

In both types, the quantified variable `a` ranges over arbitrary monomorphic types, so the components of any data structure that we build or access using these functions must have monomorphic types.

The purpose of this section is to show how these ideas can be extended to datatypes with quantified component types. Specifically, we are interested in a system that allows definitions of the form:

```hs
data T a = C (𝓠 x. τ)
-- data T a = C (forall x. τ)
-- data T a = C (exists x. τ)
```

for some quantifier `𝓠 ∈ {∀, ∃}`. This would give constructor and selector functions with types of the form:

```hs
C   :: ∀a. (𝓠 x. τ) → T a
unC :: ∀a. T a → (𝓠 x. τ)

-- C   :: forall a. (𝓠 x. τ) → T a
-- C   :: forall a. (forall x. τ) → T a
-- C   :: forall a. (exists x. τ) → T a

-- unC :: forall a. T a → (𝓠 x. τ)
-- unC :: forall a. T a → (forall x. τ)
-- unC :: forall a. T a → (exists x. τ)
```

A general treatment of the nested quantifiers in these types would make it difficult to deal with type inference, and could lead to undecidability. But our goal is more modest - nested quantifiers are used only in the types of constructors and selectors - so it is reasonable to hope that we can make some progress.

Indeed, we are not the first to consider extensions of the HM type system that support datatypes with quantified component types. For example, Perry and Laufer have each described extensions of Milner's type inference algorithm that allow datatypes with existentially quantified components. Remy used an extension of ML with both universally and existentially quantified datatype components to model aspects of OOP, but did not discuss type inference. Recent work by Odersky and Laaufer has similar goals to the present paper and permits universal quantification of datatype fields, with an encoding to simulate existential quantification. However, their approach requires a significant extension of the usual type inference mechanisms - replacing unification with instantiation. We believe that the approach described here is simpler, achieving the same degree of expressiveness, but based on techniques that are easier to use and easier to implement.

## 2.1 Eliminating nested quantifiers

If we cannot work with nested quantifiers, then perhaps we can find a way to eliminate them. There is a well-known procedure in predicate logic for converting an arbitrary **formula**, possibly with nested quantifiers, to equivalent formula in *prenex form* with all quantifiers at the outermost level.

The process is justified by the following equivalences from **classical logic**, all subject to the *condition that `x` does not appear free in `P`*:

- `P` and `Q` are formulas, not predicates!
- would be clearer to use `ϕ` and `ψ` as metavars for formulas
- `x ∉ FV(P)`, but hopefully `x ∈ FV(Q)` denoted by `Qˣ`

```hs

(∀x. P ⇒ Qˣ)  ≡  P ⇒ (∀x. Qˣ)         -- (1)    x ∉ FV(P)
(∀x. Qˣ ⇒ P)  ≡  (∃x. Qˣ) ⇒ P         -- (2)    x ∉ FV(P)

(∃x. P ⇒ Qˣ)  ≡  P ⇒ (∃x. Qˣ)         -- (3)    x ∉ FV(P)  *** invalid in IL
(∃x. Qˣ ⇒ P)  ≡  (∀x. Qˣ) ⇒ P         -- (4)    x ∉ FV(P)  *** invalid in IL
```

Using the Curry-Howard isomorphism as a bridge between logic and type theory, we can use the equation (1) to justify the equivalence:

```hs
(∀a. ∀x. T a → ϕ)  ≡  ∀a. T a → (∀x. ϕ)
```

This result tells us that we can convert an arbitrary term of one type to a corresponding term of the other type. In this case, the equivalence allows us to deal with selectors for datatypes with universally quantified components, that is, with *functions whose types are of the form on the rhs, by treating them as values of the prenex form type on the lhs*.

In a similar way, the equation (2) suggests a simple treatment for constructors of datatypes with existentially quantified components:

```hs
(∀a. ∀x. τ → T a)  ≡  ∀a. (∃x. τ) → T a
```

We might hope that the two remaining equations could be used to deal with selectors for datatypes with existentially quantified components, and with constructors for datatypes with polymorphic components, respectively. But the CHI deals with the relationship between type theory and *intuitionistic logic*, rather than classical logic, and the equations (3) and (4) are not valid in IL.

Instead, we are forced to make do with weaker implications.

For example, the closest that we can get to the classical equivalence of the equation (3) is the implication (3'):

```hs
(∃x. P ⇒ Qˣ)  ≡  P ⇒ (∃x. Qˣ)    -- (3)   x ∉ FV(P)      invalid in IL
(∃x. P ⇒ Qˣ)  ⇒  P ⇒ (∃x. Qˣ)    -- (3')  x ∉ FV(P)        valid in IL
```

A term with the type on the rhs is a function that, for each argument of type `P`, returns a result of some type `[τ/x]Q`. The choice of the *otherwise- unspecified witness type* `τ` may depend on the particular value that the function is applied to; different argument values may produce results of different types. This is exactly the behaviour that we expect for a selector function of a datatype with an existentially quantified component.

But compare this with the type on the lhs of the implication; the position of the quantifier in the formula, `∃x. P ⇒ Q`, indicates that the choice of a witness type `τ` is independent of any particular argument value of type `P`.

Clearly, the two types are not equivalent, although the implication tells us that we can convert an arbitrary term of the lhs type to a term of the rhs type.


In a similar way, the closest that IL gets to the classical equivalence of the equation (4) is an implication:

```hs
(∃x. Q ⇒ P)  ≡  (∀x. Q) ⇒ P     -- (4)  x ∉ FV(P)  *** invalid in IL
(∃x. Q ⇒ P)  ⇒  (∀x. Q) ⇒ P    -- (4')  x ∉ FV(P)        valid in IL
```

It follows that certain terms of type 
`∀a. ∃x. (τ → T a)` can be substituted for terms of type 
`∀a. (∀x. τ) → T a`, 
the type of the data ctor function for a datatype with a polymorphic component.

Unfortunately, it does not help us to determine which terms of the latter type can be represented in this way. 

And even if that were not a problem, we'd still need some form of the toplevel, existential quantification - a feature that is not usually supported in HM style type systems.

This is frustrating: if we are restricted to a language of types 
that allows only outermost quantification, then 
we can select from, 
 but not construct datatypes 
  with *universally quantified components*; and 
we can construct, 
 but not select from datatypes 
  with *existentially quantified components*.

We cannot meet our goals if constructors and selectors are to be treated as normal, first-class functions.

## 2.2 Special syntax for constructors

The problems that we have seen are the result of restricting our attention to types in the *prenex form*.

In the context of logic, such restrictions seem rather artificial: 
given `(∀x.Q) ⇒ P` as a hypothesis, 
we can try to construct a proof of `(∀x.Q)` 
and then deduce `P` as a conclusion, 
as in the following derivation:

```
                   A |- Q     x ∉ A
                  ------------------ ∀I
A |- (∀x.Q) ⇒ P       A |- ∀x.Q
------------------------------------ →E
                 P
```

Switching back from logic to terms and types, this is exactly the structure that we need to deal with a constructor for a datatype with a universally quantified component:

```
                   A |- E : τ     x ∉ FV(P)
                  ------------------------- ∀I
K : (∀x.Q) -> T a       A |- E : ∀x. τ
------------------------------------------- →E
                A |- K E : T a
```

The remaining problem, to provide access to a value stored in a datatype with an existentially quantified component, can also be dealt with by introducing a new syntactic construct. In this case, our inspiration comes from the *elimination rule for existential quantification*:

```
∃x.P     ∀x.P ⇒ Q     x ∉ P
----------------------------- ∃E
             Q
```

If we consider a constructor `K : (∀x. τ) → T a`, then we obtain a typing rule:

```
                       A,x : τ |- E' : τ'
                      -------------------------- →I
A |- E : T a           A |- λz. E' : τ → τ'
------------------    -------------------------- ∀I
A |- K⁻¹ E : ∃x. τ     A |- λz. E' : ∀x. τ → τ'
------------------------------------------------ ∃E
        A |- (case E of (K z) → E') : τ'
```

with the side condition that `x ∉ FV(A, τ')`.

An attractive feature of this approach is that soundness of our typing rules follows directly from the soundness of the corresponding rules in IPL.

We now have the key to understanding FCP, the type system introduced in this paper. By abandoning the first-class status of constructors and introducing new syntactic constructs in their place, we obtain the tools that we need to construct and access datatypes with universally or existentially quantified components.

## 3. Examples
