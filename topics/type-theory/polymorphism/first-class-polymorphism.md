# First-class polymorphism with type inference
paper by 
`First-class polymorphism with type inference` - Mark P. Jones, 1997

## Abstract

Haskell encourages the view of values as first-class entities (such entities can be passed into and returned from functions, stored as components of data structures, etc.). Haskell also supports parametric polymorphism, which allows the use of values that behave uniformly over a range of different types. However, the combination of these two features is not supported - polymorphic values are not first-class!

This restriction is sometimes attributed to the dependence of FP languages like Haskell and ML on type inference, in contrast to more expressive, explicitly typed languages, like `System F`, that have support for first-class polymorphism.

This paper uses the relation between types and logic to develop a __`FCP` type system__ that supports *first-class polymorphism with type inference*, and also *first-class abstract datatypes*.

The immediate result is a more expressive language, but there are also long term implications for language design.

## 1. Introduction

Programming languages gain flexibility and orthogonality by allowing values to be treated as first-class entities. Such values can be passed as arguments or results of functions, or be stored and retrieved as components of data structures. In functional languages, the former often implies the latter: values are stored in a data structure by passing them as arguments to constructor functions, and retrieved using selector functions.

In languages like ML and Haskell, new types of first-class value are specified by giving the names and types for their ctors.

For example, the declaration for `List` introduces a new unary type ctor `List` with two data ctor functions, nullary `Nil` and binary `Cons`. Pattern matching is then used to deconstruct list values and to define other operations on lists.

```hs
data List a = Nil | Cons a (List a)

nil :: forall a. List a
nil = Nil

cons :: forall a. a -> List a -> List a
cons = Cons

head :: forall a. List a -> a
head (Cons x xs) = x

length :: forall a. List a -> Int
length Nil = 0
length (Cons x xs) = 1 + length xs
```

All of these functions have polymorphic types, which indicate that they work in a uniform manner, independently of the type of elements in the lists concerned.

An important property of languages based on the Hindley-Milner type system is that most principal types for functions like these can be automatically inferred from the types of the data ctors (like Nil and Cons). There is no need for further type annotations.

At the same time, the typing discipline provides a guarantee of soundness or type security; the execution of a well-typed program will not "go wrong".

Combining these attractive features, the Hindley-Milner type system has been adopted as the basis for a number of different programming languages.

However, the Hindley-Milner type system has a significant limitation: polymorphic values are not first-class.

Formally, this is captured by making a distinction between *monomorphic types* and *polymorphic type schemes*.

>Universal quantifiers (signalling polymorphism) can only appear at the outermost level of a type scheme, while quantified variables can only be instantiated with monomorphic types.

To put it another way: first-class values have monomorphic type.

In many applications, this limitation is considered a reasonable price to pay for the convenience of type inference. For example, it is often enough to be able to pass particular monomorphic instances of polymorphic values to and from functions, rather than the polymorphic values themselves.

On the other hand, comparisons with explicitly typed languages, like System F, that do support first-class polymorphism, reveal significant differences in expressiveness.

We will see a number of practical examples in later sections that use first-class polymorphism in essential ways, but cannot be coded in a standard Hindley-Milner type system.

## Contributions of this paper

In this paper, we show that polymorphic values can be used as first-class objects in a language with an effective type inference algorithm, *provided that we are prepared to package them up as datatype components*.

The constructs that are used to build and extract these components serve as an alternative to type annotations; they allow us to define and use first-class polymorphic values without sacrificing the simplicity and convenience of type inference.

Although the notation is different, the use of special constructs to package and unwrap polymorphic values is not new. For example, System F uses type abstraction to build polymorphic values and type application to instantiate them. However, in contrast with our approach, the standard presentation of System F requires explicit type annotations for every λ-bound variable. Also, the type inference problems for implicitly and partially typed variations of System F are undecidable.

Our approach is inspired by rules from predicate calculus that are used to convert logical formulae to prenex form, with all quantifiers at the outermost level.

https://en.wikipedia.org/wiki/Prenex_normal_form

This leads to a system that allows both universal and existential quantifiers in the types of datatype components.

The former provides support for first-class polymorphism, while the latter can be used to deal with examples of first-class abstract datatypes.

## Eliminating nested quantifiers

If we cannot work with nested quantifiers, then perhaps we can find a way to eliminate them.

There is a well-known procedure in predicate logic for converting an arbitrary formula, possibly with nested quantifiers, to an equivalent formula in *prenex normal form* with all quantifiers at the outer-most level.

The process is justified by the following equivalences from classical logic, all subject to the condition that `x` does not appear free in `P`:

On condition that `x ∉ FV(P)`:

1. `(∀x.P → Q)` ≡ `P → (∀x.Q)`
2. `(∀x.Q → P)` ≡ `(∃x.Q) → P`

3. `(∃x.P → Q)` ≡ `P → (∃x.Q)`
4. `(∃x.Q → P)` ≡ `(∀x.Q) → P`

Using the Curry-Howard isomorphism as a bridge between logic and type theory, we can use (1) to justify the equivalence:

`∀a. ∀x. T a -> τ` ≡ `∀a. T a -> (∀x.τ)`     (a)

This result tells us that we can convert an arbitrary term of one type to a corresponding term of the other type.

In this case, the equivalence allows us to deal with *selectors for datatypes with universally quantified components*; that is, with accessor functions whose types are of the form on the rhs, by treating them as values of the prenex form type on the lhs.

Similarly, (2) suggests a simple treatment for constructors of datatypes with existentially quantified components:

`∀a. ∀x. τ -> T a` ≡ `∀a. (∃x.τ) -> T a`     (b)

We might hope that the two remaining equations could be used to deal with *selectors for datatypes with existentially quantified components*, and with *constructors for datatypes with polymorphic components*, respectively. Alas, the CHI deals with the relation between TT and IL (not classical predicate logic), and (3) and (4) are not valid in IL.

Instead, we are forced to make do with weaker implications.

For example, the closest we can get to the classical equivalence of (3)    
`(∃x.P -> Q)` ≡ `(P -> ∃x.Q)`   
is the implication 

`(∃x.P -> Q) -> (P -> ∃x.Q)`  (3.1)

A term with the type shown on the rhs, `P -> ∃x.Q`, is a function that, 
for each argument of type `P` returns a result of some type [x:=τ]Q. 
*The choice of the otherwise-unspecified witness type `τ` may depend on the particular value that the function is applied to*.

Different argument values may produce results of different types. This is exactly the behaviour that we expect for a selector function for a datatype with an existentially quantified component. 

But compare this with the type on the lhs of the implication; the position of the quantifier in the formula `(∃x.P -> Q)` *indicates that the choice of a witness type `τ` is independent of any particular arg value of type `P`*.

Clearly, the two types are not equivalent, although the implication tells us that we can convert an arbitrary term of the lhs type to a term of the rhs type.


In a similar way, the closest that IL gets to the classical equivalence of (4)
`(∃x.Q → P)` ≡ `(∀x.Q) → P`

is an implication:

`(∃x.Q -> P) -> (∀x.Q) -> P` (4.1)


It follows that certain terms of type `∀a. ∃x.(τ -> T a)`
can be  substituted for terms of type `∀a.(∀x.τ) -> T a`

`∀a. ∃x.(τ -> T a)` ≡ `∀a.(∀x.τ) -> T a` (c)

the type of a constructor function for a datatype with a polymorphic component.

Unfortunately, it does not help us to determine which terms of the latter type can be represented in this way. And even if that were not a problem, we would still need some form of top-level, existential quantification - a feature that is not usually supported in Hindley-Milner style type systems.

We find ourselves in a rather frustrating situation. If we are restricted to a language of types that allows only outermost quantification, then 
*we can select from, but not construct datatypes with polymorphic components*, and 
*we can construct, but not select from datatypes with existentially quantified components*.

We cannot meet our goals if constructors and selectors are to be treated as normal, first-class functions.




`∀a. ∀x. T a -> τ`  ≡ `∀a. T a -> (∀x.τ)`     (a)
`∀a. ∀x. τ -> T a`  ≡ `∀a. (∃x.τ) -> T a`     (b)
`∀a. ∃x.(τ -> T a)` ≡ `∀a.(∀x.τ) -> T a`      (c)
