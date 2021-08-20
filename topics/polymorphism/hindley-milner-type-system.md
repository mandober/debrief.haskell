# Hindley-Milner type system

https://en.wikipedia.org/wiki/Hindley-Milner_type_system

https://stackoverflow.com/questions/12532552/what-part-of-hindley-milner-do-you-not-understand

## Contents

- Introduction
- Monomorphism vs. polymorphism
- Let-polymorphism
- Overview
- The Hindley-Milner type system
  - Syntax
    - Monotypes
    - Polytypes
    - Context and typing
    - Free type variables
  - Type order
    - Principal type
    - Substitution in typings
  - Deductive system
    - Typing rules
  - Let-polymorphism
  - Generalization rule
- An inference algorithm
4.1 Degrees of freedom choosing the rules
4.2 Syntax-directed rule system
4.3 Degrees of freedom instantiating the rules
4.4 Algorithm J
5 Proving the algorithm
5.1 Algorithm W
5.2 Proof obligations
6 Extensions
6.1 Recursive definitions
6.1.1 Typing rule
6.1.2 Consequences
6.2 Overloading
6.3 Higher-order types
6.4 Subtyping
7 Notes


## Introduction

A Hindley-Milner (HM) type system is a classical type system for *the lambda calculus with parametric polymorphism*.

It is also known as Damas-Milner or Damas-Hindley-Milner. The origin is the type inference algorithm for the simply typed lambda calculus that was devised by Haskell Curry and Robert Feys in 1958. In 1969, J. Roger Hindley extended this work and proved that their algorithm always inferred the most general type. In 1978, Robin Milner, independently of Hindley's work, provided an equivalent algorithm, `Algorithm W`. In 1982, Luis Damas finally proved that Milner's algorithm is complete and extended it to support systems with polymorphic references. `Algorithm W` is an efficient type inference method in practice, and has been successfully applied on large code bases, although it has a *high theoretical complexity*.

HM type inference is `DEXPTIME`-complete. In fact, merely deciding whether an ML program is typeable (without having to infer a type) is itself DEXPTIME-complete. Non-linear behaviour does manifest itself, yet mostly on pathological inputs. Thus the complexity theoretic proofs by Mairson (1990) and Kfoury, Tiuryn & Urzyczyn (1990) came as a surprise to the research community.

Among HM's more notable properties are its completeness and its ability to infer the most general type of the types of variables, expressions and functions in a given program without programmer-supplied type annotations or other hints. Being *scope sensitive*, it is not limited to deriving the types only from a small portion of source code, but rather from complete programs or modules. Being able to cope with parametric types, too, it is core to the type systems of many FPL. It was first implemented as part of the type system of the programming language ML. Since then, HM has been extended in various ways, most notably with type class constraints like those in Haskell.

## Monomorphism vs. polymorphism

In the simply typed lambda calculus, types, `T`, are either *atomic type constants* (base types) or *function types* of form `T -> T`. Such types are *monomorphic*. Typical examples are the types used in arithmetic values:

```hs
3       : Number
add 3 4 : Number
add     : Number -> Number -> Number
```

Contrary to this, the untyped lambda calculus is insensitive to typing, so functions can be (meaningfully) applied to any type of (function) argument (there are only functions in the λ-calculus). The trivial example is the identity function, `λx.x`, that returns whatever arg it is applied to. Less trivial examples include parametric types like lists.

While polymorphism in general means that functions (operations) accept values of more than one type, the type of polymorphism in the λ-calculus, although it is implicit, is *parametric polymorphism*. Parametrically polymorphic variables are also referred to as *type schemes* in order to emphasize the parametric nature of the polymorphism. Also, the type schemes are implicitly universally quantified meaning they they refer to any and all conceivable types.

```hs
cons :: forall a. a -> List a -> List a
nil  :: forall a. List a.
id   :: forall a. a -> a
```

Polymorphic types (type schemes) become monomorphic by *consistent substitution* of their variables. Examples of monomorphic instances of the example functions above are:

```hs
cons' :: Char -> List Char -> List Char
nil'  :: List Number
id'   :: String -> String
```

In fact, a polymorphic function, because of its universally quantified variable, also gets an extra initial argument that is a type (each quantified variable needs to be instantiated with a type). `System F` makes this explicit and requires the instantiation of the type variable when appying the function.

```hs
cons :: ∀a. a -> List a -> List a

-- explicit type var instantiation with a String type
-- argument, a string value and a string list value:
x1 = cons @String "universally" ["quantified", "variable"]

-- type scheme instatiated at Char
consChars :: Char -> List Char -> List Char
consChars = cons @Char

-- type scheme instatiated at Int
consInt :: Int -> List Int -> List Int
consInt = cons @Int
```

In Haskell, absent of explicit quantification, the polymorphic variables are implicitly universally quantified. Also, Haskell doesn't insist on the explicit type instantiation when applying a function, but it allows it, using the `@T` syntax.

> More generally, types are polymorphic when they contain type variables, while types without them are monomorphic.

Contrary to the type systems used for example in Pascal (1970) or C (1972), which only support monomorphic types, HM is designed with emphasis on parametric polymorphism. The successors of the languages mentioned, like C++ (1985), focused on different types of polymorphism, namely subtyping polymorphism in connection with OOP and overloading. While subtyping is incompatible with HM, a variant of systematic overloading is available in the HM-based type system of Haskell.

## Let-polymorphism

When extending the type inference for the STLC towards polymorphism, one has to *define when it is admissible to derive an instance of a value*. Ideally, this would be allowed with any use of a bound variable, as in the following example where the `f` parameter would be instantiated once at Int and once at String:

```hs
-- parameters in lambda-abstractions are treated as being monomorphic,
-- so let-polymorphism woudn't allow this:
x = (λf. (f 3, f "text")) (λx. x)
```

Unfortunately, in this case type inference becomes undecidable.

> In polymorphic λ-calculus type inference is undecidable.

Instead, HM provides *let-polymorphism* by restricting the binding mechanism in the form of the let-polymorphic extension of expression syntax, e.g.:

```hs
-- let-polymorphism
x = let f = λx. x
    in  (f 3, f "text")
```

Only values bound in a `let` construct are subject to instantiation, i.e. are polymorphic (so `f` is polymorphic). On the other hand, the parameters in lambda-abstractions are treated as being monomorphic.


## Overview

The remainder of this article proceeds as follows:
- The HM type system is defined. This is done by describing a deduction system that makes precise what expressions have what type, if any.
- From there, it works towards an implementation of the type inference method. After introducing a syntax-driven variant of the above deductive system, it sketches an efficient implementation (algorithm `J`), appealing mostly to the reader's metalogical intuition.
- Because it remains open whether algorithm `J` indeed realises the initial deduction system, a less efficient implementation (algorithm `W`), is introduced and its use in a proof is hinted.
- Finally, further topics related to the algorithm are discussed.

The same description of the deduction system is used throughout, even for the two algorithms, to make the various forms in which the HM method is presented directly comparable.

## The HM type system definition

The type system can be formally described by syntax rules that fix a language for the expressions, types, etc. The presentation here of such a syntax is not too formal, in that it is written down not to study the surface grammar, but rather the depth grammar, and leaves some syntactical details open. This form of presentation is usual. Building on this, type rules are used to define how expressions and types are related.

### Syntax

* Expressions

```bnf
e := x                      term variable
   | e₁ e₂                  term application
   | λ x . e                term abstraction
   | let x = e₁ in e₂       let-in expression
```

The expressions to be typed are exactly those of the λ-calculus (variables, application and abstraction), extended with let-expressions. Parentheses can be used to disambiguate an expression. The application is left-binding and binds stronger than abstraction or the let-expression.

* Types

```bnf
mono   τ := α               type variable
          | C τ … τ         type application
          | τ -> τ          type abstraction
poly   σ := τ
          | ∀α. σ           quantifier
```

Types are syntactically split into two groups: monotypes and polytypes (polytypes are called *type schemes* in the original HM paper).

### Monotypes

Monotypes always designate a particular type. Monotypes `τ` are syntactically represented as terms.

Examples of monotypes include type constants like `Int` and `String` and parametric types like `Map (Set String) Int`. The latter types are examples of applications of type functions (of function type ctor), for example, from the set `{ Map², Set¹, String⁰, Int⁰, ->² }`, where the superscript indicates the arity.

The complete set of type functions, `C`, is arbitrary in HM, except that it must contain at least `->²` i.e. the type of functions, which is often written in infix notation for convenience. For example, a function mapping integers to strings has type `Int -> String`.

The parametric types `C τ … τ` were not present in the original paper on HM and are not needed to present the method. None of the inference rules below will take care or even note them. The same holds for the non-parametric "primitive types" in said paper. All the machinery for polymorphic type inference can be defined without them. They have been included here for sake of examples but also because the nature of HM is all about parametric types. This comes from the function type `τ -> τ`, hard-wired in the inference rules, below, which already has two parameters and has been presented here as only a special case.

The application binds stronger than the infix arrow, which is right-binding.

Type variables are admitted as monotypes.

> Monotypes are not to be confused with *monomorphic types*, which exclude variables and allow only *ground terms*.

Two monotypes are equal if they have identical terms.

### Polytypes

Polytypes (or type schemes) are types containing variables bound by zero or more universal (forall) quantifiers, e.g. `∀α. α -> α`. A function with polytype `∀α. α -> α` can map any value of the same type to itself, and the identity function is a value for this type.

As another example, `∀α. (Set α) -> Int` is the type of a function mapping all finite sets to integers. A function which returns the cardinality of a set would be a value of this type.

Quantifiers can only appear top level. For instance, a type `∀α. α -> ∀α. α` is excluded by the syntax of types.

Also, monotypes are included in the polytypes, thus a type has the general form `∀α₁ … ∀αₙ . τ` , where `n >= 0` and `τ` is a monotype.

Equality of polytypes is up to reordering the quantification and renaming the quantified variables (α-equivalence). Further, quantified variables not occurring in the monotype can be dropped.


## Context and Typing

```
Value Context    Γ := ϵ
                    | Γ, x : σ
Typing context     := Γ |- e : σ
```

To meaningfully bring together the still disjoint parts (syntax expressions and types) a third part is needed: *context*. Syntactically, a context is a list of pairs `x : σ`, called assignments, assumptions or bindings, each pair stating that value variable `xᵢ` has type `σᵢ`. All three parts combined give a typing judgment of the form `Γ |- e : σ`, stating that under assumptions `Γ`, the expression `e` has type `σ`.

## Free type variables

```
free(α)         = {α}
free(C τ₁ … τₙ) = ⋃ [i=1..n] free(τᵢ)
free(Γ)        = ⋃ [x:σ ∈ Γ] free(σ)

free(∀α.σ)      = free(σ) - {α}
fee(Γ |- e : σ) = free(σ) - free(Γ)
```

* In a type `∀α₁ … ∀αₙ . τ` the symbol `∀` is the quantifier binding the type variables `αᵢ` in the monotype `τ`. The variables `αᵢ` are called quantified and any occurrence of a quantified type variable in `τ` is called *bound* and all unbound type variables in `τ` are called *free*.

* In addition to the quantification `∀` in polytypes, type variables can also be bound by occurring in the context, but with the inverse effect on the right hand side of the `⊢`. Such variables then behave like type constants there.

* Finally, a type variable may legally occur unbound in a typing, in which case they are implicitly universally quantified.

The presence of both bound and unbound type variables is a bit uncommon in PLs. Often, all type variables are implicitly treated as forall quantified. For instance, one does not have clauses with free variables in Prolog. Likewise in Haskell (Haskell provides the `ScopedTypeVariables` language extension allowing to bring all-quantified type variables into scope), where all type variables implicitly occur quantified, i.e. a Haskell type `a -> a` means `∀a. a -> a` aka `∀α. α -> α` here. Related and also very uncommon is the binding effect of the right hand side `σ` of the assignments.

Typically, the mixture of both bound and unbound type variables originate from the use of free variables in an expression. The constant function (maker) `K := λx.λy.x` is one example of this because it has the monotype `α -> β -> α`. Regarding HM restrictions, one can force the (let) polymorphism using a let binding such as `let k = λx. (let f = λy.x in f) in k`; it is in this subexpression that we see that a variable `f` can obtain the type such as `∀γ. γ -> α`, i.e. a type with a free variable, `α`. This free monotype variable `α` originates from the type of the variable `x` bound in the surrounding scope. But the variable `k` that binds the overall expression has the type `∀α∀β. α -> β -> α`. One could assume that the free type variable `α` in the type of `f` was bound by the `∀α` in the outer scope, i.e. in the type of `k`. But such a nested scoping cannot be expressed in HM; instead, the binding is realized by the context.

```hs
k :: a -> b -> a
k = λx. λy. x

let k = λx. (let f = λy. x in f) in k
y :: b
x :: a
f :: ∀b. b -> a
k :: ∀a ∀b. a -> b -> a
```

## Type order

Polymorphism means that one and the same expression can have (perhaps infinitely) many types.

However, in HM type system, all these types are not completely unrelated, but rather orchestrated by the parametric polymorphism.

For example, the identity function `λx.x` can have `∀α. α -> α` as its type, as well as `Int -> Int` and `String -> String` and many others; but not (Int -> String). The most general type for this function is `∀α. α -> α` while the others are more specific and can be derived from the general one by consistently replacing another type for the type parameter, i.e. the quantified variable `α`. The counter-example (Int -> String) fails because the replacement is not consistent.

The consistent replacement can be made formal by applying a substitution
`S = { aᵢ ⟼ τᵢ, … }` to the term of a type `τ`, denoted by `Sτ`. As the example suggests, substitution is not only strongly related to an order, that expresses that a type is more or less special, but also with the all-quantification which allows the substitution to be applied.

Formally, in HM, a type `σ'` is more general than `σ`, formally `σ' ⊑ σ`, if some quantified variable in `σ'` is consistently substituted such that one gains `σ` as shown below. This order is part of the type definition of the type system.

* Specialization rule

```
τ' = { αᵢ ⟼ τᵢ } τ     βᵢ ∉ free(∀α₁ … ∀αₙ . τ)
-------------------------------------------------
        ∀α₁ … ∀αₙ . τ ⊑ ∀β₁ … ∀βₘ . τ'
```

In our previous example, applying the substitution `S = { α ⟼ String }` would result in `∀α. α -> α  ⊑  String -> String`.

While substituting a monomorphic (ground) type for a quantified variable is straight forward, substituting a polytype has some pitfalls caused by the presence of free variables. Most particularly, unbound variables must not be replaced (captured); they are treated as constants here. Additionally, quantifications can only occur top-level. Substituting a parametric type, one has to lift its quantifiers (the legend table makes the rule precise).

Alternatively, consider an equivalent notation for the polytypes without quantifiers in which quantified variables are represented by a different set of symbols. In such a notation, the specialization reduces to plain consistent replacement of such variables.

The relation `⊑` is a partial order, and `∀α. α` is its smallest element.

## Principal type

While specialization of a type scheme is one use of the partial order, the partial order plays a crucial second role in the type system. Type inference with polymorphism faces the challenge of summarizing all possible types an expression may have. The order guarantees that such a summary exists as the most general type of the expression.

## Substitution in typings

The type order defined above can be extended to typings because the implied all-quantification of typings enables consistent replacement:

`Γ |- e : σ ===> SΓ |- e : Sσ`

Contrary to the specialisation rule, this is not part of the definition, but like the implicit all-quantification rather a consequence of the type rules.

Free type variables in a typing serve as placeholders for possible refinement. The binding effect of the environment to free type variables on the right hand side of `|-` that prohibits their substitution in the specialisation rule is again that a replacement has to be consistent and would need to include the whole typing.

This article will discuss 4 different rule sets:
1. |-ᴅ    declarative system
2. |-s    syntactical system
3. |-ᴊ    algorithm J
4. |-ᴡ    algorithm W


## Deductive system

The syntax of HM is carried forward to the syntax of the inference rules that form the body of the formal system, by using the typings as judgments. Each of the rules define what conclusion could be drawn from what premises. Additionally to the judgments, some extra conditions introduced above might be used as premises, too.

A proof using the rules is a sequence of judgments such that all premises are listed before a conclusion. The examples below show a possible format of proofs. From left to right, each line shows the conclusion, the `[Name]` of the rule applied and the premises, either by referring to an earlier line number if the premise is a judgment, or by making the predicate explicit.

## The Syntax of Rules

```
Predicate  := σ ⊑ σ'
            | α ∉ free(Γ)
            | x : α ∈ Γ

Judgement  := Typing
Premise    := Judgement | Predicate
Conclusion := Judgement

              Premise …
Rule       := ----------- [Name]
              Conclusion
```











## Declarative Rule System

```
x : σ ∈ Γ
----------- [Var]
Γ |- x : σ

Γ |- e₀ : τ -> τ'    Γ |- e₁ : τ
-------------------------------- [App]
Γ |- e₀ e₁ : τ'

Γ, x : τ |- e : τ'
----------------------- [Abs]
Γ |- λ x . e : τ -> τ'

Γ, x : σ |- e₁ : τ    Γ |- e₀ : σ
---------------------------------- [Let]
Γ |- let x = e₀ in e₁ : τ

Γ |- e₀ : σ'    σ' ⊑ σ
----------------------- [Inst]
Γ |- e₀ : σ

Γ |- e : σ    α ∉ Free (Γ)
--------------------------- [Gen]
Γ |- e : ∀ α . σ
```
