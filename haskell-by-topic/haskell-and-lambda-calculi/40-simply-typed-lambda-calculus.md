# Simply-Typed Lambda Calculus

## STLC Summary
- Simply Typed Lambda Calculus (STLC) or **λ→**
- STLC is a typed interpretation of Lambda Calculus
- STLC has only one type constructor (→) that builds function types
- `α, β, …` denote type parameters
- `α, β, …` to represent *variable* or *undetermined* type symbols (Church 1940)
- metavariables `τ, σ, …`
- B is a set of base types (atomic types, type constants)
- term constants are naturals if there is the type `nat`
- abs must specify the type of its arg (typing à la Church)
- typing environment (contexts) `Γ` is set of typing assumptions
- typing relation `Γ ⊢ e : σ`
- instances of typing relation are typing judgements
- validity of typing judgement is shown via typing derivation, using type rules

## STLC

To define the types, a set of **base types**, `B`, must first be defined. These are sometimes called *atomic types* or *type constants*. With this fixed, the syntax of types is:

```
τ := T         where T ∈ B       base types
   | τ → τ                       function types
```

For example, set `B = {a, b}`, generates an infinite set of types starting with a → a, a → b, b → b, b → a, a → (a → a), …, (b → a) → (a → b), …

A set of **term constants** is also fixed for the base types. For example, it might be assumed that one of the base types is `nat`, and its term constants could be the natural numbers: 0, 1, 2, …

The **syntax** of the simply typed lambda calculus is essentially that of the lambda calculus itself. The term `x : τ` denotes that the variable `x` is of type `τ`. The term syntax in BNF is: variable reference, abstractions, application, or term constant:

```
e := x                  variable reference
   | λ x : τ . e        abstraction
   | e e                application
   | c                  term constant
```

In typing à la Church, an abstraction must annotatate the type of its arg. 
In typing à la Curry, there are no type annotation and the types are inferred.

To define the set of well-typed lambda terms of a given type, we define a *typing relation between terms and types*. First, we introduce typing contexts, or *typing environments* Γ, Δ, …, which are *sets of typing assumptions*. A typing assumption has the form `x:σ`, meaning variable `x` has type `σ`. The typing relation `Γ ⊢ e : σ` indicates that `e` is a term of type `σ` in context Γ. In such case `e` is said to be *well-typed*.

Instances of the typing relation are called **typing judgements**. The *validity of a typing judgement* is shown by providing a *typing derivation*, constructed using *typing rules*.

## Typing rules

The typing rules of the simply-typed lambda calculus (left) compared to the inference rules of propositional logic (right):

```hs
STLC                                         LOGIC

K : T                                         [P]¹
----------- CONST                              ⁝
Γ ⊢ K : T                                      Q
                                             ------- ⇒I  ------ ASS
                                             P¹ ⇒ Q         P
                                             ------------------- ⇒E
                                                      Q

x : σ ∈ Γ           σ = lookup Γ x
---------- VAR i.e. -------------- VAR       ------- ASS
Γ ⊢ x : σ             Γ ⊢ x : σ               Γ ⊢ σ


Γ, x : σ ⊢ M : τ                              Γ, σ ⊢ τ
------------------------ ABS                 ------------ ⇒I
Γ ⊢ (λx : σ. M) : σ → τ                      Γ ⊢ σ ⇒ τ


Γ ⊢ M : σ → τ     Γ ⊢ N : σ                  Γ ⊢ σ → τ    Γ ⊢ σ
----------------------------- APP            ------------------- ⇒E (MP)
Γ ⊢ M N : τ                                        Γ ⊢ τ


Γ, x : σ ⊢ M : τ     Γ ⊢ N : σ
--------------------------------- LET
    Γ ⊢ let x = N in M : τ


Γ ⊢ e₁:bool   Γ ⊢ e₂:τ   Γ ⊢ e₃:τ
---------------------------------- IF  --------------- T  ---------------- F
Γ ⊢ if (e₁ e₂ e₃) : τ                  Γ ⊢ true : bool    Γ ⊢ false : bool
```

The inference rules of propositional logic are like the typing rules of STLC stripped of terms.

## Typing constraints

Constraints of the APP rule: a lambda's arg must be the same type as the lambda's formal parameter. The type of an application is then the type of the lambda's return type.

Constraints of the IF rule: the test exp must have the type `bool`, and the two branches must have the same type.

These are typing constraints, which only have to do with *well-typedness*, not evaluation. The *evaluation rules* of STLC, when specified, will include rules like: `if (true e₁ e₂)` ->> `e₁`, and `if (false e₁ e₂)` ->> `e₂`.


## Description of typing rules

```hs
- CST: term constant `K` has appropriate base type.    K : T

- VAR: if `x` has type `σ` in the context Γ,           x : σ ∈ Γ
       then we can derive                              ---------
       that `x` has type `σ`.                          Γ ⊢ x : σ

- ABS: if, in a certain context Γ
      `x` has type `σ` and `M` has type `τ`,           Γ, x : σ ⊢ M : τ
       then in context Γ with `x` removed,             --------------------
      `λx:σ.M` has function type `σ → τ`.              Γ ⊢ (λx:σ.M) : σ → τ


- APP: if, in a certain context,
       exp `e₁` has function type `σ → τ`,
       and exp `e₂` has type `σ`, 
       then the application `e₁ e₂` has type `τ`.

- LET: if, in a certain context Γ,
      `x` has type `σ` and
      `e₁` has type `τ`
                                               Γ, x : σ ⊢ M : τ
       and, if, in the context Γ, 
      `e₂` has type `σ`
                                               Γ ⊢ N : σ
       then in the context Γ with `x` removed, 
      `let x = e₂ in e₁` has type `τ`.
                                               Γ ⊢ let x = N in M : τ
```

## STLC details

- terms and types
- terms
  - lambda term (lambda exp)
  - constant terms: e.g. 'true' of `bool`, 1 of type `nat`
- types
  - base types (type constants, atomic types):
    - set `B` of base types, e.g. B = {a, b} or B = {bool, nat}
    - e.g. `bool`, `nat`
  - type parameters: `α`, `β`, …
  - type constructors:
    - only one type constructor, `->`
    - function types it builds


### Terms
- λ→ contains terms and types
- TERM: either lambda terms or term constants
- LAMBDA TERM: has one of 3 forms: Var, App and Abs, i.e. lambda terms are either variables, abstractions or applications. We could also say that a lambda term is a term constructed with one of the 3 data constructors: Var, App, Abs
- EXPRESSION: a lambda expression is any ~~arbitrary~~ (no! not anymore) well-typed combination of lambda terms. Actually, there is no difference between terms and expressions: any exp must is a term because its root node must be either Var, App or Abs. Thus, "expression" and "term" are synonyms.
- EXPRESSION tree: may contain arbitrary non-well-defined terms (random exps)
- DERIVATION tree: contains only well-defined terms (wffs)
- CONSTANT: term constant, e.g. 1 (if `nat` type), 'true' (if `bool` type)
- LITERAL: term constant
- VALUE: term constants and abstractions (lambda functions) if first-class function support is assumed, which it commonly is. In fact, these are called closures because they can capture (close over) values from the surrounding environment (scope)
- SCOPE
- CLOSURE

### Types
- `α, β, …` denote type parameters (type variables)
- however, we also use greek letter as metavariables, `τ, σ, …`
- base types are constant types like `nat`, `bool`, etc.
- abstraction must specify the type of its arg (typing à la Church)

### Environment
- typing relation `Γ ⊢ e : σ`
- instances of typing relations are typing judgements
- typing environment (contexts) `Γ` is set of typing assumptions
- validity of typing judgement is shown via typing derivation, using type rules
- context, or typing environment, is usually denoted by `Γ`
- context records the types of variables (identifiers)
- it is a list of x:σ pairs (var-type pairs)
- when we determine that a variable `x` type `σ`, we add the new entry `x:σ` to the context. Later, we can emphasize the fact that `x:σ` is indeed in gamma by writing `Γ, x : σ` or `x : σ ∈ Γ`.
- Note that `σ` is a *metavariable* is these descriptions. It is replaced by a *concrete type*, so we do not add `x : σ` to the context, but, e.g. `x : nat`, `y : bool`, `z : a`, `f : a → a`, `g : nat → bool`. Vars can have a mix of concrete types and type parameters.
- if the set of base types is B = {a, b}, then a legal context is a list, e.g. Γ = [ b: a, x: b, y: a → a, f: a → b → a → b ]
- if the set of base types is B = {nat, bool}, then a legal context is a list, e.g. Γ = [ b: bool, x: nat, f: nat → bool, y: bool → bool, z: α → α, w:α→nat ], so a *context may mention base types but type vars as well*.
- context starts empty, sometimes denoted by `∅ ⊢ Δ` or `∙ ⊢ Δ`
- context only records the type of variables
- context records the type of variables as they are encountered, so a more recent occurrence of a variable named `x` will *shadow* the previous occurrence of the same var `x` in the same conext, if the conext is indeed a list (not a set). That is, when we look up the type of `x` in the context, the most recent entry is returned - because new entries are appended (not prepended) to a list.
- `Γ, x : σ` may be interpreted as a context Γ which already contains the entry `x : σ` and we just want to emphasize that fact. This may also be interpreted as inserting a new entry `x : σ` in Γ, i.e. extending the context.

### CONST typing rule
- CONST rule types the constant terms of base types
- constant terms (literals) are values of some base type
- if there is `bool` type, its values ('true', 'false') are term constants
- when a term has a base types, that fact need not be recorded in the context because a constant term like 'true' always has the same type, viz. `bool`, i.e. the type is fixed; unlike the type of vars, the types of constants cannot vary. No point in recording the fact that `Γ ⊢ true : bool`. The environment Γ only records the type of variables - it is the list of `x : σ` pairs.

### VAR typing rule
- We look up the type of a var `x` in the context Γ. Of course, the type must have been recorded in the context previously, otherwise we'd get a type error.

### ABS typing rule
- abstraction must specify the type of its arg (typing à la Church)
- As in natural deduction we are allowed to introduce abstraction (implication) by swapping sides around the turnstile: Γ, σ ⊢ τ ≡ Γ ⊢ σ → τ

### APP typing rule
- application `M N` means applying the term `M` to term `N`
- `M` better be an abstraction, of the form `λx.B`, so we get `(λx.B)N`
- app `(λx.B)N` is evaluated using the by β-reduction rule of evaluation
- β-reduction proceeds by substitution, `(λx.B)N` ⟶ᵦ B[x:=N]

### LET typing rule
- not an "official" rule, but often included
- a let-expression combines abs and app
- `let x = B in N` has similar meaning to app `(λx.B)N`
- let-expression is evaluated using the LET rule which is like β-reduction
- it also uses substitution, `let x = B in N` ⟶ᵦ B[x:=N]



## Evaluation rules

```hs
-------------------- β
(λx.M)N ⟶ [x:=N]M


         e → eʹ                 if (true M N)         if (false M N)
-------------------------E-IF   ------------- E-IFT   -------------- E-IFF
if (e M N) → if (eʹ M N)              M                      N
```

## Examples

Examples of *closed terms*, i.e. terms typable in the empty context
- For every type `τ`, a term `λx : τ. x : τ → τ` (I combinator)
- For types `σ`, `τ`, a term `λx : σ. λy : τ. x : σ → τ → σ` (K combinator)

`τ` and `σ` are metavariables! This is not polymorphism!

## Order of a type

Each type `τ` is assigned an **order**, a number `o(τ)`. 
- for base types, `o(T) = 0`
- for function types, `o(σ→τ) = max(1 + o(σ), o(τ))`

>The order of a type measures the depth of the most left-nested arrow.

For example:
- o(ι → ι → ι) = 1
- o((ι → ι) → ι) = 2

## Semantics

### Intrinsic vs extrinsic interpretations

Broadly speaking, there are two different ways of assigning meaning to the simply typed lambda calculus (and to typed languages more generally), variously called
- intrinsic vs extrinsic
- ontological vs semantical
- Church-style vs Curry-style

An **intrinsic semantics** only assigns meaning to well-typed terms, or more precisely, assigns meaning directly to typing derivations. This has the effect that terms differing only by type annotations can nonetheless be assigned different meanings.

For example, the identity term `λx:int. x` on integers and the identity term `λx:bool. x` on Booleans may mean different things (the classic intended interpretations are the identity function on integers, and the identity function on booleans).

In contrast, an **extrinsic semantics** assigns meaning to terms regardless of typing, as they would be interpreted in an untyped language. In this view, `λx:int.x` and `λx:bool.x` mean the same thing, i.e. the same thing as `λx. x`.

The distinction between intrinsic and extrinsic semantics is sometimes associated with the presence (Church-style) or absence (Curry-style) of *annotations on abstractions*, but strictly speaking this usage is imprecise.

It is possible to define an extrinsic semantics on annotated terms simply by ignoring the types (i.e. through type erasure), as it is possible to give an intrinsic semantics on unannotated terms when the types can be deduced from context (i.e. through type inference).

>Rather, the essential difference between intrinsic and extrinsic approaches is just whether the typing rules are viewed as defining the language, or as a formalism for verifying properties of a more primitive underlying language.

Most of the different semantic interpretations discussed below can be seen through either an intrinsic or extrinsic perspective.

### Equational theory

The simply typed lambda calculus (STLC) has the same equational theory of *βη-equivalence* as untyped lambda calculus, but subject to type restrictions.

The equation for beta reduction, `(λx:σ.t)u` =ᵦ `t[x:=u]`, holds in context Γ whenever `Γ,x : σ ⊢ t : τ` and `Γ ⊢ u : σ`.

The equation for eta reduction `λx:σ.tx =η= t` holds whenever `Γ ⊢ t : σ → τ` and `x` does not appear free in `t`.

The advantage of typed lambda calculus is that STLC allows potentially nonterminating computations to be cut short (that is, reduced).

### Operational semantics

Likewise, the operational semantics of simply typed lambda calculus can be fixed as for the untyped lambda calculus, using call-by-name, call-by-value, or other *evaluation strategies*.

As for any typed language, *type safety* is a fundamental property of all of these evaluation strategies.

Additionally, the *strong normalization* property described below implies that any evaluation strategy will terminate on all simply typed terms.

### Categorical semantics

The simply typed lambda calculus enriched with product types, pairing and projection operators (with βη-equivalence) is the *internal language* of *Cartesian closed categories* (CCCs).

Given any CCC
- objects: *basic types* (of the corresponding lambda calculus)
- morphisms: *terms*

Conversely, the STLC with
- product types and
- pairing operators
over a collection of
- base types and
- given terms
forms a CCC whose
- objects are the *types* and
- morphisms are *equivalence classes of terms*

There are typing rules for
- pairing
- projection
- unit term

Given two terms `s : σ` and `t : τ`, the term `(s, t)` has type `σ × τ`.

Likewise, if one has a term `u : τ₁ × τ₂`, then there are terms 
- `π₁ u : τ₁`
- `π₂ u : τ₂`
where `πᵢ` correspond to the projections of the Cartesian product.

The unit term, `⋆`, of type `𝟙`, is the final object.

The equational theory is extended likewise, so that one has
- π₁(s : σ, t : τ) = s : σ
- π₂(s : σ, t : τ) = t : τ
- (π₁(u : σ × τ), π₂(u : σ × τ)) = u : σ × τ
- t : 𝟙 = ⋆

The last one is read: if `t` has type unit, then it reduces to nil.

The above can then be turned into a category by taking the types as objects. The morphisms `σ → τ` are equivalence classes of pairs `(x:σ, t:τ)` where `x` is a variable of type `σ`, and `t` is a term of type `τ`, having no free variables in it, except optionally for `x`.

The set of terms in the language is the closure of this set of terms under the operations of abstraction and application.

This correspondence can be extended to include "language homomorphisms" and functors between the category of CCCs and the category of simply-typed lambda theories.

Part of this correspondence can be extended to *closed symmetric monoidal categories* by using a *linear type system*.

### Proof-theoretic semantics

STLC is closely related to the implicational fragment of propositional intuitionistic logic, i.e. the *implicational propositional calculus*, via the Curry-Howard isomorphism: terms correspond precisely to proofs in natural deduction, and inhabited types are exactly the tautologies of this logic.

From his *logistic method* Church (1940) laid out an axiom schema which Henkin (1949) filled in to show that type domains (e.g. ℕ, ℝ, etc.) ~~huh? to show that type domains...what?~~

Henkin (1996) described how Church's logistic method could seek to provide a foundation for mathematics (Peano arithmetic and real analysis), via model theory.

## Alternative syntaxes

The presentation above is not the only way of defining the syntax of STLC.

One alternative is to remove type annotations entirely (so that the syntax is identical to the untyped lambda calculus), while ensuring that terms are well-typed via **Hindley-Milner type inference**. The inference algorithm is terminating, sound, and complete: whenever a term is typable, the algorithm computes its type. More precisely, it computes the term's principal type, since often an unannotated term (such as λx.x) may have more than one type.

Another alternative presentation is based on **bidirectional type checking**, which requires more type annotations than HM, but is easier to describe. The type system is divided into two judgments, representing checking and synthesis.

- synthesis judgment, `Γ ⊢ e ⇒ τ`, takes `Γ` and `e` as inputs, producing `τ`.
- checking  judgment, `Γ ⊢ e ⇐ τ`, takes `Γ`, `e`, `τ` as inputs.

But what does the checking judgment return? It'd seem that checking judgements are intended to be *correct by construction* seeing they only return the unit.

These judgments are derived via the following rules:

## Bidirectional typing rules

- synthesis judgment, `Γ ⊢ e ⇒ τ`, takes `Γ` and `e` as inputs, producing `τ`.
- checking  judgment, `Γ ⊢ e ⇐ τ`, takes `Γ`, `e`, `τ` as inputs.

```hs
c is a constant of type T
-------------------------- BIDI∙CONST
Γ ⊢ c ⇒ T


x : σ ∈ Γ
---------- BIDI∙VAR
Γ ⊢ x ⇒ σ


Γ, x : σ ⊢ e ⇐ τ
------------------ BIDI∙ABS
Γ ⊢ λx.e ⇐ σ → τ


Γ ⊢ e₁ ⇒ σ → τ     Γ ⊢ e₂ ⇐ σ
------------------------------- BIDI∙APP   (2 premises!)
Γ ⊢ e₁ e₂ ⇒ τ


Γ ⊢ e ⇒ τ
----------- BIDI∙TURN
Γ ⊢ e ⇐ τ


Γ ⊢ e ⇐ τ
---------------- BIDI∙SYNTH-CHECK
Γ ⊢ (e : τ) ⇒ τ
```

## Bidi rules description

Most rules are nearly identical to the rules in the exposition above, except for the careful choice of checking or synthesis judgments. These choices can be explained like so:

Note how the rules for
- *synthesis*  are read top-to-bottom ↧
- **checking** are read bottom-to-top ↥


Bi-di rules description:

- ↧ BIDI∙CONST:   
    The types of term constants are fixed and can be *synthesized*   
    (looking shit up is considered a form of synthesis).   
    `T` is a base type. `c` is a literal whose type is thus fixed.   

      c is constant of type T
      ------------------------ BIDI∙CONST
      Γ ⊢ c _⇒_ T


- ↧ BIDI∙VAR:   
    If `x : σ` is in the context Γ,   
    we can *synthesize* type `σ` for `x`   
    (See? Looking up a var in a context is "synthesis").   

      x : σ ∈ Γ
      ------------- BIDI∙VAR
      Γ ⊢ x _⇒_ σ


- ↥ BIDI∙ABS:    
    To __check__ that `λx.e` has type `σ → τ` in context Γ,   
    extend Γ with `x : σ` and __check__ that `e` has type `τ`.  

      Γ, x : σ ⊢ e __⇐__ τ
      ---------------------- BIDI∙ABS
      Γ ⊢ λx.e __⇐__ σ → τ


- ↧ BIDI∙APP:   
      If `e₁`    *synthesizes* type `σ → τ` in Γ   (lambda term)   
     and `e₂`     __checks__   type `σ`     in Γ   (arg term)   
    then `e₁ e₂` *synthesizes* type `τ`     in Γ   (application)   

      Γ ⊢ e₁ _⇒_ σ → τ     Γ ⊢ e₂ __⇐__ σ
      -------------------------------------- BIDI∙APP
      Γ ⊢ e₁ e₂ _⇒_ τ


Note that we don't need annotation on the abstraction in rule BIDI∙ABS, because the type of the bound variable can be deduced from the type at which we check the lambda and its arg in the rule BIDI∙APP.


- ↥ BIDI∙SYNTH-CHECK:   
    To **check** that `e`  has  type `τ`,   
    it suffices to *synthesize* type `τ`.   

      Γ ⊢ e  _⇒_  τ
      ---------------- BIDI∙SYNTH-CHECK
      Γ ⊢ e __⇐__ τ


- ↧ BIDI∙CHECK-SYNTH:    
    If `e` **checks** against type `τ`   
    then the explicitly annotated term `(e : τ)` *synthesizes* `τ`.   

      Γ ⊢ e __⇐__ τ
      ------------------- BIDI∙CHECK-SYNTH
      Γ ⊢ (e : τ) _⇒_ τ


Because of these last two rules coercing between synthesis and checking, it is easy to see that any well-typed but unannotated term can be checked in the bidirectional system, so long as we insert "enough" type annotations. In fact, **annotations are needed only on β-redexes** in a bidi type system.

Noe that
- knowing the types of term constants
- looking a type up in an environment
- reading the type annotation off a term
are all considered forms of *synthesis*.



## General observations

Given the standard semantics, STLC is **strongly normalizing**: every sequence of reductions eventually terminates.

This is because *recursion is not allowed* by the typing rules: it is impossible to find types for fixed-point combinators and the looping term `Ω = (λx. x x)(λx. x x)`

Recursion can be added to the language by either having a special operator `fix α` of type `(α → α) → α`, or adding general recursive types, though both eliminate strong normalization.

Unlike untyped lambda calculus, STLC as well as any other typed lambda calculi are *not Turing complete*. All programs in STLC halt.

For the untyped lambda calculus, there are programs that do not halt, and moreover there is no general decision procedure that can determine whether a program halts (which has nothing to do with LC per se).

## Important results

* Tait showed in 1967 that β-reduction is strongly normalizing [10]. As a corollary βη-equivalence is decidable.

Statman showed in 1979 that the normalisation problem is not elementary recursive,[12] a proof that was later simplified by Mairson.[13] The problem is known to be in the set E4 of the Grzegorczyk hierarchy.[14] 

A purely semantic normalisation proof (see normalisation by evaluation) was given by Berger and Schwichtenberg in 1991.[15]

* The unification problem for βη-equivalence is undecidable. Huet showed in 1973 that 3rd order unification is undecidable[16] and this was improved upon by Baxter in 1978[17] then by Goldfarb in 1981[18] by showing that 2nd order unification is already undecidable. 

A proof that *higher-order matching* (unification where only one term contains existential variables) is decidable was announced by Colin Stirling in 2006, and a full proof was published in 2009.[19]

* We can encode natural numbers by terms of the type 
(o → o) → (o → o) (Church numerals). 

Schwichtenberg showed in 1975 that in λ→ exactly the extended polynomials are representable as functions over Church numerals;[20] these are roughly the polynomials closed up under a conditional operator.

* A full model of λ→ is given by interpreting base types as sets and function types by the set-theoretic function space. Friedman showed in 1975 that this interpretation is complete for βη-equivalence, if the base types are interpreted by infinite sets.[21] 

Statman showed in 1983 that *βη-equivalence* is the maximal equivalence that is typically ambiguous, i.e. closed under type substitutions (Statman's Typical Ambiguity Theorem).[22] A corollary of this is that the finite model property holds, i.e. finite sets are sufficient to distinguish terms that are not identified by βη-equivalence.

* Plotkin introduced logical relations in 1973 to characterize the elements of a model that are definable by lambda terms.[23] In 1993 Jung and Tiuryn showed that a general form of logical relation (Kripke logical relations with varying arity) exactly characterizes lambda definability.[24] Plotkin and Statman conjectured that it is decidable whether a given element of a model generated from finite sets is definable by a lambda term (Plotkin-Statman conjecture). The conjecture was shown to be false by Loader in 2001.[25]

## Notes

- Alonzo Church, 1956, "Introduction to Mathematical Logic"
- Alonzo Church, June 1940. "A formulation of the simple theory of types"
  https://web.archive.org/web/20190112232531/https://pdfs.semanticscholar.org/28bf/123690205ae5bbd9f8c84b1330025e8476e4.pdf


[10]: Tait, W. W. (August 1967). "Intensional interpretations of functionals of finite type I"

## Refs

- Loader, Ralph (February 1998). "Notes on Simply Typed Lambda Calculus"
  http://www.lfcs.inf.ed.ac.uk/reports/98/ECS-LFCS-98-381/
- "Church's Type Theory" in the Stanford Encyclopedia of Philosophy
  https://plato.stanford.edu/entries/type-theory-church/
