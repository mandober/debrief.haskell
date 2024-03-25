# Haskell and formal reasoning

`The Nax language: unifying functional programming and logical reasoning in a language based on mendler-style recursion schemes and term-indexed types`
Ki Yung Ahn, 2014

Relying on the Curry-Howard correspondence, computer scientists, particularly programming language researchers, have tried and are still trying to come up with a universal language that can be used for functional programming (modeling computation) and for formally reasoning about programs (writing proofs). Recent languages like Idris, Agda and Lean have closed further on the target, but most people will still use Coq to write formal proofs. So, there is still not one all encompasing language to cover all the needs.

The gap between the conflicting design goals of typed functional programming languages and formal reasoning systems is still wide. One difficulty is that data types admitted in functional languages and those admitted in reasoning systems do not coincide completely.

>Programming languages are designed to achieve computational expressiveness, for which they often sacrifice logical consistency.

Programmers should be able to conveniently express all possible computations, regardless of whether those computations have a logical interpretation.

>Formal reasoning systems are designed to achieve logical consistency, for which they often sacrifice computational expressiveness.

Users expect that it is only possible to prove true propositions but impossible to prove falsity. They are willing to live with the difficultly (or even inability) to express certain computations within the reasoning system for achieving logical consistency.

Consequently, the *recursion schemes* of programming languages and formal reasoning systems differ considerably.

Programming languages provide *unrestricted general recursion* to conveniently express computations that may or may not terminate.

Formal reasoning systems provide *induction principles* for sound reasoning, or, from the computational viewpoint, *principled recursion schemes* that can only express terminating computations.

The two different design goals for programming languages and reasoning systems are reflected in the design of their type systems, especially regarding data types and recursion schemes.

Programming languages place few restrictions on the definition of data types. Programmers can express computations over a wide variety of data types.

In reasoning systems based on conventional approach, additional restrictions are enforced on data type definition - *only positive data types* are accepted.

In addition, most functional programing languages have a clearly distinguish between terms and types (i.e. terms do not appear in types).

In reasoning systems, terms can appear in types for specifying fine-grained properties involving values at the term-level (e.g. size invariants of data structures).

A unified language that is logically consistent while being able to conveniently express many useful computations should place as few restrictions on data type definitions like in FP, but also see to support a rich set of non-conventional recursion schemes that always terminate. The non-conventional recursion schemes are known as *Mendler-style recursion schemes*. There is also some middle ground between polymorphic types and dependent types in that a language can have term indices in types without supporting full dependent typing.

## 1.3 Thesis

We characterize the sweet spot of language design for unifying functional programming and reasoning by supporting the following features:
1. A convenient programming style supported by the major constructs of modern FPLs: parametric polymorphism, recursive data types, recursive functions, type inference.
2. An expressive logic that can specify fine-grained program properties using types, and terms that witness the proofs of these properties under the Curry-Howard correspondence.
3. A small theory based on a minimal foundational calculus that is expressive enough to support programming features, expressive enough to embed propositions and proofs about programs, and logically consistent to avoid paradoxical proofs in the logic.
4. A simple implementation that keeps the trusted base small.

Our thesis is that a language design based on Mendler-style recursion schemes and term-indexed types can lead to a system that supports these four features.

## 1.4 Mendler-style recursion and term-indexed types

### 1.4.1 Restriction on recursive types for normalization

Logical reasoning systems establish the Curry-Howard correspondence assuming normalization. So, one challenge in the successful design of reasoning systems is how to restrict recursion, so that all well-typed terms have normal forms.

In contrast to the unrestricted general recursion in functional languages, there are two different design choices to this end. The conventional approach restricts the formation of recursive types - *restricts data type formation*, whereas the Mendler-style approach restricts the elimination of the values of recursive types - *restricts pattern matching*.

#### Recursive types in functional programming languages

A review of the theory of recursive types used in FPLs: just as we can capture the essence of unrestricted general recursion *at the term level* by using a fixpoint operator, `fix` in Haskell, we can capture the essence of recursive types by the using a recursive type operator `µ` *at the type level*.

The rules for the formation, `µ-form`, introduction, `µ-intro`, and elimination, `µ-elim`, of the recursive type operator `µ` are given below.

We need a reduction rule, `unIn-In`, that relates `In`, the data ctor for recursive types, and `unIn`, the destructor for recursive types, at the term-level.

The recursive type operator `µ` is already powerful enough to express non-terminating programs, even without introducing the general recursive term operator `fix` into the language.


* Unrestricted general recursion in FPLs

```hs

          Γ |- F : * -> *
kinding ----------------------- (µ-form)
          Γ |- µF : *


          Γ |- t : F (µF)
typing  ----------------------- (µ-intro)
          Γ |- In t : µF


          Γ |- t : µF
typing  ----------------------- (µ-elim)
          Γ |- unIn t : F (µF)


reduction --------------------- (unIn-In)
            unIn (In t) ~~> t
```

* A conventional recursion scheme

```hs
          Γ |- F : * -> * positive(F)
kinding ------------------------------- (µ-form+)
          Γ |- µF : *


-- typing: (µ-intro) and (µ-elim) same as for FP

      Γ |- t : µF      Γ |- φ : F A -> A
(It) ------------------------------------
              Γ |- It φ t : A


-- reduction: (unIn-In) same as for FP

(It-In) -------------------------------------
          It φ (In t) ~~> φ (mapᶠ (It φ) t)
```

* A Mendler-style recursion scheme

```hs
-- kinding: (µ-form) same as as for FP

-- typing: (µ-intro) same as as for FP

        Γ |- t : µF       Γ |- φ : ∀X. (X -> A) -> F X -> A
(mit) ------------------------------------------------------
                      Γ |- mit φ t : A


reduction ---------------------------------- (mit-In)
            mit φ (In t) ~~> φ (mit φ) t
```


But first here is a short reminder of how a fixpoint works at the term-level.

The typing rule and the reduction rule for `fix` can be given as follows:

```hs
          Γ |- f : A → A
typing --------------------
           fix f : A

reduction -----------------------
            fix f ~~> f (fix f)
```

We can actually implement `fix` using the `µ` type operator as follows:

```hs
newtype Fix f = In { unIn :: f (Fix f) }

-- a non-recursive data type
data T a r = C (r -> a)

-- an encoding of the untyped ω := λx.xx in a typed language
w :: Fix (T a) -> a
w = \x -> case unIn x of
   C f -> f x

-- an encoding of Y := (λf.(λx.f(x x)) (λx.f(x x)))
fix :: (a -> a) -> a
fix = \f -> (\x -> f (w x))
     (In (C (\x -> f (w x)) ))
```

We need to alter the rules for `µ` (`Fix`) in someways to guarantee termination. One way is to restrict the rule *µ-form* and the other way is to restrict the rule *µ-elim*. The design of principled recursion combinators, i.e. `It` for the former and `mit` for the latter, follows from the choice of the rule to restrict.

#### Positive and negative recursive data types

> Positive data types are recursive on only covariant positions.

For example, `µ T2`, where `data T2 r = C2 (Bool → r)`, is a positive data type since the recursive argument `r` in the base structure `T2` appears only in the covariant position.

Recursive data types that have no function arguments are by default positive data types. For instance, the natural number data type `µ ℕ`, where `data ℕ r = S r | Z`, is a positive data type.

>Negative datatypes have recursion in contravariant positions.

Note that `µ(T a)` in the example above is a negative datatype because the recursive argument `r` in the base structure `T` appears in the contravariant position. Another example of a negative datatype is `µ T0`, where `data T0 r = C0 (r → r)` because `r` in `T0` appears in both contravariant and covariant positions. We say that `r` is in a negative position because `r → a` is analogous to (¬r ∧ a) when we think of → as a logical implication.

#### Recursive types in conventional approach

In conventional approach, the formation (datatype definition) of recursive types is restricted, but arbitrary elimination (pattern matching) over the values of recursive types is allowed.

In particular, the formation of negative recursive types is restricted. Only positive recursive types are supported. Thus, above, we have a restricted version of the formation rule (µ-form+) with an additional condition that F should be positive. The other rules (µ-intro), (µ-elim), and (unIn-In) remain the same as in functional languages.

Because we have restricted the recursive types at the typelevel and we do not have general recursion at the term-level, the language is indeed normalizing. However, we cannot write interesting (recursive) programs that involve recursive types, nor can we reason inductively about those programs, unless we have principled recursion schemes that are guaranteed to normalize. One such recursion scheme is called iteration (aka catamorphism).

The typing rules for the conventional iteration `It` are given above. Note, we have the typing rule `It` and the reduction rule `It-In` for `It` in addition to the rules for the recursive type operator `µ`.

#### Recursive types in Mendler-style approach

In Mendler-style approach, we allow arbitrary formation (data type definition) of recursive types, but we restrict the elimination (pattern matching) over the values of recursive types.

The formation rule (µ-form) remains the same as that for FPL. That is, we can define arbitrary recursive types, both positive and negative. However, we no longer have the elimination rule (µ-elim). That is, we are not allowed to pattern match against the values of recursive types freely, as we do for values of non-recursive datatypes using case expressions. We can only pattern match over the values of recursive types using Mendler-style recursion combinators.

The rules for the Mendler-style iteration combinator `mit` are above. Note that there are no rules for `unIn` in the Mendler-style approach. The typing rule `µ-elim` is replaced with `mit` and the reduction rule `unIn-In` is replaced with `mit-In`.

More precisely, the typing rule `mit` is both an elimination rule for recursive types and a typing rule for the Mendler-style iterator. You can think of it as replacing both the elimination rule (µ-elim) and the typing rule for conventional iteration (It), but in a safe manner that guarantees normalization.

Mendler style is a promising approach because all recursive types (both positive and negative) are definable and the recursion schemes over those types are normalizing.
