# Data type equality

## Equality proofs and deferred type errors (2012)
A compiler pearl - by D. Vytiniotis, S. Peyton Jones, Jose Pedro Magalhaes

Keywords
- Type equalities
- Deferred type errors
- System FC


The GHC is an optimizing compiler that expresses and manipulates *first-class equality proofs* in its intermediate language. We describe a simple, elegant technique that exploits these equality proofs to support deferred type errors. The technique requires us to treat equality proofs as possibly-divergent terms; we show how to do so without losing either soundness or the zero-overhead cost model that the programmer expects.

## Introduction

In a compiler, a typed intermediate language provides a firm place to stand, free from the design trade-offs of a complex source language. Moreover, type-checking the intermediate language provides a simple and powerful consistency check on the earlier stages of type inference and other optimizing program transformations.

GHC has just such an intermediate language. This intermediate language has evolved in the last few years from `System F` to `System FC` to accommodate the source-language features of GADTs and type families, and then from System FC to `System FC↑`, a calculus now fully equipped with kind polymorphism and datatype promotion.

The principal difference between System F and System FC↑ is that, together with type information, System FC↑ carries *equality proofs: the evidence that type equality constraints are satisfied*. Such proofs are generated during the type inference process and are useful for type checking System FC↑ programs. However, once type checking of the program is done, proofs - very much like types - can be completely (and statically) erased, so that they induce no runtime execution or allocation overhead.

Proof assistants and dependently typed languages (e.g. Coq) adopt a similar design with statically erasable proofs, including ones that go beyond equality to more complex program properties. However, there is one important difference: in proof assistants, the proof language is the computation language, always a side-effect free and terminating language that guarantees logical consistency of the proofs.

On the other hand, in System FC↑ the computation language includes partial functions and divergent terms. To ensure logical consistency, System FC↑ keeps the *equality proof language as a syntactically separate, consistent-by- construction set of equality proof combinators*.

In this paper we investigate the opportunities and challenges of blurring the rigid proof/computation boundary, without threatening soundness, by allowing "proof-like" first-class values to be returned from ordinary (even divergent or partial) computation terms, and we make these contributions:

* The **proofs-as-values** approach opens up an entirely new possibility, that of deferring type errors to runtime. Our new approach gives a principled way in which such erroneous programs can be run with complete type safety.

* The key to the almost effortless shift to proofs-as-values is based on a simple observation: System FC↑, with the recent addition of kind polymorphism already allows us to define within the system an ordinary first-class type for type equality. As such, we can have ordinary values of that type, that are passed to or returned from arbitrary (even partial or divergent) terms.

* Programmers think of types as static objects, with zero runtime overhead, and they expect the same of proofs about types. Treating type equality proofs as values seriously undermines this expectation. We show how the optimizer of GHC, with no changes whatsoever, can already eliminate the cost of equality proofs (except in corner cases where it would be wrong to do so).

## 2. The opportunity: deferring type errors

Suppose you type this Haskell term in GHCi: `fst (True, 'a' && False)`. This term does not "go wrong" when evaluated: you might expect to just get back the result True from projecting the first component of the pair, but in a statically typed language like Haskell, you get the type error: "Couldn't match Bool with Char in the first argument of (&&), namely a". This behaviour is fine for programs that are finished, but some programmers would prefer the term to evaluate to True during the prototype programming phase. After all, if the error is in a bit of the program that is not executed, it is doing no harm! In particular, when refactoring a large program it is often useful to be able to run parts of the completed program, but type errors prevent that. What we want is to defer type errors until they matter, and we can now do that with the flag `-fdefer-type-errors`.

```hs
ghci> let foo = (True, 'a' && False)
-- Warning: Couldn't match 'Bool' with 'Char'
ghci> :type foo
-- (Bool, Bool)
ghci> fst foo
-- True
ghci> snd foo
-- Runtime error: Couldn't match 'Bool' with 'Char'
```

That is, the error message is produced lazily, at runtime, when and only when the requirement for Char and Bool to be the same type is encountered.

### How deferring type errors works

Informally, type inference algorithm in GHC works in two stages: first we generate type constraints, and then we solve them. In addition, inference elaborates the Haskell source term to an explicitly typed FC↑ term, that includes the types and proofs ("evidence" in GHC jargon) computed by the constraint solver.

In the previous example, during type inference for the sub-term `'a' && False`, we generate a type equality constraint, written `Char ∼ Bool`. Usually the constraint solver would immediately reject such a constraint as insoluble, but with `-fdefer-type-errors` we take a different course: we generate "evidence" for `Char ∼ Bool`, but ensure that if the (bogus) evidence is ever evaluated it brings the program to a graceful halt. More concretely, the generated term looks something like this:

```hs
foo = let (c :: Char ∼ Bool) = error "Couldn't …"
      in  (True, (cast 'a' c) && False)
```

The elaborated `foo` contains a lazy binding of an evidence variable `c` of type `Char ∼ Bool` to a call to error. The built-in `error` function prints its argument string and brings execution to a halt.

When we evaluate `fst foo` the result is True; but if we evaluate `snd foo`, we must evaluate the result of `&&`, which in turn evaluates its first argument, `cast 'a' c`. The `cast` forces evaluation of `c`, and hence triggers the runtime error. Note that the exact placement of coercions, and thus which errors get deferred, depends on the internals of the type inference process.

There is something puzzling about binding variable `c` with the type `Char ∼ Bool`. The evidence variable `c` is supposed to be bound to a proof witnessing that Char and Bool are equal types, but is nevertheless bound to just a term, in fact, a crashing error term! How can we then ensure soundness, and how can we get statically erasable proofs?

It turns out that the type `Char ∼ Bool` is almost, but not quite, *the type of a proof object*. To explain how this works, we move on to present some more details on GHC's typed intermediate language, System FC↑.

## System FC↑

A look at the syntax description sheet, we see that the term language `e` is mostly conventional, explicitly-typed, lambda calculus, with let bindings, literals (l), data constructors (K), and case expressions.

In addition, the language includes type and kind polymorphism: type (`Λa: η.e`) and kind (`Λχ.e`) abstractions, and type (`e φ`) and kind (`e ϰ`) applications, respectively. Kind polymorphism is essential for our purposes.

The distinctive feature of System FC↑ is the use of coercions, `γ`. A coercion `γ` of type `τ ∼# φ` is *the proof of type equality* between types `φ` and `τ`.

Contrary to the notation used earlier here and in the previous presentations of System FC↑, notice that the operator `∼#` is used for coercion types instead of the `∼` operator. Also, `Constraint#` is used for their kind, rather than just `Constraint`; this is for a good reason that will become evident, later, now:

* `∼#` is type of primitive coercions `γ` that are fully erasable
* `∼` is type of evidence generated by type inference that cannot be erased



The term (`e |> γ`) is a **cast** that converts a term `e` of type `τ` to one of type `φ`, when `γ : τ ∼# φ`.

The only other place where a coercion `γ` may appear in the term language is in an application (`e γ`), so *coercions are not first-class values*. Dually, one can abstract over coercions with a *coercion abstraction*, `λ(c : τ ∼# φ) . e`.



<details>
<summary>Figure 1: Syntax of System FC</summary>


```ebnf

✱ Terms

e,u := x | l
     | λx:σ.e                     (term abstraction)
     | e u                        (term application)
     | Λa:η.e                     (type polymorphism)
     | e φ                        (type application)
     | Λχ.e                       (kind polymorphism)
     | e ϰ                        (kind application)
     | λc : τ.e                    (coercion abstraction)
     | e γ                        (coercion application)
     | let x:τ = e in u           (let binding)
     | e |> γ       ______        (cast)
     | K ❘ case e of p → u        (case exp)
       ___ ___
p := K c:τ x:τ                    (patterns)

✱ Types

φ,σ,τ,υ := a                      (variables)
         | H                      (constraints)
         | F                      (type functions)
         | φ₁ φ₂                  (application)
         | φ ϰ                    (kind application)
         | ∀a:η.φ                 (polymorphic types)
         | ∀χ.τ                   (kind-polymorphic types)

✱ Type constants
H := T                            (Datatypes)
   | (→)                          (Arrow)
   | (∼#)                         (Primitive equality type)

✱ Kinds
k,h := χ
     | ✼
     | k → k
     | ∀χ.ϰ                       (Polymorphic kinds)
     | Constraint#                (Kind of static proofs)

✱ Coercion values
g,d := c                          Variables
     | γ₁ γ₂                      Application
     | ⟨φ⟩                        Reflexivity
     | γ₁; γ₂                     Transitivity
     | sym γ                      Symmetry
     | …                          Other coercion forms


etc.

```

</details>

The syntax of coercions themselves (`γ` in Figure 1) includes coercion variables, constructors for reflexivity, transitivity, and symmetry, as well as other constructors (such as lifting type equalities over data constructors) that we do not need to discuss in this paper.


<details>
<summary>Figure 2: Well-formed terms</summary>

...

</details>


The well-formedness judgement for terms appears in Figure 2 and is mostly conventional. In particular, the rules for coercion abstraction and application (ECABS and ECAPP) mirror those for terms (EABS and EAPP). The rule for case expressions (ECASE) is also standard but notice that it allows us to bind coercion variables, as well as term variables, in a pattern.


### 3.1 Types, kinds, and kind polymorphism

What should the kind of `∼#` be? We mentioned previously that we would like to classify any type `t ∼# s` as having kind `Constraint#`, but the kind of `t` and `s` can be any kind whatsoever. This indicates that `∼#` should be given the polymorphic kind, `forall k. k → k → Constraint#`.

### 3.2 FC↑ datatypes with coercions

Coercions can appear as arguments to data constructors, a feature that is particularly useful for representing GADTs.

Consider this program which defines and uses a GADT:

```hs
data T a where
  T₁ :: Int -> T Int
  T₂ :: a -> T a

f :: T a -> [a]
f (T₁ x) = [x + 1]
f (T₂ v) = []

main = f (T₁ 4)
```







---

Types have a very valuable property that programmers take for granted: they give strong static guarantees, but they carry no runtime overhead. This zero-overhead guarantee is formally justified by the *erasure property*: we can erase all the types before running the program, without changing the result.
