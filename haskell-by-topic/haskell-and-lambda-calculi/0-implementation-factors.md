# Implementation factors


- syntax
  - variable as a string
  - variable as a parameter (terms parameterized by the type of variables)
  - variable as a De Bruijn index
  - FOAS, lambda as `Abs String Term`
  - HOAS, `Abs (Term -> Term)`
  - PHOAS
- let-binding
- substitution model
  - direct substitution
  - via environment
- order of eval
  - normal
  - applicative
  - eval under lambda
- type system
  - untyped
  - simply-typed
  - polymorphism
  - type ctors
  - dep types
- call-by-value
- call-by-name
- call-by-need
- strict eval
- non-strict eval
- lazy eval
- small-step semantics
- big-step semantics
- standard library
- rightmost-outermost order
- leftmost-innermost order
- β-reduction
- α-conversion
- η-conversion
- equivalence of terms



Q: Exact difference between these terms: strict, call-by-value, applicative order, rightmost-outermost order?

Q: Is *applicative order* a synonym for *rightmost-outermost order*?


## Categories

Names
- names are variables
- disconnect of vars in `Var String` vs `Abs String Exp`, 
  i.e. `Var String` vs `String`
- attaching extra info to names
- parameterization of var by type
- name as string
- name as param
- name as de Bruijn index
- capture-avoiding substitution
- deciding α-equivalance

Name binding
- de Bruijn indices
- locally nameless
- director strings
- Naïve substitution
- Barendregt convention
- First-Order Abstract Syntax (FOAS)
- Higher-Order Abstract Syntax (HOAS)
- Parameterized Higher-Order Abstract Syntax (PHOAS)
- Bird and Paterson
- Generalized Generalized De Bruijn

Rules
- β-reduction, `-->ᵦ`, `-->ᵦ⋆`, e.g. (λx.x)(λy.y) -->ᵦ [x:=λy.y]x ≡ᵦ λy.y
- α-conversion, `λx.E =α= [x:=xʹ]E`, e.g. λx.x =α= λxʹ.xʹ
- η-conversion
  - η-reduction, e.g. `F =η= λx.Fx`
  - η-expansion, e.g. `λx.Fx =η= F`

Equivalence of terms
- syntactic equality, `=`
- α-equivalence, `=α=`
- β-equivalence, `=β=`, `≡ᵦ`
- η-equivalence, `=η=`

- Evaluation strategy
  - normal order to WHNF


Normal forms
- normal form
- head normal form
- whnf
- diverging form

Semantics
- small-step semantics, `-->ᵦ`
- big-step semantics,   `-->ᵦ⋆`
- computational semantics
- β-reduction
- self-interpreter
- quoting
- semi-circular interpreter
- STG
- abstract machine

Relations
- small-step semantics, `-->ᵦ`
- big-step semantics,   `-->ᵦ⋆`
- β-equivalence,        `≡ᵦ`
- α-equivalence,        `=α=`
- congruence relations on lambda terms
  - M ⇝ M′ ⇒ M N ⇝ M′ N
  - N ⇝ N′ ⇒ M N ⇝ M N′
- equivalence classes of lambda terms under α-equivalence


Substitution
- naive substitution
- capture-avoiding substitution
- direct substitution
- substitution via environment
- β-reduction:   λx.E -->ᵦ [x:=A]E
- α-equivalence: λxʹ.E =α= [x:=xʹ]E
- β-redex can step via (capture-avoiding) substitution: (λx.f)a ⇝ f[x:=a]
- doing subst with bind (monad)
- doing subst with the S combinator

Type system
- type annotation
- type signature
- type checking
- type inference
- instantiation
- substitution
- generalization
- let-polymorphism
