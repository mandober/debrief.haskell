# Hindley-Milner type system

Language items
- terms, expressions
- evaluation rules
- types
- typing context
- typing rules

Terms
- terms (expressions), `e`
  - literals (constants)
    - bool term constants, { true, false }
    - int  term constants, {-1, 0, 1, 2, …}
  - variables (term vars), `x`
  - abstraction, `λx.e`
  - application
  - let-expression
- expressions vs values
  - values
    - domain of values: host language
    - bottom, `⊥`
  - canonical forms
    - NF
    - HNF
    - WHNF

Types
- monotypes, `τ`
  - base types (type constants, atomic types), B
    - Booleans, `bool`
    - integers, `int`
  - type variables (type params), `α`
  - type ctors, `C τ₀ … τₙ`
- polytypes, `σ`
  - monotypes, `τ`
  - universally quantified types, `∀α̅.σ`
- typing context (typ env), `Γ`
  - empty context, `ε`
  - context extended by a new entry, `x : σ`


```hs
Expressions
 e := true | false       bool term constants
    | -1, 0, 1, …        int term constants
    | x                  term variables
    | e e                term application
    | λ x . e            abstraction (lambda)
    | let x = e in e     le󠀠t expression


BaseTypes
 𝓑 := bool               booleans
    | int                integers


Monotypes
 τ := 𝓑                  base types
    | α                  type variable
    | C τ₀ … τₙ          type ctor (type function)
    | τ₀ → τ₁            function type

Polytypes
 σ := τ                  monotype
    | ∀ α₁ α₂ … αₙ . σ    universally quantified types
    | ∀ α̅ . σ            universally quantified types

TypingContext
 Γ := ε                  empty context
    | Γ, x : σ           context extended with a new entry

EvalContext
 Θ := ∙
    | x : 𝑉
```

`α̅` is a list (vector) of type vars used in a type scheme, e.g.
- `∀ a b. (a -> b -> b) -> b -> [a] -> b`
- `Scheme [a, b, b] (Typ …)`
- a → a
- ∀a. a → a
- C τ instantiated to ∀a. List a
- function ctor (→) is infix type operator: C τ₀ τ₁ --> τ₀ → τ₁



## Definitions

```hs
data Lit
  = LInt Int
  | LBool Bool

data Exp
  = ELit Lit
  | EVar EName
  | EApp Exp Exp
  | ELam EName Exp
  | ELet EName Exp Exp

data Typ
  = TInt
  | TBool
  | TVar TName
  | TFun Typ Typ

type EName  = String
type TName  = String
data Scheme = Scheme [TName] Typ
type TI a   = State Int a
type Ctx    = Map EName Scheme
type Subst  = Map TName Typ

applySubst :: Subst -> Typ -> Typ
newTyVar :: TI Typ
infer :: Ctx -> Exp -> TI (Subst, Typ)

```
