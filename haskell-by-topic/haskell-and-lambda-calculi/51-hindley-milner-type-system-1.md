# Hindley-Milner type system

## Term Syntax

Expressions (terms)

```hs
e := x                      variable
   | e₁ e₂                  application
   | λx.e                   abstraction
   | let x = e₂ in e₁       let-expression
   | true | false           bool term constants
   | 0 | 1 | …              int term constants
```

## Type Syntax

Types

```hs
-- monotypes
  τ := α                    type variables
     | bool                 int type constant
     | int                  bool type constant
     | C τ₁ … τₙ            type ctors

-- type ctors
  C := TC                   nullary type ctor
     | TC τ                 unary type ctor
     | TC τ₁ τ₂             binary type ctor
     | TC τ₁ τ₂ τ₃          ternary type ctor
     | TC τ₁ … τₙ            n-ary type ctor

-- concrete type ctors
     | (→) τ₁ τ₂            function type
     | [] τ                 alt list type
     | (,) τ₁ τ₂            pair type
     | ()                   unit type

-- alt concrete type ctors
     | Fn   τ₁ τ₂           list type
     | List τ               list type
     | Pair τ₁ τ₂           alt pair type

-- polytypes
  σ := τ                    monotypes are polytypes
     | ∀α.σ                 universally quantified types
```

>Monotypes

Monotypes are atomic types (type constants) `bool` and `int` that are here added to the usual set of types comprised of *type variables* and *type constructors*. Type ctors are functions from types to types.

Monotypes
- Booleans, `bool`
- Integers, `int`
- Type variables, `α`, `β`, …
- Type constructors, `TyCtor α₁ … αₙ`
  - function type ctor, `(→) α β` i.e. `α → β`
  - list type ctor, `List α`
  - tuples
    - pair type ctor, `(,) α β` i.e. `(α, β)`
    - 3-tuple type ctor, `(,,) α β γ` i.e. `(α, β, γ)`
    - n-tuple type ctor, `(,…,) α₁ … αₙ` i.e. `(α₁, …, αₙ)`


>Functions

In fact, `→` is the **type ctor of function types**. So the meta-type-ctor symbol `C` above is instantiated at `→`. The `→` is a binary type ctor, so when applied to two types, `α` and `β`, it becomes *saturated* as `(→) α β`, normally writen infix as `α → β`. Thus, the metalang form `C τ₀ τ₁` in the surface lang becomes, e.g. `(→) α β`, i.e. `α → β`, or `bool → nat`, etc. That is to say that we don't need the clause `τ₀ → τ₁` above since it is merely an instance of the more general clause `C τ₀ … τₙ`.

>List

We use the more general caluse to build e.g. **list type**, `C τ` may be instantiated as `List τ`.

>Tuples

The **pair type** where `C τ₀ … τₙ` is instantiated as `(,) τ₀ τ₁`, also writen in the infix form as `(τ₀, τ₁)`, and in the surface lang writen as, e.g. `(α, β)`, or `(bool, nat)`, etc. The type ctor of a pair type is `,`, but we can also have **tuple types** that are somewhat complicated to denote since each tuple has its own type ctor whose form depends on a tuple's arity. The meta-type-ctor `C`, that is, its form `C τ₀ τ₁ … τₙ`, may be instantiated as:
- `C τ₁ τ₂`    : `(,) α β`   i.e. (α, β)
- `C τ₁ τ₂ τ₃` : `(,) α β γ` i.e. (α, β, γ)
- etc.

An `n`-tuple has `n` components and its type ctor has `n - 1` comma symbols. The extreme case of tuples is the empty tuple, `()` that signifies the *top* type, also called *unit*, whose sole term is also denoted by `()`; so `() : ()`. There is no unary tuple.


A type ctor `C` may take any number of type params. A *nullary type ctor* is actually a *constant type* (e.g. bool, int), but nevertheless it is still counted as a type ctor only as one with no type variables.

Among type ctors is always the binary type ctor, `→`, for *function type*. A function type is constructed by applying the function type ctor (→) to two types α and β, obtaining the saturated fucntion type `α → β`.

Other type ctors are added as suitable, and may include list, pair, tuples, etc.

>Polytypes

All monotypes are counted as polytypes.

Type schemes are polytypes. A type scheme is denoted as `∀α̅.σ` above, where `α̅` represents a vector of universally quantified type variables and `σ` is a polytype, e.g. `∀ α β. (α → β → α)`.

We represent type schemes in Haskell with the data type:

```hs
data Scheme = Scheme [String] Typ
```

where the list of strings is a list of type variables.




## Typing context

```hs
Γ := ϵ                empty context
   | Γ, x:α           context extended with a new entry
```


## HM typing rules

```hs
x : σ ∈ Γ
+----------- 𝑽𝑨𝑹
Γ ⊢ x : σ


Γ ⊢ e₁    : τ₁
Γ ⊢ e₀    : τ₁ → τ₂
+-------------------- 𝑨𝑷𝑷
Γ ⊢ e₀ e₁ : τ₂


Γ, x : τ₁ ⊢ e : τ₂
+-------------------- 𝑨𝑩𝑺
Γ ⊢ λx. e : τ₁ → τ₂


Γ, x : σ ⊢ e₁ : τ                -- x has polytype
Γ        ⊢ e₀ : σ                -- arg is polytype
+------------------------ 𝑳𝑬𝑻
Γ ⊢ let x = e₀ in e₁ : τ


Γ ⊢ e : σʹ    σʹ ⊑ σ
+--------------------- 𝑰𝑵𝑺𝑻
Γ ⊢ e : σ


Γ ⊢ e : σ    α ∉ FV(Γ)
+----------------------- 𝑮𝑬𝑵
Γ ⊢ e : ∀α. σ
```


## Description of rules

INST: instantiation typing rule is employed when we can use an expression with a *less general* type (based on **type order**) in place where a *more general* type is expected.



## Subsumption rule and type order


Subsumption rule / type order / subtyping type relation
- subsumption symbols
   - ≺ ≼ ≻ ≽
   - ⊏ ⊑ ⊐ ⊒
- polytype `σ` ≻ `τ` monotype
- polytype `σ` ⊑ `τ` monotype
- subsumption is influenced by variance
- most type ctors are covariant, except
- function type ctor which is
   - covariant in the return type
   - contravariant in the input type
- relation `⊑` is
  - reflexive:     `α ⊑ α`, for all types α
  - antisymmteric: `α ⊑ β` ∧ `β ⊑ α` ⇒ `α ≡ β`
  - transitive:    `α ⊑ β` ∧ `β ⊑ γ` ⇒ `α ⊑ γ`



`σ₁ ⊑ σ₂` or `σ₁ ≻ σ₂` means `σ₂` is a more specific type then `σ₁`, so 
>we can use a *more specific* type where a *more general* type is expected. 
This relation necessarily involved type variables, so e.g. 
∀a.[a] → [a] ⊑ [Int] → [Int]
∀a.[a] → [a] ≻ [Int] → [Int]

Subsumption also depends on **variance**.

Most type ctors are covariant, but function type ctor is peculiar:
- function type ctor is *covariant* in the return type    
  `α` ⊑ `β` ⇒ `int → α` ⊑ `int → β`
- function type ctor is *contravariant* in the input type    
  `α` ⊑ `β` ⇒ `β → int` ⊑ `α → int`



```
           α ⊑ α
           α ⊑ β
           β ⊑ α
         int ⊑ ∀α.α
         int ⊑ int
 int -> bool ⊑ int -> bool
∀a. List a -> List a ⊑ List int -> List int

α ⊑ β ⇒ [α] ⊑ [β]

a -> int ⊑ List int -> a
a -> int ⊑ int -> a

                bool ≻ bool
                 int ≻ int
                   α ≻ α
                ∀α.α ≻ ∀α.α
                bool ≻ ∀α.α
                 int ≻ ∀α.α
∀a. List a -> List a ≻ List int -> List int
     ∀α. α -> List α ≻ ∀α. α -> List int
     ∀α. List α -> α ≻ ∀α. List α -> int
       ∀a.[a] -> [a] ≻ [Int] -> [Int]
```

## Instantiation

INST: instantiation typing rule is employed when we 
can use an expression with a less general type (based on type order) 
in place where a more general type is expected.


## Inference


- Type scheme, `∀α₀…αₙ.τ`, e.g. `forall a b. a → b → a`.
- `Scheme [] Typ`
- `Subst` maps type vars to types. 
- `Env` maps value-level variables to type schemes.

## Substitution

- Substitution maps type vars to types.
- Substitution may be applied to types, schemes, context.

>When applying a substitution to a type we replace all free occurrences of a type variable that occur in the substitution (so not type vars bound by the ∀).

- {a ⟼ int}(a)     = int
- {a ⟼ int}(a → a) = int → int
- {a ⟼ int}(a → b) = int → b
- {a ⟼ int}(∀a.a)  = ∀a.a
- {a ⟼ int, b ⟼ bool}(∀a.a → b) = ∀a.a → bool



## Implementation

https://www.youtube.com/watch?v=ytPAlhnAKro&list=TLGGFw3PXkxaHQswNjA1MjAyNA&index=5

```hs
data Lit
  = LInt Int
  | LBool Bool

data Exp
  = ELit Lit
  | EVar String
  | EApp Exp Exp
  | ELam String Exp
  | ELet String Exp Exp

data Typ
  = TInt
  | TBool
  | TVar String
  | TFun Typ Typ

data Scheme = Scheme [String] Typ

type TI a = State Int a

-- | Maps (term-level) variables to type schemes.
type Ctx = Map String Scheme

-- | Subst maps type vars to types.
--   Subst may be applied to types, schemes, context.
type Subst = Map String Typ

applySubst :: Subst -> Typ -> Typ
applySubst substMap ty = case ty of
  -- {"a" ⟼ int}(a)     = int
  TVar tvar    -> fromMaybe (TVar tvar) (M.lookup tvar substMap)
  -- {"a" ⟼ int}(a → b) = int → b
  TFun arg res -> TFun (applySubst substMap arg) (applySubst substMap res)
  -- Forward other type-exp (TInt and TBool)
  TInt  -> TInt
  TBool -> TBool

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme smap (Scheme qtvars typ) =
    Scheme qtvars (applySubst smap' typ)
  where
  smap' = foldr M.delete smap qtvars
  --      foldr M.delete (Map TName Typ) [TName]

-- | Apply a subst to a context. Watch out for bound tyvars.
applySubstCtx :: Subst -> Ctx -> Ctx
applySubstCtx smap ctx = M.map (applySubstScheme smap) ctx

-- | Composition of substitutions
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.union s1_s2 s1
  where
  s1_s2 = M.map (applySubst s1) s2




newTyVar :: TI Typ
newTyVar = do
  s <- get
  put (s + 1)
  return (TVar ("u" ++ show s))

infer :: Ctx -> Exp -> TI (Subst, Typ)
infer ctx e = case e of
  ELam binder body -> do
    tyBinder <- newTyVar
    let tmpCtx = M.insert binder (Scheme [] tyBinder) ctx
    (s1, tyBody) <- infer tmpCtx body
    return (s1, TFun (applySubst s1 tyBinder) tyBody)

```
