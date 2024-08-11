# HM elements

- term (exp)
- term variable (exp var), evar, eVar, term-level name
  - type EName = String
- type: Typ, typ, ty
- type variable, tvar, tVar, type-level names
  - type TName = String
- substitutions:
  - k-v map of substitutions: `type Subst = Map TName Typ`
  - Subst maps tvar to types
- Type schemes:
  - scheme: `data Scheme = Scheme [TName] Typ`
  - a pair of list of tvar and types
- Type inference monad
  - `type TI a = State Int a`
  - state with `Int` used to generate fresh var names
- Context maps enames to type schemes, `type Ctx = Map EName Scheme`

- vars
  - evar: term (exp) vars, `EVar`
  - tvar: type vars, `TVar`
- types:
  - `Typ`: type
    - `typ`: type as a pattern name
  - `TVar`: term (exp) vars
    - `tvar`: tvar as a pattern name
  - `TFun`: function type. `TFun Typ Typ`
    - `tfun`: fn type as a pattern name
- context
  - `Ctx`: context datatype
  - `ctx`: context as a pattern name


data Lit
- LInt Int
- LBool Bool

data Exp
- ELit Lit            -- ^ @ELit (LInt 42), ELit $ LBool True@
- EVar EName          -- ^ Variables, @x@
- ELam EName Exp      -- ^ Abstractions, @λx.B@
- ELet EName Exp Exp  -- ^ Let-expresion, @let x = A in B@ = @(λx.B)A@
- EApp Exp Exp        -- ^ Application, @M N@


data Typ
  = TInt           -- ^ @Base type int,  ELit (LInt 10000 :: Lit) :: TInt@
  | TBool          -- ^ @Base type bool, ELit (LBool True :: Lit) :: TBool@
  | TVar TName     -- ^ Type variables, @α@
  | TFun Typ Typ


TYPES
- Typ:
  - TBool
  - TInt
  - TVar "t0"
  - TFun TInt TBool
  - TFun (TVar "t0") (TVar "t0")
- TName (String), `"t0"`, `"t1"`
- types, `Typ`:
  - bool type, `TBool`
  - int  type, `TInt`
  - tvar,      `TVar "t1"`
  - tyfn,      `TFun Typ Typ`
               `TFun (TVar "t0") (TVar "t0")`

SCHEMES
- Scheme [TName] Typ
  ∀      [tvar]  typ
  scheme [tvar]  typ
- list ↗ contains (quantified) tvars, or is empty
  - list contains tvar names (`TNames`) as strings: "t0", "t1", …
  - list contains tvars understood to represent universally quantified tvars
  - list of forall tvars
  - list of tvars where each one is a *forall tvar*
  - ["t0","t1"] means `forall t0 t1`
- scheme is a list-of-tvars and types
- nonquantified vs quantified schemes
  - nonquantified schemes
  - quantified schemes

- __Non-quantified Schemes__
  - All types may be expressed (collected) as schemes:
    All `Typ` are `Schemes` of the form: `Scheme [] Typ`.
    i.e. they all have the empty list of *quantified tvars*.
  - empty tvar quantifier list: `Scheme [] Typ`
     `Scheme [] Typ`
    - Scheme [] TInt
    - Scheme [] TBool
    - Scheme [] (TVar "t0")
    - Scheme [] (TFun (TVar "t0") (TVar "t0"))
    - Scheme [] (TFun (TVar "t0") (TFun (TVar "t1") (TVar "t0")))
      ∀ [] . (t0 → (t1 → t0))

- __Quantified Schemes__
  - have nonempty list of *quantified tvars*
  - ∀ t₀ t₁ . t₀ → t₁ → t₁
  - ∀ [t₀, t₁] . (t₀ → (t₁ → t₁))
  - Scheme [TName] Typ
  - Scheme ["t0", "t1"] Typ
  - ∀ t0 t1 . t0 → t1 → t0
    ∀ [t0 t1] . (t0 → (t1 → t0))
    Scheme ["t0", "t1"] (TFun (TVar "t0") (TFun (TVar "t1") (TVar "t0")))


- Subst
- type Subst Map TName Typ

Subst is a map
- Subst maps  tvar   ⟼ type
-             TName  ⟼ Typ
-             String ⟼ Typ
- Subst mapping example:
    { "t0" ⟼ TInt
    , "t1" ⟼ TBool
    , "t2" ⟼ TFun "t0" "t0"
    , "t2" ⟼ TFun (TVar "t0") (TVar "t0")
    , "t3" ⟼ TVar "t1"
    }
