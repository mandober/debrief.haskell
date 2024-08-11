# Types

Types
- monotypes
  - base types, `ℬ`
  - type vars, `α`, set of tvars, `𝒯`     𝒯 𝒱 𝒞 ℬ   𝓣 𝓥 𝓒 𝓑
  - type ctors, `𝒞 τ₁ … τₙ`
- polytypes
  - monotypes
  - polymorphic types

```
monotype
  τ := B             base types
     | α             type vars
     | C τ₁ … τₙ      type ctors

constant types
  B := int
     | bool

polytype
  σ := τ
     | ∀ σ . τ
```



## Types (monotypes)

Types, `Typ`, are
- literal (atomic, constant) types
  - integers, `TInt`
  - Booleans, `TBool`
- type variables, `TVar TName`
- type functions, `TFun Typ Typ`

```hs
type TName = String
data Typ
  = TInt
  | TBool
  | TVar TName            -- tvars
  | TFun Typ Typ

```

## Type schemes (polytypes)

Abstract syntax for type schemes: `data Scheme = Scheme [TName] Typ`.

A type scheme represents a polymorphic type composed of a number of universally quantified type variables, [TName], and a monomorphic type, `Typ`.

A polytype `σ` is a polymorphic type denoted by `∀ α₁ … αₙ . τ`, where `αᵢ` are type variables (encoded as a list of tvars), and `τ` is monotype.


A type scheme `∀ a₁ … aₙ . t` is a type `t` with a number of universally quantified type vars (as a list).
