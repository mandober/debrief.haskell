# Terms

## Blunt

```hs
data Exp where
  Var :: String -> Exp
  Abs :: String -> Exp -> Exp
  App :: Exp -> Exp -> Exp

data Exp
  = Var String
  | Abs String Exp
  | App Exp Exp

x1 = Lam "x" (Lam "y" (Var "x"))

-- separate Var type
newtype Var = Var String
data Exp
  = EVar Var
  | EAbs Var Exp
  | EApp Exp Exp
```

## Parameterized var type

```hs
data Exp a where
  Var :: a -> Exp a
  Abs :: a -> Exp a -> Exp a
  App :: Exp a -> Exp a -> Exp a

data Exp a
  = Var a
  | Abs a (Exp a)
  | App (Exp a) (Exp a)

-- separate, existentially parameterized Var type
newtype Var = forall a. Var a
data Exp
  = EVar Var
  | EAbs Var Exp
  | EApp Exp Exp
```




## Misc

Terms (exp)
- term variables, `EVar`
- abstraction, `EAbs`
- application, `EApp`
- let-expression, `ELet`
- term constants (literals), `ELit`
  - L0 literal integers, `LInt Int`  , LInt 1     :: Lit
  - L0 literal Booleans, `LBool Bool`, LBool True :: Lit
  - L1 literal integers,          ELit LInt 1     :: Exp
  - L1 literal Booleans,          ELit LBool True :: Exp

Symbols
  - Sets of names
    - set of evars, `𝒱` = {x,y,z,…}
    - set of tvars, `𝒯` = {a,b,c,…α,β,γ,…}
    - set of meta-tvars, `ℳ` = {a,b,c,…α,β,γ,…}
  - monotypes, τ
  - polytypes, σ
  - evars, x
  - tvars, α
  - term (exp), e
  - scheme
  - subst


𝒶 𝒷 𝒸 𝒹 ℯ 𝒻 ℊ 𝒽 𝒾 𝒿 𝓀 𝓁 𝓂 𝓃 ℴ 𝓅 𝓆 𝓇 𝓈 𝓉 𝓊 𝓋 𝓌 𝓍 𝓎 𝓏
𝓪 𝓫 𝓬 𝓭 𝓮 𝓯 𝓰 𝓱 𝓲 𝓳 𝓴 𝓵 𝓶 𝓷 𝓸 𝓹 𝓺 𝓻 𝓼 𝓽 𝓾 𝓿 𝔀 𝔁 𝔂 𝔃
𝒜 ℬ 𝒞 𝒟 ℰ ℱ 𝒢 ℋ ℐ 𝒥 𝒦 ℒ ℳ 𝒩 𝒪 𝒫 𝒬 ℛ 𝒮 𝒯 𝒰 𝒱 𝒲 𝒳 𝒴 𝒵
𝓐 𝓑 𝓒 𝓓 𝓔 𝓕 𝓖 𝓗 𝓘 𝓙 𝓚 𝓛 𝓜 𝓝 𝓞 𝓟 𝓠 𝓡 𝓢 𝓣 𝓤 𝓥 𝓦 𝓧 𝓨 𝓩


𝒯 𝒱 ℳ
𝓣 𝓥 𝓜


a b c d e f g h i j k l m n o p q r s t u v w x y z
A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

```hs
data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord)

type EName = String

-- | Abstract syntax for terms
data Exp
  = ELit Lit
  | EVar EName            -- evars
  | EApp Exp Exp
  | EAbs EName Exp
  | ELet EName Exp Exp
  deriving (Eq, Ord)
```
