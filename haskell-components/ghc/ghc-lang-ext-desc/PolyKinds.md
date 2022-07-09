# PolyKinds

- Pragma  : `PolyKinds`
- Implies : `KindSignatures`
- Since   : 7.4.1
- Desc    : Allow kind polymorphic types

Kind polymorphism

```hs
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE KindSignatures       #-}

{-# {- DEPRECATED -} LANGUAGE TypeInType #-}
```

The extension `TypeInType` is now deprecated: its sole effect is to switch on `DataKinds` and `PolyKinds` (which implies `KindSignatures`).


## 6.4.11.1. Overview of kind polymorphism

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html

Consider inferring the kind for

```hs
data App f a = MkApp (f a)
```

In Haskell 98, the inferred kind for `App :: (Type -> Type) -> Type -> Type`.

But this is overly specific, because another suitable Haskell 98 kind is:    
`((Type -> Type) -> Type) -> (Type -> Type) -> Type` where `a :: Type -> Type`.

Indeed, without `KindSignatures`, it is necessary to use a dummy ctor to get a Haskell compiler to infer the second kind.

With kind polymorphism (`PolyKinds`), GHC infers the most general kind for App: `App :: forall k. (k -> Type) -> k -> Type`

Thus, the chief benefit of kind polymorphism is that we can now infer these most general kinds and use `App` at a variety of kinds:

```hs
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}

type App :: forall k. (k -> Type) -> k -> Type
data App f a = MkApp (f a)

App Maybe Int           -- `k` is instantiated to Type
App T Maybe             -- `k` is instantiated to (Type -> Type)
data T a = MkT (a Int)  -- `a` is inferred to have (Type -> Type) kind
```

## 6.4.11.2. Overview of Type-in-Type

GHC 8 extends the idea of kind polymorphism by declaring that types and kinds are indeed one and the same. Nothing within GHC distinguishes between types and kinds.

Another way of thinking about this is that the type `Bool` and the "promoted kind" `Bool` are actually identical. Note that term `True` and the type `'True` are still distinct, because the former can be used in expressions and the latter in types.

This lack of distinction between types and kinds is a hallmark of dependently typed languages (full dependently typed languages also remove the difference between expressions and types).

## 6.4.11.3. Principles of kind inference

Generally speaking, when PolyKinds is on, GHC tries to infer *the most general kind for a declaration*.

In many cases (for example, in a datatype declaration) the definition has a RHS to inform kind inference. But that is not always the case.

Type family declarations have no RHS, but GHC must still infer a kind for `F`. Since there are no constraints, it could infer `F :: forall k1 k2. k1 -> k2`, but that seems too polymorphic.

So GHC defaults those entirely-unconstrained kind variables to `Type` and we get `F :: Type -> Type`. You can manually declare `F` to be kind-polymorphic.

```hs
type family F1 a                -- F1 :: Type -> Type
type family F2 (a :: k)         -- F2 :: forall k. k -> Type
type family F3 a :: k           -- F3 :: forall k. Type -> k
type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2
```


When there is a RHS, GHC infers the most polymorphic kind consistent with RHS. Examples: ordinary data type and GADT declarations, class declarations. In case of a class decl the role of "RHS" is played by the class method signatures.

When there is no RHS, GHC defaults arg and result kinds to `Type`, unless otherwise kind-annotated. Examples: data and open type family declarations.

This rule has occasionally-surprising consequences:

```hs
class C a where    -- Class declarations are generalised
                   -- so C :: forall k. k -> Constraint
  data D1 a        -- No right hand side for these two family
  type F1 a        -- declarations, but the class forces (a :: k)
                   -- so   D1, F1 :: forall k. k -> Type

data D2 a   -- No right-hand side so D2 :: Type -> Type
type F2 a   -- No right-hand side so F2 :: Type -> Type
```




If you wish to see the kind indexing explicitly, you can do so by enabling `-fprint-explicit-kinds` and querying G with `:info` command.
