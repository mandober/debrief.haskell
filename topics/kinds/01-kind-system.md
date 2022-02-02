# Kind system

Types classify terms, kinds classify types. Type and kind are things called sorts. Some dynamic languages are single sorted; they have a single sort, i.e. all terms in a language belong to a single universal type. Static languages are multi-sorted since they have numerous types. Each type is comprised of terms that excusively belong to that type.

Sorts may be considered sets of different order: each type is a set inhabited with certain terms; each kind is a higher-order set inhabited with certain types.

In some languages, in order to avoid the Russell's paradox, the hierarchy of sorts is infinite: `Set₀ ⊂ Set₁ ⊂ Set₂ ⊂ …`.

In Haskell, types have the sort `Set₀` and kinds `Sort₁`, but the hierarchy stops there as the type of `Sort₁` is again `Sort₁` as enforced by the `TypeInType` extension, `Type : Type`. However, there is no risk to succumb to Russell's paradox beause [why-exacyly?]...

Kinds classify types; only types (type-level things) have kinds. The meaning of the term *kind* is somewhere between *type of a type* and a mechanism to specify *arity of type constructors*.


In type theory, a kind is the type of a type constructor. A kind system is essentially a simply typed lambda calculus "one level up", endowed with a primitive type, `Type`, which is the kind of any monomorphic data type.

*Monotypes* (ordinary types) and nullary type constructors, have kind `Type`. Higher order type constructors have kinds of the form `κ₁ -> κ₂`, where `κ₁` and `κ₂` are kinds. In Haskell 98, `Type` is the only inhabited kind, that is, all values have types of kind `Type`. GHC introduces another inhabited kind `#` for unlifted types.

A type constructor of the form `* -> * -> … -> *` is a *first-order Haskell98 type constructor*. These type ctors are promotable. All Haskell98 data constructors of a *first-order Haskell98 data type* are promotable.

A type constructor of the form such as `(* -> *) -> * -> *` is a higher-order type constructor.

```hs
-- no type application -> first-order type constructor
type FirstOrder :: Type -> Type -> Type -> Type
data FirstOrder a b c = FirstOrder a b c

-- type application -> higher-order type constructor
type HigherOrder :: Type -> (Type -> Type) -> Type -> Type
data HigherOrder a b c = HigherOrder a (b c)

-- higher-rank (forall k), higher-order (type application)
type HOHR :: forall k. (k -> Type) -> k -> Type
data HOHR f a = HOHR (f a)
```
