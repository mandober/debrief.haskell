# Datatype promotion

The `DataKinds` pragma enables datatype promotion such that a type declaration, which normally has type-level (type ctor) and term-level (data ctors) constructors, also means getting a new kind (named the same as the type ctor) that is inhabited by the same number of type ctors (which are themselves uninhabited at the term level) as there are data ctors.

```hs
-- the standard type declaration
data Maybe a = Nothing | Just a

-- and its 2 data ctors
>>> :type Nothing
Nothing :: forall a. Maybe a
>>> :type Just
Just :: forall a. a -> Maybe a

-- are now promoted, introducing the new kind 'Maybe'
-- that classifies two new type ctors (both uninhabited):
>>> :kind! 'Nothing
'Nothing :: forall a. Maybe a
= 'Nothing

>>> :kind! 'Just
'Just :: forall a. a -> Maybe a
= 'Just

-- The type ctor, 'Maybe' and its promoted instance, i.e. the new kind 'Maybe'
-- are now indistinguishable.
>>> :kind! Maybe
Maybe :: Type -> Type
= Maybe
```

Universe:
- base kind `Type`, ex `*`
- unary kind, `Type -> Type`, ex `* -> *`
- binary kind, `Type -> Type -> Type`, ex `* -> * -> *`
- ternary kind, `Type -> Type -> Type -> Type`, ex `* -> * -> * -> *`
- kind κ := Type | κ -> Type

Before promotion:
- type ctor `Maybe`
- data ctor `Nothing`
- data ctor `Just`




```js
★-kinds: 1
// the base kind, 𝚃𝚢𝚙𝚎
₁k⁰ :: *

★★-kinds: 1
// 1 ⨯ unary kind function
₂k¹ :: * -> *

★★★-kinds: 2
// 1 ⨯ unary kind function:
₃k¹ :: (* -> *) -> *
// 1 ⨯ binary kind function:
₃k² :: * -> (* -> *)
//  :: * -> * -> *

★★★★-kinds:
// 3-ary ⨯ 1
₄k³ :: * -> (* -> (* -> *))
//     * -> (* -> * -> *)
//     * -> * -> * -> *

// 2-ary ⨯ 2
ᴬ₄k² :: * -> ((* -> *) -> *)
//     * -> (* -> *) -> *

ᴮ₄k² :: (* -> *) -> (* -> *)
//     (* -> *) -> * -> *
```



```js
kind κ := * | κ -> *

1) *
2) (k = *) * -> *
3) (k = * -> *) * -> * -> * = * -> (* -> *)

k = * -> (* -> *)


κⁿ : κ -> Type
κ⁰ :                                                      Type
κ¹ :                                              Type -> Type
κ² :                                     Type -> (Type -> Type)
κ³ :                            Type -> (Type -> (Type -> Type))
κ⁴ :                   Type -> (Type -> (Type -> (Type -> Type)))
κ⁵ :          Type -> (Type -> (Type -> (Type -> (Type -> Type))))
κ⁶ : Type -> (Type -> (Type -> (Type -> (Type -> (Type -> Type)))))






kind⁰ :: Type
kindᶠ :: κ -> Type

if κ : Type then
  kind⁰ :: κ
  kind⁰ :: Type
let
  Int :: Type
  Maybe Char :: Type
  Either Bool String :: Type

if κ = Type then
  kind₁ :: κ    -> Type
  kind₁ :: Type -> Type
let

if κ = Type -> Type then
  kind₂ :: κ    -> Type
  kind₂ :: Type -> Type -> Type


```
