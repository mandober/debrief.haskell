# Operator symbols

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#infix-type-constructors-classes-and-type-variables

* A type ctor or class name can be any non-reserved operator
* Symbols used in types are treated as *capitalized identifiers*
* Data ctors are required to begin with a colon, `:`
* data/type declarations can be infix
* type ctor/param can be infix with back-quotes
* symbolic type ctor can be prefix with parentheses
* the function ctor (->) is **infixr -1**

```hs
-- data declarations can be infix:
data a :*: b = Foo a b

-- type synonyms can be infix:
type a :+: b = Either a b

-- class name declaration as infix:
class a :=: b where

-- data/type decl must be parenthesised if you want further args:
data (a :**: b) x = Baz a b x
type ((:**:) a b) x = Baz a b x

-- type constraints can be infix:
x :: Int :*: Bool

-- class constraints can be infix:
f :: (a :=: b) => a -> b

-- type ctors can be infix with back-quotes:
Int `Either` Bool === Either Int Bool

-- type params can be infix with back-quotes:
Int `a` Bool === a Int Bool

-- parentheses allow symbolic type ctor to be prefix:
(:*:) Int Bool === Int :*: Bool
```

Fixities may be declared for:
- data constructors
- type constructors
- classes

However, when a type ctor has the same name as the data ctor, one cannot distinguish between the two in a fixity declaration; a fixity declaration sets the fixity for a data ctor and the corresponding type ctor. For example, `infixl 7 T, :*:` sets the fixity for both type ctor `T` and data ctor `T`, and similarly for `:*:`.


## Type operators

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-operators

> In types, an operator symbol is normally treated as a type parameter!

The lang pragma *TypeOperators* (implies ExplicitNamespaces, available since GHC 6.8.1) allows the use and definition of types with operator names. Namely, **in types**, an operator symbol (+) is treated as **type param**, like `a`.

```hs
type T (+) = ((+), (+)) === type T a = (a,a)

f :: T Int -> Int
f (x,y)= x
```










## Compound glyphs as operators

```
:|
:|:
:->
:=>
:=
:::
:*:
:+:
:**:
:++:


```



## Logic

∧ ∨     ⋀ ⋁
⊢ ⊣     ⊬   ⊨ ⫤


⨁ ⨂ ⨝ ⨼
↣ ↝ ↪ ↦ ↯ ↺ ↱ ↵ ⇿
⁅ ⁆
♪ ₰ ∅ √ ∛ ∜ ∤ ∣ ≈
⋏ ⋎ ⋘ ⋙ ⋋
º ‗
