# Polymorphism in Haskell

- concrete values
- concrete types
- polymorphic values
- polymorphic types
- instantiation
- specialization
- abstraction
- generalization

## As briefly as possible

```hs
:type () :: ()     -- type of () term is ()
:kind () :: Type   -- kind of () type is Type
:kind Type :: Type -- type of Type is Type due to the TypeInType axiom
```

However, the Russell paradox does not bite us in Haskell despite the `TypeInType` axiom. Other languages, like Agda, have an infinite hierarchy of types:

```hs agda
:type () : Set = Set₀
:type Set : Set₁
:type Set₁ : Set₂
…
:type Setω : Setω₁
:type Setω₁ : Setω₂
…
```

the subscripted indices are called *levels*. Agda has no kinds, just types of types, ad infinitum; well, at least until `Setω`


```hs
:type 0 :: Num a => a
:type (0 :: Int) :: Int
:kind Int :: Type
:kind Bool :: Type
:kind () :: Type
:kind (,) :: Type -> Type -> Type
:kind (,,) :: Type -> Type -> Type -> Type
:kind (,,,) :: Type -> Type -> Type -> Type -> Type
:kind List :: Type -> Type

:kind Either :: Type -> Type -> Type
:kind Either String :: Type -> Type
:kind Either String Int :: Type

:kind (->) :: Type -> Type -> Type
:kind ((->) Int) :: Type -> Type
:kind (Int ->) :: Type -> Type
:kind ((->) Int Bool) :: Type
```

If *concrete types* are like values, and *polymorphic types* are like functions, what is akin to higher-order functions on types?
>Higher-kinded types correspond to higher-order functions on types.

Higher-kinded types are types with kind signatures that have parenthesis somewhere on the left side, e.g. `(* -> *) -> * -> *`.

```hs
data Collection f a = Collection (f a) deriving (Show)

>>> :k Collection
Collection :: (Type -> Type) -> Type -> Type


:kind Functor :: (Type -> Type) -> Constraint
```

The kind signature of `Functor` is `(* -> *) -> Constraint`, which means that it takes a type constructor (e.g. Maybe) and returns something of kind `Constraint`.

While data types are of kinds like `*` or `* -> *`, etc., typeclasses are of a kind like `* -> Constraint` or `(* -> *) -> Constraint`.

`Constraint` is the kind of class constraints - it covers anything that can appear on the left side of the arrow when defining a type.

In general, since types with the kind of `* -> *` are uninhabited, we canbot work with them, but we can make them instances of classes like Functor; of classes that require that the type ctor has kind `* -> *`.

some kinds
- Concrete types:      `*`             e.g. `Bool`
- Polymorphic types:   `* -> *`        e.g. `Maybe`
- Higher-kinded types: `(* -> *) -> *` e.g. `Alt` from `newtype Alt f a`

Alternative and Applicative wrappers for monoids
https://hackage.haskell.org/package/base/docs/Data-Monoid.html

When `PolyKinds` is enabled, we get *kind polymorphism*, and kinds like: `k -> *` i.e. `forall k. k -> *`, which use forall quantifier at the kind level.




## Value and type polymorphism in Haskell

Unsurprisingly, Haskell has type polymorphism, but it also has value (term) polymorphism. Some literals - particularly number literals like `0` - are polymorphic (values). This is contrasted by *monomorphic terms* (literals) - literals like `True` whose type is always easily determined.

Ignoring bottom(s),

- The `Void` type is uninhabited so there are no terms of this type. 

- The unit type has a single term, `() :: ()`. Whenever we see the value `()`, we immediately know its type. Whenever we see the type `()`, we immediately know the only possible term it types (classfies). Types with a single value are called *singletons* (akin to 1-sets) and they can act as a bridge between terms and types (which get erased and are not around at RT).

- The type `Bool` classifies two terms, `True` and `False`. Whenever we see the value 'True' (or 'False'), we know its type. But when we have a type `Bool`, we cannot know which of the two terms it types.

- The type `Char` classifies a large (but of course finite) number of characters. When we encounter a literal `'a'`, we know it must be typed by `Char`. But when we have a type `Char`, we cannot know which exact char should be typed by it.

- On the other hand, the type of the literal `0` could be any of the several numeric types (Integer, Nat, Int, Int8, Int16, Int32, Word, Word8, Word16, Word32, Float, Double, Rational, Complex, …). When encountering such literals, we cannot conclude anything about their type, except that it is numeric. The type given to a literal like `0` is the broadest possible type, `Num a => a`, since the class `Num` *classifies numeric types*.

On one hand, we have a concrete type, like `Bool`, which has copncrete terms like 'True' and 'False'. It doesn't get any more "flat" than this, except maybe with the unit type (and the fact that `Bool` is really a sum type, so a compound and not an atomic one; it seems `Char` is the most atomic of the primitive types).




### Value polymorphism



## Value polymorphism

Even in math
- `0` can belong to the set ℕ or ℤ, ℚ, ℝ, ℂ, ℍ, 𝕆, …
- `-1` can only belong to ℤ, ℚ, ℝ, …
- `3/4` can only belong to ℚ, ℝ, ℂ, …
- `0.333…` can only belong to ℝ, ℂ, ℍ, 𝕆, …
- `π` can only belong to ℝ, ℂ, ℍ, 𝕆, …
- `-2i` can only belong to ℂ, ℍ, 𝕆, …

This shows that sometimes the form of a literal value determines (or at least constraints) the possible types it belongs to.

What is the type of 0?

```hs
x :: Num a => a
x = 0
```

A big differences between sets and types is that an object can be a memeber of multiple sets, while any term can only ever be a member of a single type.

>What about bottom?

In fact, `⊥` can belong and does belong to any and all types, at least in Haskell. More precisely, it belongs to any lifted type.

A *lifted type* is exactly a type with the bottom added: `ℕ` is the set of naturals, and `ℕ ⋃ {⊥}` is the lifted set of naturals.

ℕ = {0, 1, 2, …}
ℕ̝ = ℕ⫠ = ℕ˔ = ℕ ⋃ {⊥} = {⊥, 0, 1, 2, …}

In Haskell, types are augmented with `⊥` so that *divergent computation* can be represented: if a computation is successful it yields a normal value, if it fails it yields ⊥.

>forall τ. τ ⋃ {⊥} = τ˔

- `τ` is an unlifted type
- `τ˔` is a lifted type

This also implies that no Haskell type is ever truly empty as it has at least the bottom term. That is, *lifted types* are never truly empty. Haskell also has the corresponding unlifted types, which do not have the bottom term, i.e. they are *bottomless types* (vs fat-bottom types).

Even a value like `0` only belongs to one type, which is not as obvious as with non-polymorphic values like `True` - `True` is a concerete value of the type `Bool`.

But `0` is actually a *polymorphic value* of the *polymorphic type* `Num a => a`. The literal `0` seemingly belongs to many numeric types, but this is not true since `0` is a polymorphic value. When it is instantiated to a concrete type, like `Int`, it will still look the same but then it will be a bona fide element of the `Int` type. Before specialization, `0` is a polymorphic value and actually belongs to no type, but potentially belongs to numeric types.

`0` is a literal, and it represents a polymorphic numeric value - its type is also polymorphic, `0 :: Num a => a`.





## Type polymorphism

>Which type expression accept the most types/terms?

Obviously, it's gonna be a polymorphic, forall-quantified, type variable, but to make it as broad as possible it should also be kind polymorphic.

```hs
id :: forall (a :: k). a -> a
id x = x

x :: forall a. a
x = undefined
```

Having a polymorphic value (term) `forall a. a` may represent the broadest or the most narrow choice of types. If we need to materialize a value of this polymorphic type, we see that the fitting values must be at the intersection of all types. Naturally, there is nothing there except the ubiquotous `undefined` value that inhabits each and every type in Haskell; no Haskell type is truly empty. Any type, known to be inhabited or not, always contains at least the `undefined` term (value). In fact, no lifted type in Haskell is truly empty, and the fact that it contains `undefined` is the reason why it is called *lifted*.

On the other side, from the caller's perspective, if a function requires an argument of type `forall a. a`, then we can pass it a term of any type. Inside that function, there will be nothing reasonable to do with that polymorphic argument except to return it as is; inspecting the value of a polymorphic type is impossible.
