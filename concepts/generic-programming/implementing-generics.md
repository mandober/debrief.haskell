# Implementing generics

* Datatype-Generic Programming by Andres Löh - Advanced Track @ ZuriHac 2020
https://www.youtube.com/watch?v=pwnrfREbhWY


Haskell libraries that implement facilities for generic programming have to decide on various issues, primarily on the set of *canonical representation types*, which is, preferably the smallest set, of types used to represent all other types uniformly.

Haskell types are usually called *possibly recursive sums of products*, which suggest we first need to make a decision how to uniformly represent product and coproduct types.


## Products

**Product types** can be defined using the *ADT format* either as the fields, or as an tuple, which follow the data ctor. Using *fields* instead of tuples decluters pattern matching later; but using a *tuple* instead of fields allows for `newtype` instead of `data` declaration (most of the time).

Alternatively, products may be defined using the *GADT format*, where the fields become (arrow-separated) parameters of the data ctor (that needs to explicitly return the type being defined). Similarly to using a tuple in ADT (but without the newtype benefit), instead of having two parameters, we can have a 2-tuple of parameters (which only complicates pattern matching later on).

```hs
-- Product as ADT with fields
data Product a b = Product a b
-- Product as ADT with a tuple
newtype Product a b = Product (a, b)

-- Product as GADT
data Product a b where
  -- Product as GADT with params
  Product :: a -> b -> Product a b
  -- Product as GADT with a tuple
  Product :: (a, b) -> Product a b
```

In any case, we need to decide on a type that will uniformly represent products, and that type is unsurprisingly a *pair* or 2-tuple. Additional (more than two) fields are represented by nesting pair ctors.

## Coproducts

**Coproduct types** can be defined using the *ADT format* by separating product (sub)types, called *variants*, using the pipe character. Alternatively, sum types can be defiend using the GADT format where each variant is listed as a separate data ctor.

```hs
-- coproduct ADT
data Coproduct a b = Left a | Right b

-- coproduct GADT
data Coproduct a b where
  Left  :: a -> Coproduct a b
  Right :: b -> Coproduct a b
```

The canonical Haskell type used to represent coproduct is `Either`.

We can define type alises `⨉` for (,), and `+` for `Either` to have type signatures that suggest the connection with type algebra.

## Unit

Defining the type representation for sum types that have fields is evident, but we also need to encode the case of enumeration sum types, e.g. `Bool`.

In the algebra of types, an empty data ctor stands for itself, e.g. the data ctor `True` stands for the value 'true' itself (it needs no fields, as opposed to e.g. `Left a` where `a` is a field).

```hs
data Bool = True | False
-- ≅
type Bool = Maybe ()
-- ≅
type Bool = Either () ()
```

The `Bool` type can be equivalently represented as `Maybe ()`, where `Just ()` could encode 'true', and `Nothing` the 'false' value. However, we have decided on the Either type as the canonical representation of sum types, so the Bool type is represented as `Either () ()`, where `Left ()` stands for `true`, and `Right ()` for 'false'.

Algebraically, The Boolean type is `𝔹 = 1 + 1` (two nullary data ctors).

```hs
-- canonical representations
𝟘 = Void
𝟙 = ()
a + b = Either a b
a ⨉ b = (a, b)

-- 𝟘, Void


-- 𝟙, unit
𝟙 = 0⁰ = a⁰ = (0 -> a) = (Void -> a)

𝔹 = 𝟙 + 𝟙
𝔹 = Maybe 𝟙
Maybe a := 𝟙 + a
List a := 𝟙 + a ⨉ La

Tree a := a + (a ⨉ a) := a + a² = a + (2 -> a) = a + (𝔹 -> a)

𝟙 := (0 -> a) = a⁰ = 𝟙
```


## Structural equality

We need to be able to represent all Haskell types in a uniform manner so that we can compare types for structural equality. Haskell's type system only supports *nominal typing*, so any newly introduced data type is different from all others - two data types are distinct even if they have the exact same structure. To be able to structurally compare data types, we first decide on the canonical set of type representations:
- `Void` for 0 types
- `()` for 1 types (unit)
- `⨉` (pair) for products
- `+` (`Either`) for coproducts

Uisng these we can represent a lot of Haskell types, which we can then compare for structural equality or manipulate generically. The myriad of Haskell types then collapses into a few well-understood types.

Some non-trivial issues regarding type representation include
- how to represent recursion
- how to encode existential types

## Enumaration

The enum or genum method should return the names of data ctors of an enumeration only sum type as a list. This method should not accept non-enum sum types.

```hs
genum @(Rep Bool) -- [True, False]
```

## Metadata

We also need to expose metadata about types: names of type/data ctors, field accessors and their names, whether the fields are packed, their strictness and fixity, etc.
