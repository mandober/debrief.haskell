# Type Families in Haskell

`Type Families in Haskell: The Definitive Guide` by Vladislav Zavialov, 2021
https://serokell.io/blog/type-families-haskell

## Contents

- [Summary](#summary)
- [Abstract](#abstract)
- [Type constructor flavours](#type-constructor-flavours)
- [Closed type families](#closed-type-families)


## Summary

* **Indexed Type Families (ITF)** is the most general name that includes all the different variants of the type families.

ITF attributes (dimensions):
-   `type` vs `data`
-     open vs closed
- toplevel vs associted


ITF classification:
- `type` families
  - open
    - toplevel
    - associated
  - closed
    - toplevel
- `data` families
  - open
    - toplevel
    - associated

ITF enumaration:
- Associated open `type` family
- Toplevel   open `type` family
- Toplevel closed `type` family
- Toplevel   open `data` family
- Associated open `data` family


Indexed type families | toplevel | associated
----------------------|----------|------------
Closed type family    |    ✅    |     ❌
Open type family      |    ✅    |     ✅
Open data family      |    ✅    |     ✅


ITF varaints:
- 5 variants total
- 1 closed, 4 open
- 3 toplevel, 2 associated
- 3 type families, 2 data families
- there are no closed data families
- there is no closed Associated Type Family


## Abstract

Type families are one of the most powerful type-level programming features in Haskell. You can think of them as type-level functions, but that doesn't really cover the whole picture. By the end of this article, you will know what they are exactly and how to use them. We will talk about the following topics:

- type constructor flavours
- closed type families
- type constructor arity
- synergy with GADTs
- evaluation order or the lack thereof
- open type families
- overlapping equations
- compatible equations
- injective type families
- associated types
- data families
- non-parametric quantification
- non-linear patterns


## Type constructor flavours

There are several categories to which a type constructor `T` may belong:

```hs
type    T a b         -- type synonym
newtype T a b         -- newtype
data    T a b         -- data type
class   T a b         -- type class
```

The `TypeFamilies` extension introduces two more:

```hs
type family T a b     -- type family
data family T a b     -- data family
```

Type families are divided into:
- open   type families: top-level or associated
- closed type families: top-level

Data families are always open, but can be:
- top-level  open data family
- associated open data family


## Closed type families

Considering just the open-closed attribute, there is only one variant of indexed type families, viz. Toplevel Closed Type Family. So, if the term "closed" is mentioned, there is just one variant of indexed type families we could be discussing.

Closed (toplevel) type families define type-level functions.

```hs
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- term-level function:
append :: forall a. [a] -> [a] -> [a]       -- type signature
append []     ys = ys                       -- equation 1
append (x:xs) ys = x : append xs ys         -- equation 2

-- type-level function:
type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys       -- clause 2
```

Heres a GHCi session to demonstrate how we can use it:

```hs
ghci> :kind! Append [1, 2, 3] [4, 5, 6]
Append [1, 2, 3] [4, 5, 6] :: [Nat]
= '[1, 2, 3, 4, 5, 6]
```

There are many similarities but also some differences:
- Instead of a type signature, there is a *standalone kind signature* 
  introduced with the `type` keyword, and `StandaloneKindSignatures` enabled.
- Nil data ctor, `[]`, is promoted and thus written as `'[]`
  in order to distinguish it from its term-level counterpart.
- Clauses of type family are grouped under the *type family header*.

The *type family header* is probably the most notable difference here, and to understand its importance we must first discuss the notion of arity.

## Type constructor arity

The arity of a type ctor is the number of type args it takes. 
It comes into play when we use higher-kinded types:

```hs
type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)
```

Now, what constitutes a valid arg to `S`? One might be tempted to think that any type constructor of kind `Type -> Type` could be used there, try a few:

```hs
MkS (Just True) Nothing           :: S Maybe
MkS (Left "Hi") (Right 42)        :: S (Either String)
MkS (Identity False) (Identity 0) :: S Identity
```

So `Maybe`, `Either String`, and `Identity` have all worked fine. But what about a type synonym?

```hs
type Pair :: Type -> Type
type Pair a = (a, a)
```

From the standalone kind signature, we see that it has the appropriate kind `Type -> Type`. GHCi also confirms this:

```hs
ghci> :kind Pair
Pair :: Type -> Type
```

And yet, any attempt to use `S Pair` is unsuccessful:

```hs
ghci> MkS (True, False) (0, 1) :: S Pair
<interactive>:6:29: error:
    • The type synonym 'Pair' should have 1 argument,
      but has been given none
```

Due to certain design decisions in the type system, **type synonyms cannot be partially applied**.

In the case of `Pair`, we say that its arity is 1, as it needs one argument: `Pair Bool`, `Pair Integer`, and `Pair String` are all fine. On the other hand, `S Pair` or `Functor Pair` are not.

A type constructor whose arity requirements are met is called saturated, and unsaturated otherwise.

Note that we only need the notion of arity for type constructors that can reduce to other types when applied to an argument. For instance, `Pair Bool` is equal not only to itself but also to `(Bool, Bool)`

```hs
Pair Bool ~ Pair Bool     -- by reflexivity
Pair Bool ~ (Bool, Bool)  -- by reduction
```

On the other hand, `Maybe Bool` is only equal to itself:

```hs
Maybe Bool ~ Maybe Bool   -- by reflexivity
```

`Maybe` is a generative type ctor, while `Pair` is a non-generative.

> **Non-generative type constructors** have arities assigned to them and must be used saturated.

> **Generative type constructors** are not subject to such restrictions, so we do not apply the notion of arity to them.

Type family applications can also reduce to other types (are non-generative):

```hs
Append [1,2] [3,4] ~ Append [1,2] [3,4]  -- reflexivity
Append [1,2] [3,4] ~ [1, 2, 3, 4]        -- reduction
```

Therefore, *type families are non-generative* and have arities assigned to them. The arity is determined at definition site by taking into account the kind signature and the header:

```hs
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
```

The header contains `Append xs ys` rather than `Append xs` or simply `Append`. So, at first glance it may seem that the arity of `Append` is 2. However, we must also account for the `forall` bound variable `a`. In fact, even if you write `Append [1,2] [3,4]`, internally it becomes `Append @Nat [1,2] [3,4]`. Hence the arity of `Append` is 3.

The arity would still be 3 even if we didn't write out the `forall` explicitly:

```hs
type Append :: [a] -> [a] -> [a]
type family Append xs ys where

:kind! Append
Append :: [a] -> [a] -> [a]

:kind! Append @Nat
Append @Nat :: [Nat] -> [Nat] -> [Nat]

:kind! Append @Nat '[1,2]
Append @Nat '[1,2] :: [Nat] -> [Nat]
= Append '[1, 2]

:kind! Append @Nat '[1,2] '[3,4,5]
Append @Nat '[1,2] '[3,4,5] :: [Nat]
= '[1, 2, 3, 4, 5]

:kind! Append @_ '[1,2] '[3,4,5]
Append @_ '[1,2] '[3,4,5] :: [Nat]
= '[1, 2, 3, 4, 5]
```

But why is a header important? Couldn't we deduce the arity by counting the quantifiers in the kind signature? Well, that might work in most cases, but here is an interesting counter-example:

```hs
type MaybeIf :: Bool -> * -> *
type family MaybeIf b t where
  MaybeIf True  t = Maybe t
  MaybeIf False t = Identity t
```

This definition is assigned the arity of 2, and we can use it by applying it to two arguments:

```hs
data PlayerInfo b = MkPlayerInfo
  { name  :: MaybeIf b String
  , score :: MaybeIf b Integer
  }
```

This could be useful when working with a database. When reading a player record, we would expect all fields to be present, but a database update could touch only some of the fields:

```hs
dbReadPlayerInfo :: IO (PlayerInfo False)
dbUpdatePlayerInfo :: PlayerInfo True -> IO ()
```

In `PlayerInfo False` the fields are simply wrapped in Identity, e.g. `MkPlayerInfo { name = Identity "Jack", score = Identity 8 }`.

In `PlayerInfo True` the fields are wrapped in Maybe and therefore can be Nothing, e.g. `MkPlayerInfo { name = Nothing, score = Just 10 }`.

However, `MaybeIf` cannot be passed to `S`

```hs
ghci> newtype AuxInfo b = MkAuxInfo (S (MaybeIf b))
  • The type family 'MaybeIf' should have 2 arguments, but has been given 1
  • In the definition of data constructor 'MkAuxInfo'
    In the newtype declaration for 'AuxInfo'
```

Fortunately, this problem is solved by a minor adjustment to the definition of `MaybeIf` by removing the second type var `t`:

```hs
type MaybeIf :: Bool -> * -> *
type family MaybeIf b where
  MaybeIf True  = Maybe
  MaybeIf False = Identity
```

The kind signature remains unchanged, even though the `t` parameter is removed from the header and the clauses. With this tweak, the arity of `MaybeIf` becomes **1** and the definition of `AuxInfo` is accepted.

Exercise: determine the arity of `Not`, `FromMaybe`, and `Fst`.
