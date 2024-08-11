# Type families in Haskell

* Type families in Haskell - Vladislav Zavialov, 2021
https://serokell.io/blog/type-families-haskell

* Type families @HaskellWiki
https://wiki.haskell.org/GHC/Type_families

* ImpredicativeTypes and TypeFamilies - Richard Eisenberg
https://www.youtube.com/watch?v=l5veKgGxXd4&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=4


## Overview

```
Type and Data families | Top-level | Associated
-----------------------------------------------
Type family Open       | ✔️        | ✔️
Type family Closed     | ✔️        | ❌
Data family Open       | ✔️        | ✔️
Data family Closed     | ❌        | ❌
```

## TOC

<!-- TOC -->

- [Overview](#overview)
- [TOC](#toc)
- [Contents](#contents)
- [Type constructor categories](#type-constructor-categories)
- [Closed type families](#closed-type-families)
- [Type constructor arity](#type-constructor-arity)
- [Synergy with GADTs](#synergy-with-gadts)
- [Evaluation order](#evaluation-order)
- [Open type families](#open-type-families)
- [Overlapping equations](#overlapping-equations)
- [Compatible equations](#compatible-equations)
- [Injective type families](#injective-type-families)
- [Associated types](#associated-types)
- [Constrained Type Families](#constrained-type-families)
- [Data families](#data-families)
- [Non-parametric quantification](#non-parametric-quantification)
- [Non-linear patterns](#non-linear-patterns)
- [Conclusion](#conclusion)

<!-- /TOC -->

## Contents

- Type constructor categories
- Closed type families
- Type constructor arity
- The synergy with GADTs
- Evaluation order
- Open type families
- Overlapping equations
- Compatible equations
- Injective type families
- Associated types
- Data families
- Non-parametric quantification
- Non-linear patterns

## Type constructor categories

There are several categories which a type ctor `T` may belong to:
- data type:      `data T a b`      (both ADT and GADT syntax)
- newtype:        `newtype T a b`
- type synonym:   `type T a b`
- type class:     `class T a b`
- type family:    `type family T a b`
- data family:    `data family T a b`

The last two become available by enabling the *TypeFamilies* extension.

**Type families** are further subdivided into *closed* and *open*, and open type families can be either *top-level* or *associated* with a class:

Type families | Top-level | Associated
--------------|-----------|-----------
Open          | ✔️        | ✔️
Closed        | ✔️        | ❌


Type families
- open   associated type family
- open   top-level  type family
- closed top-level  type family

There are no closed associated type families because type classes are open.

**Data families** are always open, but can also be either *top-level* or *associated* with a class:

Data families | Top-level | Associated
--------------|-----------|-----------
Open          | ✔️        | ✔️
Closed        | ❌        | ❌


Data families
- open associated data family
- open top-level  data family

There are no closed associated data families because type classes are open. 
There are no closed top-level data families because... (?)


## Closed type families

Closed type families are type-level functions, similar to term-level functions.

Term-level functions are defined in various equivalent syntactic forms, but using clauses makes them easily comparable with closed type families, which are also functions but at the type level.

```hs
-- term-level function
append :: forall a. [a] -> [a] -> [a]       -- type signature
append []     ys = ys                       -- clause 1
append (x:xs) ys = x : append xs ys         -- clause 2

-- type family is a type-level function
-- top-level and closed type family
type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys       -- clause 2
```

Differences between the term and type level functions
- Instead of type signatures, we use standalone kind signatures
- promoted data ctors have a tick prepended to distinguish them from type ctors
- clauses of *top-level closed type family* are grouped under the type family header, whereas term-level functions do not have headers

## Type constructor arity

The arity of a type constructor is the number of arguments it requires at use sites. It comes into play when we use higher-kinded types.

```hs
type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)
```

Now, what constitutes a valid argument to `S`? One might be tempted to think that any type constructor of kind `Type -> Type` could be used there. Let's try a few:

```hs
MkS (Just True) Nothing           ::   S Maybe
MkS (Left "Hi") (Right 42)        ::   S (Either String)
MkS (Identity False) (Identity 0) ::   S Identity
```

So Maybe, Either String, and Identity have all worked fine. But what about a type synonym?

```hs
type Pair :: Type -> Type
type Pair a = (a, a)
```

We see that `Pair` has the appropriate kind `Type -> Type`, and yet, any attempt to use `S Pair` is unsuccessful:

```hs
ghci>>> MkS (True, False) (0, 1) :: S Pair
• The type synonym 'Pair' should have 1 argument, but has been given none
```

Due to certain design decisions in GHC type system, type synonyms cannot be partially applied (but can be partially defined, or at least η-contracted).

In the case of `Pair`, its arity is 1: `Pair Bool` and `Pair String` are fine. On the other hand, `S Pair` or `Functor Pair` are not. The use of a type constructor where its arity requirements are met is called *saturated*, and *unsaturated* otherwise.

Note that we only need the notion of arity for type ctors that can be reduced to other types when applied to a type argument.

For example, `Pair Bool` is equal not only to itself but also to `(Bool, Bool)`:

```hs
Pair Bool ~ Pair Bool     -- reflexivity
Pair Bool ~ (Bool, Bool)  -- reduction
```

On the other hand, `Maybe Bool` is only equal to itself:

```hs
Maybe Bool ~ Maybe Bool   -- reflexivity
```

Thus we call `Maybe` a generative type ctor, while `Pair` is non-generative.

>**Non-generative type ctors** have arities assigned to them and must be used saturated.
>**Generative type ctors** are not subject to such restrictions, so we do not apply the notion of arity to them.


Type family applications can also reduce to other types:

```hs
Append [1,2] [3,4] ~ Append [1,2] [3,4]  -- reflexivity
Append [1,2] [3,4] ~ [1, 2, 3, 4]        -- reduction
```

>Therefore, type families are non-generative and have arities assigned to them.

The arity is determined at the definition site by taking into the account the kind signature and the header:

```hs
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
```

The header says `Append xs ys`, rather than `Append xs`, or simply `Append`. So, at first glance, it seems that the arity of `Append` is 2. However, we must also account for the forall-bound type variable `a`. In fact, even if you write `Append [1,2] [3,4]`, internally it becomes `Append @Nat [1,2] [3,4]`. Hence the arity of `Append` is 3, which would also be true even if we didn't write the `forall` explicitly.

Why is the header important? 
Couldn't we deduce the arity by counting the quantifiers in the kind signature?

While that may work in most cases, here is an interesting counter-example:

```hs
type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b t where
  MaybeIf True  t = Maybe t
  MaybeIf False t = Identity t
  -- or, in this case, right down to `t` (without Identity)
  MaybeIf False t = t
  -- although `Identity` keeps the functorial kind
```

The `MaybeIf` is assigned the **arity of 2**, and we can use it by applying it to two arguments:

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

If `PlayerInfo 'False`, the fields are simply wrapped in `Identity`. 
If `PlayerInfo 'True`, the fields are wrapped in `Maybe`, so can be `Nothing`.

```hs
-- | if b := 'True then
data PlayerInfo 'True = MkPlayerInfo
  { name  :: MaybeIf 'True String -->> Maybe String
  , score :: MaybeIf 'True Int    -->> Maybe Int
  }

-- | if b := 'False then
data PlayerInfo 'False = MkPlayerInfo
  { name  :: MaybeIf 'False String -->> Identity String
  , score :: MaybeIf 'False Int    -->> Identity Int
  }
```


However, `MaybeIf` cannot be passed to `Space`:

```hs
newtype AuxInfo b = MkAuxInfo (Space (MaybeIf b))

• The type family 'MaybeIf' should have 2 arguments, but has been given 1
• In the definition of data constructor 'MkAuxInfo'
  In the newtype declaration for 'AuxInfo'
```


Fortunately, this problem is solved by a minor adjustment to the definition of `MaybeIf` by η-contracting the `t` type parameter.

```hs
-- before: arity of MaybeIf = 2
type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b t where
  MaybeIf True  t = Maybe t
  MaybeIf False t = Identity t

-- after: arity of MaybeIf = 1
type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b where
  MaybeIf True  = Maybe
  MaybeIf False = Identity
```

Notice how the kind signature is unchanged, but the `t` parameter is removed from the header and the clauses. With this, `MaybeIf` now has **arity of 2** and the definition of `AuxInfo` is accepted.

However, now we have lost the ability to reduce the type expression `MaybeIf 'False Int` all the way down to `Int`:

    MaybeIf 'False Int ---/->> Int

i.e. now we must use the `Identity` wrapper.

    MaybeIf 'False Int -->> Identity Int

## Synergy with GADTs

The need for closed type families arises most often when working with GADTs.

```hs
-- HList, Heterogeneous list
type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x : xs)

infixr 5 :&

-- ...can be used to represent sequences of values of different types
h1 :: HList [Integer, String, Bool]
h1 = 42 :& "Hello" :& True :& HNil

h2 :: HList [Char, Bool]
h2 = 'x' :& False :& HNil

-- computing the length
hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ :& xs) = 1 + hlength xs

x1 = hlength h1 -- 3
x2 = hlength h2 -- 2
```

However, even for something as trivial as concatenation we need type-level computation:

```hs
happend :: HList xs -> HList ys -> HList ?
```

The type of `happened h1 h2` must include the elements of both lists and that is precisely what the `Append` type family defines:

```hs
happend :: HList xs -> HList ys -> HList (Append xs ys)
```

And that is the typical reason one would reach for closed type families: to implement operations on GADTs.

## Evaluation order

Haskell is a lazy language, and its evaluation strategy enables us to write code such as the following:

```hs
>>> take 10 (iterate (+5) 0) -- [0,5,10,15,20,25,30,35,40,45]
```

Let us now attempt a similar feat at the type level. First, we define type families that correspond to `take` and `iterate (+5)`:

```hs
type IteratePlus5 :: Nat -> [Nat]
type family IteratePlus5 k where
  IteratePlus5 k = k : IteratePlus5 (k+5)

type Take :: Nat -> [a] -> [a]
type family Take n a where
  Take 0 xs = '[]
  Take n (x : xs) = x : Take (n-1) xs
```

We can see that `Take` works as expected:

```hs
>>> :kind! Take 3 [0, 1, 2, 3, 4, 5]
Take 3 [0, 1, 2, 3, 4, 5] :: [Nat]
= '[0, 1, 2]
```

On the other hand, `IteratePlus5` sends the type checker into an infinite loop:

```hs
>>> :kind! Take 10 (IteratePlus5 0)
^CInterrupted.
```

Clearly, the evaluation of type families is not lazy. In fact, it is not eager either - the rules are not defined at all! Even when working with finite data, reasoning about time or space complexity of algorithms implemented as type families is impossible.

[#18965][https://gitlab.haskell.org/ghc/ghc/-/issues/18965] is a GHC issue that offers a solution to this problem. In the meantime, it is a pitfall one must be aware of.

## Open type families

Let's say we want to assign a textual label to some types, possibly for serialization purposes:

```hs
-- | closed top-level type family
type Label :: Type -> Symbol
type family Label t where
  Label Double = "number"
  Label String = "string"
  Label Bool = "boolean"
```

We can **reify the label** at the term level using the `KnownSymbol` class:

```hs
label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

ghci> label @Double "number"
```

But what if the user defines their own type `MyType` in another module? How could they assign a label to it, such that `label @MyType = "mt"`?

With closed type families, this is not possible. That is where open type families enter the picture. To make a type family open, we omit the `where` keyword in its header. Also, instances are no longer indented; instead, they are declared at the top level (and possibly in different modules) and prefixed with the `type instance` keywords.

```hs
-- | open top-level type family
type Label :: Type -> Symbol
type family Label t

type instance Label Double = "number"
type instance Label String = "string"
type instance Label Bool   = "boolean"
```

Now a user can easily define an instance of `Label` for their own type:

```hs
data MyType = MT

type instance Label MyType = "mt"

_ = label @MyType "mt"
```

At this point, one might start wondering why anybody would ever prefer closed type families if open type families seem to be more powerful and extensible. The reason for this is that extensibility comes at a cost:
>The equations of an open type family are not allowed to overlap.
But overlapping equations are often useful!

## Overlapping equations

The clauses of a closed type family are ordered and matched from top to bottom. This allows us to define logical conjunction as follows:

```hs
type And :: Bool -> Bool -> Bool
type family And a b where
  And True True = True
  And _ _       = False
```

If we were to reorder them, the `And _ _` clause would match all inputs. But it comes second, so the `And True True` clause gets a chance to match. This is the key property of closed type families as opposed to open type families:

>The clauses of closed type families may overlap.

An open type family would need to enumerate all possibilities, leading to a combinatorial explosion:

```hs
type And' :: Bool -> Bool -> Bool
type family And' a b

type instance And' True  True  = True
type instance And' True  False = False
type instance And' False True  = False
type instance And' False False = False
```

## Compatible equations

To say that overlapping equations are disallowed in open type families and allowed in closed type families would be an oversimplification. In practice, the rules are a bit more intricate.

>Open type family instances must be **compatible**.

Type family instances are compatible if at least one of the following holds:
1. Their lhs are apart (i.e. not overlapping)
2. Their lhs unify with a substitution, under which the rhs are equal.

The second condition enables GHC to accept more programs. Consider the following example:

```hs
type family F a
type instance F a    = [a]
type instance F Char = String
```

While the lhs clearly overlap (`a` is more general than `Char`), ultimately it makes no difference. If the user needs to reduce `F Char`, both equations will result in `[Char]`. The mathematically inclined readers will recognize this property as **confluence**.

Here's a more interesting example with several type variables:

```hs
type family G a b
type instance G a    Bool = a -> Bool
type instance G Char b    = Char -> b
```

The lhs unify with a substitution `a ⟼ Char`, `b ⟼ Bool`. 
The right-hand sides are equal under that substitution:

```hs
type instance G Char Bool = Char -> Bool
```

It is therefore safe to accept both of them - they are compatible.


**Instance compatibility** also plays a role in *closed type families*. 
Consider `FInteger` and `FString`:

```hs
type family FInteger a where
  FInteger Char = Integer
  FInteger a    = [a]

type family FString a where
  FString Char = String
  FString a    = [a]
```

For an unknown `x`, GHC cannot reduce `FInteger x` to `[x]` because the equations are matched top-to-bottom, and GHC first needs to check whether `x` is `Char`, in which case it would reduce to `Integer`.

On the other hand, the equations in `FString` are compatible. So if we have `FString x`, it doesn't matter whether `x` is `Char` or not, as both equations will reduce to `[x]`.


## Injective type families

Some type families are injective; that is, we can deduce their inputs from their outputs. For example, consider boolean negation:

```hs
type Not :: Bool -> Bool
type family Not x where
  Not 'True  = 'False
  Not 'False = 'True
```

If we know that `Not x` is `'True`, then we can conclude that `x` is `'False`. By default, the compiler does not apply such reasoning:

```hs
s :: forall x. (Not x ~ True, Typeable x) => String
s = show (typeRep @x)

>>> s
• Couldn't match type `Not x0` with `'True`
    arising from a use of 's'
  The type variable 'x0' is ambiguous
```


Even though the compiler could instantiate `x` to `'False` based on the fact that `Not x` is `'True`, it does not. Of course, we could do it manually, and GHC would check that we did it correctly:

```hs
ghci>>> s @False
'False

ghci>>> s @True
  <interactive>:12:1: error:
  • Couldn't match type `'False` with `'True`
      arising from a use of 's'
```

When we instantiate `x` to `False`, the `Not x ~ True` constraint is satisfied. When we attempt to instantiate it to `True`, the constrained is not satisfied and we see a type error.

There is only one valid way to instantiate `x`. Wouldn't it be great if GHC could do it automatically? That's exactly what *injective type families* allow us to do. Change the type family header of `Not` as follows:

```hs
-- before
type family Not x where
  Not 'True  = 'False
  Not 'False = 'True

-- after
type family Not x = r | r -> x where
  Not 'True  = 'False
  Not 'False = 'True
```

First, we give a name, `r`, to the result of `Not x`. Then, using the syntax of functional dependencies, we specify that `r` determines `x`. GHC will make use of this information whenever it needs to instantiate `x`:

```hs
ghci>>> s
'False
```

This feature is enabled by the `TypeFamilyDependencies` extension. As with ordinary functional dependencies, it is only used to guide type inference and cannot be used to produce equalities. So the following is, unfortunately, rejected:

```hs
not_lemma :: Not x :~: True -> x :~: False
not_lemma Refl = Refl
-- Could not deduce: x ~ 'False
-- from the context: 'True ~ Not x
```

That is a known limitation.


## Associated types

From a code organization perspective, sometimes it makes sense to associate an open type family with a class.

Consider the notion of containers and elements:

```hs
type family Elem a

class Container a where
  elements :: a -> [Elem a]

type instance Elem [a] = a

instance Container [a] where
  elements = id

type instance Elem ByteString = Word8

instance Container ByteString where
  elements = ByteString.unpack
```

We would only use `Elem` with types that also have a `Container` instance, so it would be more clear to move it into the class. That is exactly what associated types enable us to do:

```hs
class Container a where
  type Elem a
  elements :: a -> [Elem a]

instance Container [a] where
  type Elem [a] = a
  elements = id

instance Container ByteString where
  type Elem ByteString = Word8
  elements = ByteString.unpack
```

>Associated type families are mostly equivalent to open type families
and which one to prefer is often a matter of style.


One advantage of associated types is that they can have defaults:

```hs
type family Unwrap x where
  Unwrap (f a) = a

class Container a where
  type Elem a
  type Elem x = Unwrap x
  elements :: a -> [Elem a]
```

This way, we can avoid explicit definition of `Elem` in most instances:

```hs
instance Container [a] where
  elements = id

instance Container (Maybe a) where
  elements = maybeToList

instance Container ByteString where
  type Elem ByteString = Word8
  elements = ByteString.unpack
```

Current research indicates that associated types are a more promising abstraction mechanism than top-level open type families. See:

## Constrained Type Families

* ICFP 2017 - Constrained Type Families
https://www.youtube.com/watch?v=AGJY95Otb9U


A ground type has no type families.

>A total type family, when applied to ground types, always equals some ground type.

GHC assumes all type families are total, and this leads us down the wrong path.

```hs
type family F a

x = fst (5, undefined :: F Int)
```

Type family `F` has no instances yet GHC accepts the type `F Int`! By associating type families to classes, we can constrain them.

```hs
class CF a where
  type F a

x = fst (5, undefined :: F Int)
-- ERROR
```

which does give the error now (since there are no instances of `F`).








## Data families

Data families can be thought of as type families, instances of which are always new, dedicated data types. Consider this example:

```hs
data family Vector a

newtype instance Vector ()     = VUnit Int
newtype instance Vector Word8  = VBytes ByteArray
data    instance Vector (a, b) = VPair !(Vector a) !(Vector b)
```

A `Vector` is a sequence of elements, but for the unit type we can simply store the length as `Int`, which is way more efficient than allocating memory for each unit value.

>We can decide between `data` and `newtype` on a per-instance basis.

This example can be rewritten using type families as follows:

```hs
type family VectorF a

data VectorUnit = VUnit Int
data VectorWord8 = VBytes ByteArray
data VectorPair a b = VPair (VectorF a) (VectorF b)

type instance VectorF ()     = VectorUnit
type instance VectorF Word8  = VectorWord8
type instance VectorF (a, b) = VectorPair a b
```

In this translation, there is a data type for every type family instance. However, even boilerplate aside, this is an imperfect translation.

Data families offer us something else:
>The type ctor they introduce is **generative**
so we do not have to worry about its arity!

For example, the following code is valid:

```hs
data Pair1 f x = P1 (f x) (f x)
type VV = Pair1 Vector
```

On the other hand, `Pair1 VectorF` would be rejected, as this is not applied to its argument.

Data families can also be associated with a class:

```hs
class Vectorizable a where
  data Vector a
  vlength :: Vector a -> Int
```

Just as with associated types and open type families, this is mostly a matter of code organization.


## Non-parametric quantification

In terms, `forall` is a **parametric quantifier**, and this fact can be used to reason about functions. For example, consider the type signature of the identity function:

```hs
id :: forall a. a -> a
```

There is just one thing it can do with its argument: return it untouched. It could not, say, return 42 when given an integer:

```hs
id :: forall a. a -> a
id (x :: Int) = 42 -- Rejected! id x = x
```

This is not only important for reasoning about code, but also to guarantee **type erasure**.

However, none of that applies to type families, which have their own interpretation of what `forall` is supposed to mean:

```hs
type F :: forall a. a -> a
type family F a where
  F (a :: Nat) = 42
  F a = a
```

This code is accepted and works without error:

```hs
ghci>>> :kind! F 0
F 0 :: Nat
= 42

ghci>>> :kind! F "Hello"
F "Hello" :: Symbol
= "Hello"
```

On the one hand, this hinders our ability to reason about type families. On the other hand, this basically amounts to **Π-types at the kind level**, so it can be put to good use.


## Non-linear patterns

In term-level functions, a variable cannot be bound more than once:

```hs
dedup (x : x : xs) = dedup (x : xs) -- Rejected!
dedup (y : xs)     = y : dedup xs
dedup []           = []
```

If we want to check that two inputs are equal, we must do so explicitly with the `==` operator:

```hs
dedup (x1 : x2 : xs) | x1 == x2 = dedup (x1 : xs)
dedup (y : xs)       = y : dedup xs
dedup [] = []
```

On the other hand, in type family instances the former style is allowed.

>In a type family instance, a variable can be bound more than once.

```hs
type family Dedup xs where
  Dedup (x : x : xs) = Dedup (x : xs)
  Dedup (y : xs) = y : Dedup xs
  Dedup '[] = '[]
```

This feature is called **non-linear patterns**, but do not confuse it with linear types, which are not related.


## Conclusion

Type families are a powerful and widely used (20% of Hackage packages) feature.

They were introduced in 2005 in the form of [associated type synonyms][21], and remain a subject of active research to this day, with innovations such as [closed type families][22] (2013), [injective type families][23] (2015), and [constrained type families][24] (2017).

While a useful tool, type families must be used with great care due to open issues such as [#8095][25] ("TypeFamilies painfully slow") and [#12088][26] ("Type/data family instances in kind checking").

However, there are ongoing efforts to address these issues. [Serokell's GHC department](https://serokell.io/haskell-developers) is committed to improving Haskell's facilities for type-level programming.


[21]: https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/at-syns.pdf
[22]: https://richarde.dev/papers/2014/axioms/axioms-extended.pdf
[23]: https://richarde.dev/papers/2015/injective/injective-type-families-extended.pdf
[24]: https://richarde.dev/papers/2017/partiality/partiality-extended.pdf
[25]: https://gitlab.haskell.org/ghc/ghc/-/issues/8095
[26]: https://gitlab.haskell.org/ghc/ghc/-/issues/12088
