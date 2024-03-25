# Kind system - a primer
https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/
Mark Karpov, 2018

Contents
- Types and Kinds
- Data constructors and type constructors
- Type signatures and kind signatures
- HOFs and HKTs
- Other kinds
- Unboxed/unlifted types
- Constraints
- Datatype promotion
- GHC.TypeLits
- Kind polymorphism
- Levity polymorphism

(this article was written with GHC 8.4.3 in mind)

## Types and Kinds

- Just like terms are classified into types, types are classified into kinds
- A kind is a property of a types, or, more precisely, of type ctors
- `Type` or `*` is a kind of saturated type ctors.
- base types are all nullary (and therefore saturated) type ctors: Int, Char, Float, Double. Also the types like `()`, and `Integer`, and `String` as well.
- use `:t` to check the type of a term, but `:k` to check the kind of a type.
- All inhabited types, i.e. types that have at least 1 value, are of kind `*`.
- Besides base types, these are also types with saturated type ctors.

So `Int -> String`, `[Int]`, `Maybe Int`, `Either Int Int` are all of kind `*` because there is at least one term for each of these types. A notable exception is the uninhabited type `Void` which cannot be constructed. There are no values of this type per se, it is only inhabited by bottom.

Not all types are inhabited though; for example, `Maybe`, `Either`, and even `Either Int` are uninhabited. There is no term of type `Maybe`, not even the infinite loop. They are unsaturated type ctors, so they cannot form an inhabited type until fully applied. This suggests they have a different kind than saturated type ctors.

In fact, they have several different kinds depending on the number of type variables they take, as well as on the kind of each of those type variables.

So, `Maybe` and `[]` have the kind `* -> *`, since they are both unary type ctors, they need one type to be saturated, e.g. `Maybe Int :: *`.

The kind of `Either :: * -> * -> *` since it needs two types, which is also the kind of the function type ctor, `(-> :: * -> * -> *)`, which needs an input and an output type to be saturated and thereby fully defined.

Type constructors can also be curried and partially applied. Thus, only unapplied `Either` type ctor has kind `* -> * -> *`, but `Either Int` is partially applied (to `Int` type) so it has the kind `* -> *`; and `Either Int Char`, which is fully applied, has the kind `*`.

Knowing that the kind of `Maybe :: * -> *`, means that `Maybe a` necessarily has the kind `*`, which further implies that whatever type `Maybe` is applied to, that type must have the kind `*`, i.e. `a` here must have kind `*`.

```hs
Maybe :: * -> *
Maybe a :: *
Maybe (a :: *) :: * -- explicitly kinding the `a` type var

-- instantiating `a`:
Maybe Int :: *          -- ok
Maybe [Char] :: *       -- ok
Maybe (Maybe Char) :: * -- ok

Maybe [] :: *           -- ERROR since ([] :: * -> *)
```

## Type signatures and kind signatures

GHC tries to correctly infer the kind of type variable in a signature, but lacking the sufficient info, it will default a type var's kind to `*`.

Using `KindSignatures` extension you can manually specify the correct kind.

```hs
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

data List (a :: *) = Cons a (List a) | Nil

class Functor (f :: * -> *) where
  fmap :: forall (a :: *) (b :: *). (a -> b) -> (f a -> f b)


{-# LANGUAGE StandaloneKindSignatures #-}

type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: forall a b. (a -> b) -> f a -> f b
```

The `ExplicitForAll` extension allows us to define each type var explicitly, using the `forall` quantifier.

The `StandaloneKindSignatures` allows us to separate kind sig from the definition, just like we can state a (function's) sig separately from its definition.


## Higher-kinded types

Just like we can have higher-order functions (HOFs), functions that take other functions as arguments, we can also have higher-kinded types (HKTs), types constructors that take other type constructors as arguments.

> Higher-kinded types (HKT) are types ctors that take other type ctors as args.

```hs
data NonEmpty f a = MkNonEmpty { head :: a, tail :: f a }
-- ghci feedback:
type role NonEmpty representational nominal
type NonEmpty :: (* -> *) -> * -> *
data NonEmpty f a = ...
head :: forall (f :: * -> *) a. NonEmpty f a -> a
tail :: forall (f :: * -> *) a. NonEmpty f a -> f a
```

`NonEmpty` is a type ctor that takes another type ctor of kind `* -> *`, plus a type of kind `*`.

When applied to `[]` and `Bool`, we obtain the type `NonEmpty [] Bool` (but not as `NonEmpty [Bool]`) i.e. a list of boolean values that is guaranteed to have at least one value.

```hs
:t MkNonEmpty True [False, True]
MkNonEmpty True [False, True] :: NonEmpty [] Bool
```

We can apply this type constructor to any two types, so long as their kinds match the expected kinds, e.g. `NonEmpty [] Int`, `NonEmpty Tree String` or `NonEmpty Vector Char`.


> **Type application is left-associative**.

`NonEmpty [] Bool` is not `NonEmpty ([] Bool)` which is `NonEmpty [Bool]`.

```hs
:t tail @[]
tail @[] :: forall {a}. NonEmpty [] a -> [a]

:t tail @[] @Bool
tail @[] @Bool :: NonEmpty [] Bool -> [Bool]

:t tail @_ @Bool
tail @_ @Bool :: forall {w :: * -> *}. NonEmpty w Bool -> w Bool

:t tail @_ @[Bool]
tail @_ @[Bool] :: forall {w :: * -> *}. NonEmpty w [Bool] -> w [Bool]
```

## Unboxed and unlifted types

The `*` is the kind of all inhabited boxed (or lifted) types, but there are also unboxed (or unlifted) types. These are defined in `GHC.Prim` module in the `ghc-prim` package.

By convention, all unlifted types end with a `#`, called the *magic hash*, enabled by the `MagicHash` extension. Examples include `Char#` and `Int#`. You can even have unboxed tuples `(# a, b #)` and unboxed sums `(# a | b #)`, enabled by `UnboxedTuples` and`UnboxedSums` 

*Each unlifted type has a kind that describes its runtime representation*. This may be a pointer to a heap value, or a signed/unsigned word-sized value. The compiler uses a type's kind to decide which machine code it needs to produce - this is called *kind-directed compilation*.


term            | type              | kind
----------------|-------------------|------------------------------------
3#              | GHC.Prim.Int#     | TYPE 'GHC.Types.IntRep
3#              | Int#              | TYPE IntRep
'a'#            | Char#             | TYPE WordRep
4##             | Word#             | TYPE WordRep
(# 3#, 'a'# #)  | (# Int#, Char# #) | TYPE (TupleRep '[IntRep, WordRep])

```hs
:k (# Word# | Int# #)

(# Word# | Int# #) :: TYPE
  ('GHC.Types.SumRep
    ((':)
      @GHC.Types.RuntimeRep
        'GHC.Types.WordRep
          ((':)
            @GHC.Types.RuntimeRep
              'GHC.Types.IntRep
                ('[] @GHC.Types.RuntimeRep))))
```

## Constraints

The `Constraint` kind covers everything that can appear to the left of an `=>` arrow, including class constraint.

```hs
:k Show
Show :: * -> Constraint

:k Show Int
Show Int :: Constraint

:k Functor
Functor :: (* -> *) -> Constraint

:k Functor IO
Functor IO :: Constraint
```

With the `ConstraintKinds` extension, we can use constraints as first-class citizens.

`Set` famously does not have an instance for the Functor class because `fmap` must work `forall a b`, but `Set.map` only works for `Ord a` constrained types: `Data.Set.map :: Ord b => (a -> b) -> Set a -> Set b`.

### More generic Functor class

With `ConstraintKinds`, we can write a more generic `Functor` type class that abstracts over any constraint that may or may not exist:

```hs
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Kind (Constraint)
import qualified Data.Set as Set
import qualified Data.List as List

class GFunctor (c :: * -> Constraint) (f :: * -> *) | f -> c where
  gfmap :: c b => (a -> b) -> (f a -> f b)

instance GFunctor Ord Set.Set where
  gfmap = Set.map

instance GFunctor EmptyConstraint [] where
  gfmap = List.map


-- EmptyConstraint is a class constraint trivially satisfied by all types:
{-# LANGUAGE FlexibleInstances #-}

class EmptyConstraint a

instance EmptyConstraint a
```

## Kind polymorphism

Parametric polymorphism is ubiquitous in Haskell, it helps us abstract over types, but with the `PolyKinds` extension, we can go even higher and abstract over kinds as well.

In the previous example, `class EmptyConstraint a`, the type variable `a` was inferred to have kind `*`, as in `class EmptyConstraint (a :: *)`. This means `a` can only ever be instantiated at saturated types. The same with:

```hs
data Proxy a = MkProxy
```

Here, `Proxy` contains a type variable `a`. Since `Proxy` is a unary type ctor with one nullary data ctor, `a` is inferred to have kind `*`.

We use `Proxy` to tag a type variable. This is useful for passing around types when we have no values at hand:

```hs
intRepresentative = MkProxy :: Proxy Int
stringRepresentative = MkProxy :: Proxy String
```

The problem is the inferred kind of `a :: *`, meaning `Proxy` doesn't work for all types, only for lifted and saturated types. So, `maybeRepresentative = MkProxy :: Proxy Maybe` makes no sense.

Instead of creating a bunch of Proxies for each conceivable kind, like

```hs
data Proxy a = MkProxy
data Proxy1 (a :: * -> *) = MkProxy1
data Proxy2 (a :: * -> * -> *) = MkProxy2
data HKProxy1 (a :: (* -> *) -> * -> *) = MkHKProxy1

stringRepresentative   = MkProxy    :: Proxy String
maybeRepresentative    = MkProxy1   :: Proxy1 Maybe
eitherRepresentative   = MkProxy2   :: Proxy2 Either
nonEmptyRepresentative = MkHKProxy1 :: HKProxy1 NonEmpty
```

which is obviously unsustainable, instead we use the `PolyKinds` extension to enable *polykinded type variables*. Then we can create `Proxy` that works forall types `a` and forall kinds `k`.

```hs
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

data Proxy (a :: k) = MkProxy

-- Now `a` can be anything at all
maybeRepresentative    = MkProxy :: Proxy Maybe
nonEmptyRepresentative = MkProxy :: Proxy NonEmpty
functionRepresentative = MkProxy :: Proxy (->)
helloRepresentative    = MkProxy :: Proxy "hello"
showRepresentative     = MkProxy :: Proxy Show
functorRepresentative  = MkProxy :: Proxy Functor
openRepresentative     = MkProxy :: Proxy 'Open
```

In fact, when `PolyKinds` is enabled, GHC would infer a polykinded kind for the `a` type var, `a :: k`. When `PolyKinds` disabled, GHC infers `*` for type vars for which it has insufficient info to infer something better.


Kind polymorphism is also used to combine together instances of one (and the same) type that has different kinds.

```hs
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

data (:>) (a :: k) (b :: *)
infixr 4 :>

-- Desc an endpoint that responds to POST /books.
-- We use :> to combine one same type but of diff kinds:
--    "books"              :: Symbol    (repr e.g. current category)
--    2                    :: Nat       (repr e.g. current page)
--    ReqBody '[JSON] Book :: *
--    Post    '[JSON] ()   :: *
-- into one type

type BooksAPI = "books" :> 2 :> ReqBody '[JSON] Book :> Post '[JSON] ()
```

## Levity polymorphism

In some cases, it may be useful to abstract over lifted and unlifted types. 

For example, consider the `error` function, which takes an error message and throws an exception:

```hs
{-# LANGUAGE ExplicitForAll #-}

error :: forall a. String -> a

-- error is polymorphic in its return type
-- so it can be used (almost) anywhere
increment :: Int -> Int
increment x = error "oops"

-- but doesn't work with unlifted types

-- Couldn't match a lifted type with an unlifted type:
incrementUnlifted :: Int# -> Int#
incrementUnlifted x = error "oops"

-- again no good:

-- Expected a type, but 'a' has kind 'k'
{-# LANGUAGE PolyKinds #-}
error :: forall k (a :: k). String -> a
```

However, `error` doesn't work with unlifted types because `a` is by default of kind `*`. We cannot use `error` where an unlifted type is expected!

Enabling `PolyKinds` and introducing a kind variable `k` won't work either.


The reason this doesn't compile is that it doesn't make sense for `a :: k` to be anything other than an inhabited type. Say `k` was `Symbol` and `a` was `"hello"`. If *`"hello"` type* is uninhabited, what value could such a function possibly return?

To make the `error` function work for all inhabited types, both lifted and unlifted, we need **levity polymorphism**.

The trick resides in the kind `TYPE r`. This kind is parameterised over `r :: RuntimeRep` which describes a type's runtime representation, which is one of

```hs
data RuntimeRep
  =    FloatRep           -- 32-bit floating point number
  |   DoubleRep           -- 64-bit floating point number
  |      IntRep        -- signed   word-sized value
  |     WordRep        -- unsigned word-sized value
  |   LiftedRep           -- lifted   represented by a pointer
  | UnliftedRep           -- unlifted represented by a pointer
  |    Int64Rep        -- signed   64-bit value (on 32-bit only)
  |   Word64Rep        -- unsigned 64-bit value (on 32-bit only)
  |     AddrRep           -- a pointer but NOT to a Haskell value
  |      VecRep VecCount VecElem   -- SIMD vector type
  |    TupleRep [RuntimeRep]       -- unboxed tuple of the given reps
  |      SumRep [RuntimeRep]       -- unboxed sum   of the given reps
```


- `TYPE 'IntRep`    is the kind of unlifted integers
- `TYPE 'FloatRep`  is the kind of unlifted floats
- ...
- `TYPE 'LiftedRep` is the kind for all lifted types. `TYPE 'LiftedRep` is just a synonym for `*`.


It turns out we can use `TYPE r` not only to abstract over all unlifted types, but also over lifted ones.

```hs
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

import GHC.Exts (TYPE, RuntimeRep)

error :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
```

And now we can use `error` in both `increment` and `incrementUnlifted`.

Levity polymorphism has its restrictions - there are some places where it can't be used. To learn where and why, watch Richard Eisenberg's amazing talk:
https://www.youtube.com/watch?v=lSJwXZ7vWBw


## Wrap-up

Why have a kind system? In languages where types cannot be classified at all, like Java or C#, all type variables `<T>` are of kind `*`. You can't have a type variable of kind `* -> *`, like in `Functor<List>`.

Without being able to talk about kinds, we can't define abstractions like functors and monads, or even the `NonEmpty f a` data type from earlier.

Furthermore, with levity polymorphism, we can write abstractions that work with both boxed and unboxed types, like this generic version of the `Num a` class (as opposed to Java where generic type vars can only ever be boxed types):

```hs
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeInType #-}

import GHC.Prim
import GHC.Exts (TYPE)

class LevNum (a :: TYPE r) where
  levAdd :: a -> a -> a

instance LevNum Int  where levAdd = (+)
instance LevNum Int# where levAdd = (+#)
```

Having a kind system also opens the door to a world of type-level programming.

Another question you might be asking is, if terms are classified into types, and types into kinds, are kinds classified into something else? Well, in GHC 7 and earlier, kinds were classified into *sorts*. All kinds had the unique sort `BOX`, which was internal to GHC and invisible to us, the developers. But GHC 8 went a different way.

As you might have noticed, in this article I tried to expose the similarities between types and kinds: both can be higher-order, polymorphic, inferred, curried, etc.

This is not a coincidence! With the advent of `TypeInType`, an extension introduced in GHC 8, types and kinds (and sorts) became one and the same!

Types can now be classified by other types:    
`3` is of type `Int`, `Int` is of type `*`, and `*` is of type `*`.

This unification is meant to pave the way for full dependent types in Haskell.
