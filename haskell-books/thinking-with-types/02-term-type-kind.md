# Terms, Types and Kinds

## The Kind System

The fundamental building blocks are terms and types. Terms are the values you manipulate, the things that exist at runtime. Types are little more than sanity check: proof to the compiler (and ourselves) that a program makes sense.

Analogously, the fundamental building blocks for type-level programming are types and kinds. The types are manipulated and the kinds are the proofs.

The word "type" can be used to describe anything that exists at the type level, which is to say, anything that is neither a term nor a kind.

### Star kind

However, we can also refer to `Type` or `*`, which is the kind of types that
have inhabitants. `Int` and `Maybe Bool` have the kind `*`; it classifies the sorts of things that exist at runtime. Some things which aren't of kind `*` are `Maybe` and `Show`. Sometimes we call the sorts of things which have kind `*` *value types*.

The `*` as kind is depracated in favor of `Type`, but we still use it here notationally to avoid ambiguites.


### Arrow kinds

**Higher-kinded types** (HKTs) are types with type variables.

Fully saturated HKTs always have kind `*`, which means that their (unsaturated) type ctors do not.

```hs
Maybe :: * -> *
Maybe Bool :: *

Either :: * -> * -> *
Either Int :: * -> *
Either Int Float :: *

(->) :: * -> * -> *
(->) ([] Int) :: * -> *
(->) ([] Int) ([] String) :: *

MaybeT :: (* -> *) -> * -> *
-- Defined in Control.Monad.Trans.Maybe
type role MaybeT representational nominal
type MaybeT :: (* -> *) -> * -> *
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

`MaybeT` also takes two type parameters, but unlike `Either`, one of them must be a Monad. Because monads are always of kind `* -> *`, the kind of `MaybeT` must be `(* -> *) -> * -> *`.


### Constraint kinds

Kinds apply to everything at the type-level, not just the things we traditionally think of as types.

For example, the type of `show :: Show a => a -> String`.

The `Show` class constraint exists as part of the type signature, even though it's clearly not a `*` (`Type`); but `Show` also has a kind, viz. `Constraint`.

> More generally, `Constraint` is the kind of any fully saturated type class.


```hs
Show :: * -> Constraint
Show Int :: Constraint

Functor :: (* -> *) -> Constraint
Functor [] :: Constraint

Applicative :: (* -> *) -> Constraint
Monad :: (* -> *) -> Constraint

import Control.Monad.Trans
MonadTrans :: ((* -> *) -> * -> *) -> Constraint
```


Without language extensions, this is the extent of the expressiveness of Haskell's kind system. We have no notion of polymorphism, nor are we able to
define our own kinds, or write custom type functions.


### Data Kinds

By enabling the *DataKinds* extension, we get the ability to create kinds other than `Type`, `Constraint`, and their arrow derivatives. In particular, it lifts data ctors into type ctors and types into kinds.

> `DataKinds` ext lifts types into kinds and data ctors into types (ctors).

When enabled, every normal type definition automatically also produces the corresponding kind definition (kind syntax just for illustration):

```hs
{-# LANGUAGE DataKinds #-}

data Bool =  True |  False
kind Bool = 'True | 'False
```

In other words, via *-XDataKinds* we have now declared the types `'True` and `'False`, both of kind `Bool`. We call 'True and 'False promoted data ctors.

> Data ctors that are lifted to types are called **promoted data ctors**.

The tick are used to distinguish promoted data ctors from types because they now live in the samenamespace, both being types.

When the name of the type is the same as the name of the data ctor (as is often the case with types that have a single data ctor), and this extension is enabled, then ticks are used to distinguish between the two.


The importance of data kinds is that type-level programming in Haskell without them is equivalent to programming in a dynamically typed language. By default, having every kind you manipulate be `Type` is a lot like having all of your values be of the same (general) type.

While types don't let you do anything more you couldn't do without them, they make it easier to reason about programs. Data kinds are exactly the same, just for the type level - as we write type-level programs, the kind signatures are used to restrict the types we are dealing with.


Promoted data type ctors can be used as **phantom parameters**.

This trick is often used with authentication to separate users from admins. By declaring new type `UserType`, whose only purpose is to give us access to its promoted data ctors, we can then change the `User` type so that each user potentially has an admin token. Than we can make sensitive operations require a copy of the admin token.

```hs
-- before: User type
data User = User


-- type whose data ctors are to be promoted
data UserType = Regular | Admin

-- after: User type enhanced
data User = User { token :: Maybe (Proxy 'Admin) }

-- sensitive ops require a copy of the admin token
sensitiveOp :: Proxy 'Admin -> IO ()
```

This change will cause a type error whenever `sensitiveOp` is called without an admin token, making it impossible to be called accidentally. More refined techniques can be used to prevent people from simply conjuring up an admin token, requiring `sensitiveOp` to be called on behalf of an actual admin.


## Promotion of Built-In Types

import GHC.TypeLits

With `DataKinds` enabled, almost all types automatically promote to kinds, including the built-in ones (except GADTs and osme other tricky types), but they also require importing `GHC.TypeLits` because it defines the kinds themselves, as well as all of the useful type families for manipulating them. 

### Symbols

The promoted version of `String` is `Symbol` kind. Symbols are not list of characters - they are no longer inductive types. Symbol type literals can be written by just using a string literal in a place where a type is expected.

* 𝚂𝚢𝚖𝚋𝚘𝚕 cannot be deconstructed
* 𝚂𝚢𝚖𝚋𝚘𝚕 can be appended with `AppendSymbol` primitive from `GHC.TypeLits`
* 𝚂𝚢𝚖𝚋𝚘𝚕 can be compared with `CmpSymbol` primitive from `GHC.TypeLits`


```hs
{-# LANGUAGE DataKinds #-}
import GHC.TypeLits

-- kinds
"hi" :: GHC.Types.Symbol
AppendSymbol :: Symbol -> Symbol -> Symbol
CmpSymbol    :: Symbol -> Symbol -> Ordering

:kind! AppendSymbol "Symbol are cat'ed " "with AppendSymbol"
... :: Symbol = "Symbol are cat'ed with AppendSymbol"

:kind! CmpSymbol "abc" "def"
CmpSymbol "abc" "def" :: Ordering = 'LT
```

### Natural Numbers and arithmetic

* 𝙽𝚊𝚝 is the kind of natural numbers
* `TypeOperators` pragma enables aritmetic ops on Nat kind
* There is some problem with multiplication [unresolved]

GHC.TypeLits defines primitives for performing arithmetic on Nat kinds with familiar symbolic identifiers but they require enabling *TypeOperators*.

In GHCi use `:kind!` command to calculate the type operation.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

:kind! (5 + 17)
(5 + 17) :: Nat = 23

:kind! (Div 128 8) ^ 2
(Div 128 8) ^ 2 :: Nat = 256

:k (+) :: Nat -> Nat -> Nat
:k (-) :: Nat -> Nat -> Nat
:k (^) :: Nat -> Nat -> Nat
:k Mod :: Nat -> Nat -> Nat
:k Div :: Nat -> Nat -> Nat
```

### List

Lists data ctors are promoted as if defined as: `data [a] = [] | a : [a]`.

- `'[]`  has kind `[a]`
- `'(:)` has kind `a -> [a]`
  - as infix: `x ': xs`
  - as prefix: `'(:) x xs`

Note that the query for kind of cons operator (:) is not (':) but `'(:)`.

Because list data ctors have symbolic names, they also require `TypeOperators`.

When constructing promoted list `'[ 'True ]` is ok, but `'['True]` is a parse error, so add some spacing.


### Tuples

Tuples also promote in a straightforward way, via the `'(,)` constructor.

Tuples are promoted with a leading tick (the spacing gotcha applies).


## Type-Level Functions

- *Closed type families* as almost like type-level functions.
- Each of the primitive ops (`CmpSymbol`, `Div`, `(+)`, etc.) are closed type families.


```hs
-- term level function
or :: Bool -> Bool -> Bool
or True  _ = True
or False y = y

-- type level function
type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True  y = 'True
    Or 'False y = y

-- term level map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : as) = f a : map f as

-- type level map
type family Map (x :: a -> b) (i :: [a]) :: [b] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs
```

Type families must be saturated - there is no currying available. Because we cannot partially apply closed type families, `Map` is not particularly useful.

Pay attention to the signatures:

we write it as: `Or (x :: Bool) (y :: Bool) :: Bool`    
rather than as: `Or x y :: Bool -> Bool -> Bool`

The kinds of type families are tricky beasts; the kind you write after the `::` is the **kind of the type returned by the type family**, not the kind of the type family itself.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

type family Foo (x :: Bool) (y :: Bool) :: Bool
-- :k Foo :: Bool -> Bool -> Bool

type family Bar x y :: Bool -> Bool -> Bool
-- :k Bar :: * -> * -> Bool -> Bool -> Bool
```
