# Predicativity and parametric polymorphism

Parametric polymorphism backed by Damas-Milner type inference was first introduced in ML, and has been enormously influential and widely used.

For example, a polymorphic function such as `reverse`, despite having a single implementation can work on many different (but similar) types.

```hs
reverse :: ∀p. [p] -> [p]

reverse [1,2,3]
reverse "abracadabra"
reverse [True, False, False]
reverse [[True, False], [False, False]]
reverse [(13, 'a'), (16, 'f')]
```

However, despite this success, the Damas-Milner type inference has always suffered from an embarrassing shortcoming:

    𝙳𝚊𝚖𝚊𝚜-𝙼𝚒𝚕𝚗𝚎𝚛 𝚝𝚢𝚙𝚎 𝚒𝚗𝚏𝚎𝚛𝚎𝚗𝚌𝚎 𝚌𝚊𝚗𝚗𝚘𝚝 𝚒𝚗𝚜𝚝𝚊𝚗𝚝𝚒𝚊𝚝𝚎 𝚊 𝚝𝚢𝚙𝚎 𝚟𝚊𝚛𝚒𝚊𝚋𝚕𝚎 𝚠𝚒𝚝𝚑 𝚊 𝚙𝚘𝚕𝚢𝚖𝚘𝚛𝚙𝚑𝚒𝚌 𝚝𝚢𝚙𝚎; 𝚝𝚑𝚎 𝚜𝚢𝚜𝚝𝚎𝚖 𝚒𝚜 𝚙𝚛𝚎𝚍𝚒𝚌𝚊𝚝𝚒𝚟𝚎.

The type application below shows how the types are instantiated at different calls to reverse. Although they are here instantiated expliticlly, the same thing happens when inferred.

```hs
{-# LANGUAGE TypeApplications #-}

reverse :: ∀p. [p] -> [p]

reverse @Int    [1,2,3]
reverse @Bool   [True, False, False]
reverse @[Bool] [[True, False], [False, False]]
```

The type application using `@` indicates to GHC how we want to instantiate a type parameter, which in the case of `reverse` is `∀p`. It is like an additional argument supplying an argument type for a type parameter, before we proceed passing in the usual argument values.

Only type params with a forall qualifier can have a type applied to. Hand at hear, majority of type params do have forall qualification, but because, by default, GHCi does not show the full signature, the forall part of the signature is not shown.

Print explicit forall quantification in types:
:set -fprint-explicit-foralls

Print explicit kind foralls and kind arguments in types:
:set -fprint-explicit-kinds


> Type application is the explicit supply of a type as an argument to a corresponding forall type parameter. Only forall type params can be type-applied (by default all signature do use forall implicitly).


In the GHCi REPL, you can query for the results of explicitly applying a type to a polymorphic type. This may come handy when you want to make sure that a class method indeed has the expected signature in relation to some instance.

```ghci
> :t reverse
reverse :: forall a. [a] -> [a]

> :t reverse @Int
reverse @Int :: [Int] -> [Int]

:t fmap
fmap :: forall {f :: * -> *} {a} {b}. Functor f => (a -> b) -> f a -> f b
fmap :: forall (f :: * -> *)  a   b.  Functor f => (a -> b) -> f a -> f b

:t fmap @Maybe
fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
```


Since functions are the first-class values, we should be able to instantiate the types with function types as well, e.g. when making a list of functions.

```hs
id  :: ∀a. a -> a
not :: Bool -> Bool
reverse :: ∀p. [p] -> [p]

-- this is ok, but it cannot be shown as functions lack Show Instance
reverse [id, not]
-- we can confirm it is ok by asking for a type check
:t reverse [id, not]
reverse [id, not] :: [Bool -> Bool]

-- this is a list of, still polymorphic, functions
:t reverse [head, last]
reverse [head, last] :: forall {a}. [[a] -> a]

-- but adding lenght fn to the list instantiates the fns as [Int] -> Int
:t reverse [head, last, length]
reverse [head, last, length] :: [[Int] -> Int]

-- because length is not polymorphic (at least not in the return type)
:t length
length :: forall {t :: * -> *} {a}. Foldable t => t a -> Int

:t length @[]
length @[] :: forall {a}. [a] -> Int

:t length @Maybe
length @Maybe :: forall {a}. Maybe a -> Int
```

We see that the type checker has instantiated `id` as `Bool -> Bool` to match the signature of `not` in the expression `reverse [id, not]`. We sould also explicitaly instantiate either the list type, or `id` type (i.e. `id`'s type param), or both:

```hs
id  :: ∀a. a -> a
not :: Bool -> Bool
reverse :: ∀p. [p] -> [p]

:t reverse [id @Bool, not]
reverse [id @Bool, not] :: [Bool -> Bool]

:t reverse @(Bool -> Bool) [id, not]
reverse @(Bool -> Bool) [id, not] :: [Bool -> Bool]

:t reverse @(Bool -> Bool) [id @Bool, not]
reverse @(Bool -> Bool) [id @Bool, not] :: [Bool -> Bool]

:t reverse [id, not]
reverse [id, not] :: [Bool -> Bool]


-- ($) signatures may be typed like (by defualt):
($)   :: (a -> b) -> a -> b
-- which really means:
($)   :: ∀ a b. (a -> b) -> a -> b
-- it can also be typed as:
($)   :: ∀ a. ∀ b. (a -> b) -> a -> b
-- but this amount to the previous signature

-- $ has 2 type params (∀a and ∀b) so both can be type applied:

-- both:
:t dollar @Int @Bool
dollar @Int @Bool :: (Int -> Bool) -> Int -> Bool

-- just the first, don't mention second:
:t dollar @Int
dollar @Int :: forall {b}. (Int -> b) -> Int -> b

-- just the first, but mention second:
:t dollar @Int @_
dollar @Int @_ :: forall {_}. (Int -> _) -> Int -> _

-- just the second (mention first, of course)
:t dollar @_ @Bool
dollar @_ @Bool :: forall {_}. (_ -> Bool) -> _ -> Bool
```


## Forall types are not first-class


*Forall types*, or just *"foralls"*, are universally quantified types; they have at least one ∀-type parameter.

In a traditional Damas-Milner type system (like in ML), the foralls (forall types, i.e. universally quantified types) can only appear at the outermost level of a let bound function.

```hs
f1 :: ∀p. [p] -> [p]                -- ✔
f2 :: (∀p. [p] -> [p]) -> Int       -- ✘
f3 :: Int -> (∀p. [p] -> [p])       -- ✘
f4 :: [∀a. a -> a] -> Int           -- ✘

reverse :: ∀p. [p] -> [p]

-- This is ok:
:t reverse @Bool
reverse @Bool :: [Bool] -> [Bool]

-- This is ok:
:t reverse @(a -> a)
-- using e.g.
:t reverse @(Bool -> Bool)
reverse @(Bool -> Bool) :: [Bool -> Bool] -> [Bool -> Bool]

-- This is not supported:
:t reverse @(forall a. a -> a)
  Illegal polymorphic type: forall a. a -> a
  GHC doesn't yet support impredicative polymorphism
```

You cannot instantiate, e.g. `reverse` function, with a forall type; it must be instantiated with a concrete type like `@Bool`, `@(a -> a)`, not with a type like `∀a. a -> a`. GHCi even says that it "doesn't yet support impredicative polymorphism".



Again, Damas-Milner type inference allows foralls only at the outermost level of signatures. In Haskell, we can go beyond that with *higher-rank types* that enable us to also type foralls at the left or right of a function arrow.

```hs
f1 :: ∀p. [p] -> [p]                -- ✔ (DM type system)
f2 :: (∀p. [p] -> [p]) -> Int       -- ✔ +HRT allows this one
f3 :: Int -> (∀p. [p] -> [p])       -- ✔ +HRT allows this one
f4 :: [∀a. a -> a] -> Int           -- ✘
```

Higher-rank types enable things like encapsulated state, "scrap you boilerplate" generics and lenses.

```hs
-- encapsulated state
runST :: (∀s. ST s a) -> a

-- "scrap you boilerplate" generics
type GenericT = ∀a. Data a => a -> a

-- lenses
type Lens s a = ∀f. Functor f => (a -> f a) -> s -> f s
composeL :: ∀s1 s2 a. Lens s1 s2 -> Lens s2 a -> Lens s1 a
```

However, the forall instantiation is still not possible, especially when combined with HRT.

```hs
($)   :: ∀ a b. (a -> b) -> a -> b
runST :: ∀ a. (∀ s. ST s a) -> a
st    :: ∀ s. ST s Int

runST st        -- ✔
runST $ st      -- ✘

($) @(∀ s. ST s Int) @Int (runST @Int) st
```
