# Impredicativity

Alejandro Serrano, Jurriaan Hage, Simon Peyton Jones, Dimitrios Vytiniotis
International Conference on Functional Programming (ICFP'20) - August 2020

A Quick Look at Impredicativity (Simon Peyton Jones)
https://www.youtube.com/watch?v=ZuNMo136QqI

https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/


Type inference for parametric polymorphism is wildly successful, but has always suffered from an embarrassing flaw: *polymorphic types are themselves not first class*. I've been trying to fix this flaw for over 15 years, but every time I ended up with a system that was unusably complicated.

But now I think we have it! *Quick Look* is a practical, implemented, and deployable design for *impredicative type inference*. It is simple to explain, and crucially it can be implemented with modest, localised changes that are fully compatible with GHC's myriad other type system extensions.

𝙏𝙔𝙋𝙀 𝘼𝙋𝙋𝙇𝙄𝘾𝘼𝙏𝙄𝙊𝙉 𝘪𝘴 𝘵𝘩𝘦 𝘦𝘹𝘱𝘭𝘪𝘤𝘪𝘵 𝘴𝘶𝘱𝘱𝘭𝘺 𝘰𝘧 𝘢 𝘵𝘺𝘱𝘦 𝘢𝘴 𝘢𝘯 𝘢𝘳𝘨𝘶𝘮𝘦𝘯𝘵 𝘵𝘰 𝘢 𝘤𝘰𝘳𝘳𝘦𝘴𝘱𝘰𝘯𝘥𝘪𝘯𝘨 𝘧𝘰𝘳𝘢𝘭𝘭 𝘵𝘺𝘱𝘦 𝘱𝘢𝘳𝘢𝘮𝘦𝘵𝘦𝘳. 𝘖𝘯𝘭𝘺 𝘧𝘰𝘳𝘢𝘭𝘭 𝘵𝘺𝘱𝘦 𝘱𝘢𝘳𝘢𝘮𝘴 𝘤𝘢𝘯 𝘣𝘦 𝘵𝘺𝘱𝘦-𝘢𝘱𝘱𝘭𝘪𝘦𝘥 (𝘣𝘺 𝘥𝘦𝘧𝘢𝘶𝘭𝘵 𝘢𝘭𝘭 𝘴𝘪𝘨𝘯𝘢𝘵𝘶𝘳𝘦 𝘶𝘴𝘦 𝘧𝘰𝘳𝘢𝘭𝘭 𝘪𝘮𝘱𝘭𝘪𝘤𝘪𝘵𝘭𝘺).


## Parametric polymorphism

Parametric polymorphism backed by **Damas-Milner type inference** was first introduced in ML, and has been enormously influential and widely used.

For example, a polymorphic function such as `reverse`, despite having a single implementation can work on many different (but similar) types.

```hs
reverse :: ∀p. [p] -> [p]

reverse [1,2,3]
reverse "abracadabra"
reverse [True, False, False]
reverse [[True, False], [False, False]]
reverse [(13, 'a'), (16, 'f')]
```

However, despite this success, Damas-Milner type inference has always suffered from an embarrassing shortcoming:

> Damas-Milner type inference cannot instantiate a type variable with a polymorphic type; the system is predicative.

It allows forall types only at the top level. So, the type `∀𝑎.[𝑎] → [𝑎]` is fine (it is the type of the list reverse function), but the type `[∀𝑎.𝑎 → 𝑎]` is not, because a ∀ is not allowed inside a list. So ∀-types are not first class: they can appear in some places but not others. Much of the time that does not matter, but sometimes it matters a lot; and, tantalisingly, it is often "obvious" to the programmer what the desired impredicative instantiation should be. Alas, predicativity makes polymorphism a second-class feature of the type system.

What goes wrong will be now explained, but first we must start with the explicit type application, which needs to be enabled with `TypeApplications` pragma.

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

## Impredicativity

We'll use the term *impredicativity* for the idea of instantiating a polymorphic function with a polymorphic type, including forall types.

Generally, the term impredicativity has two meanings.

1. In Haskell and ML-like languages
- predicative = type params that range over monotypes
- impredicative = type params that range over any type, including polymorphic and forall types
- impredicativity creates problems with type inference

2. Coq, Agda and similar languages
- predicative = stratified universe of types
- impredicative = no such stratification; notably `Type :: Type` (Type has kind Type)
- impredicativity creates logical inconsistency if not done carefully

We'll use it here meaning type params that range over any type, including forall types.

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

We cannot instantiate ($) in the second expression because we'd have to instantiate it with a polytype. Here, the dollar's type param `a` would have to be instantiated with `∀s. ST s Int`.

Another example that is not supported is having data structutre that contains polymorphic functions.

```hs
id :: ∀ a. a -> a
ids :: [∀ a. a -> a]
(:) :: ∀ p. p -> [p] -> [p]

-- cons id ids - no cigar!
(:) id ids

-- we need to instantiate cons with a polymorphic type
(:) (∀a. a -> a) id ids
-- but this requires impredicative instantiation! so, no go.
```

The search for this "holy grail" has been going on for quite some time and the solutions so far balance expressiveness, the amount of type annotations and simplicity.

This impredicativity was so annoying that the type check for ($), as it was used so often, was manually built into GHC as a special case, just so it would type check. Of course, we, the programmers, often know what the type should be, it's just that a general solver has not been found yet.


## Type inference

Tha main problem with *impredicative instantiation*, also referred to as *polymorphic instantiation*, is that the type checking fails. We cannot type check the signature. To see why, we need a short review of how the type inference works.

In this example chunk of code, the inference starts by type checking `xs`, and since it has not yet been visited, it gets the fresh type `α` (it's just a stand in for an unkown type). In fact, the `α` is a unification variable; it will be matched and possibly unified with other such variables, eventually enough info will be gather that it can collapse into some concrete type.

```hs
reverse :: ∀a. [a] -> [a]
and :: [Bool] -> Bool

foo = \xs -> (reverse xs, and xs)
```

So, `xs :: α`. Then the type checker visits `reverse xs` and it instantiates `reverse` with the fresh unification variable `β`.

Note: the reverse fn has the type `reverse :: ∀a. [a] -> [a]` so it has just one type parameter, namely `a`. Therefore instantiating `reverse` means applying the type (one single type) to reverse, as in `reverse @Int`. This is similar to the way type checker instantiates reverse with a unification variable `β`.

Therefore, this occurance of `reverse` has the type `[β] -> [β]`.

```hs
reverse :: ∀a. [a] -> [a]
and :: [Bool] -> Bool

foo = \xs -> (reverse xs, and xs)

xs :: α
reverse :: [β] -> [β]
```
