---
title        : Rank-N Types
slug         : rank-n-types.md
path         : debrief.haskell/terms/
keywords     : 
---

# Quantifiers and quantified type parameters

- quantifier
- type parameter/variable
- forall keyword
- forall as both universal and existential qualifier
- implicit universal quantification
- quantified type parameter
- universal type
- universal quantifier
- universal quantification
- existential type
- existential quantifier
- existential quantification
- scoped type variable
- quantified type signature
- predicatively
- impredicativity


## Quantifier

In natural languages, a sentence about a certain thing with a particular property, may be made into a sentence about a number of things with a particular property, using a quantifier. A quantifier is a determiner, such as *all, each, every, many, some, few, a lot, no, none* (never a specific number) that indicates quantity.

For example, the sentence *"The type parameter `a` is implicitly universally quantified"* is universally quantified resultng in *"All type params are implicitly universally quantified"*.

In symbols, if `a` is a (type) variable and `P` is a predicate meaning "being implicitly universally quantified", then the previous example corresponds to `∀a ∈ A. P(a)`. If we switch "all" with "some", it becomes `∃a ∈ A. P(a)`. The `A` stands for some set (in PLs, for some type) that the `a` variable ranges over. Negating the former, we get `∀a ∈ A. ¬P(a)`, and negation of the latter is denoted by `∃a ∈ A. ¬P(a)`.

∀a. P(a) ≡ ¬(∃a.¬P(a))
∃a. P(a) ≡ ¬(∀a.¬P(a))
∀a.¬P(a)
∃a.¬P(a)

`¬(∃a.¬P(a))` may be read as: it is not the case that there exists an element `a` such that `P` of `a` does not hold (such that `P` is not true of `a`).



## The forall keyword



## Universal quantification in types

https://stackoverflow.com/questions/33576722/understanding-the-difference-between-two-haskell-signatures-one-using-forall

There are 2 important cases where the `forall` makes a difference.

1. *Fundamentally*: the location of a `forall` can change the meaning of a type. When placed on the left of an arrow, it makes an argument "more polymorphic" (explained later) than otherwise.

2. *Syntactically*: when ScopedTypeVariables pragma is enabled **and** a TP is introduced with the `forall` in the signature, then the TP becomes available for referencing throught that function (or data declaration). Although a TP may be scoped in any occasion when types are present, this is most often encountered when a nested function needs to refer to a TP that was declared in the enclosing function's signature.

Without this mechanism, a TP reference would instead mean creation of a fresh, universally quantified, type variable, regardless of the fact that it is named the same as a TP from the enclosing function's signature (it would be a programmer's responsibility to somehow ensure that the two TPs are referring to the same type).


## Forall types

```hs
id  :: ∀a. a -> a
reverse :: ∀p. [p] -> [p]

($)   :: ∀ a b. (a -> b) -> a -> b
length :: forall {t :: * -> *} {a}. Foldable t => t a -> Int


reverse @Int    [1,2,3]
reverse @Bool   [True, False, False]
reverse @[Bool] [[True, False], [False, False]]

-- This is not supported:
:t reverse @(forall a. a -> a)
  Illegal polymorphic type: forall a. a -> a
  GHC doesn't yet support impredicative polymorphism
```

Type application is the explicit supply of a type as an argument to a corresponding forall type parameter. Only forall type params can be type-applied (by default all signature do use forall implicitly).


## Forall types are not first-class

"Forall" types ("foralls") are universally quantified types that have at least one ∀-type parameter.

In a traditional Damas-Milner type system (like in ML), foralls can only appear at the outermost level of a let bound function.

```hs
f1 :: ∀p. [p] -> [p]                -- ✔
f2 :: (∀p. [p] -> [p]) -> Int       -- ✘
f3 :: Int -> (∀p. [p] -> [p])       -- ✘
f4 :: [∀a. a -> a] -> Int           -- ✘
```

Damas-Milner type inference allows foralls only at the outermost level of signatures. In Haskell, we can go beyond that with *higher-rank types* that enable us to also type foralls at the left or right of a function arrow.

```hs
f1 :: ∀p. [p] -> [p]                -- ✔ (DM type system)
f2 :: (∀p. [p] -> [p]) -> Int       -- ✔ +HRT allows this one
f3 :: Int -> (∀p. [p] -> [p])       -- ✔ +HRT allows this one
f4 :: [∀a. a -> a] -> Int           -- ✘
```



## Scoped type variables

With *ScopedTypeVariables* type variables with forall have scoped so they can be referenced from within the function definition, e.g. in a subdefinition of a helper function with a type signature, top-lwvwl type param can be reused.

> Only forall type params are scoped when *ScopedTypeVariables* is enabled.



## Differences when using forall

https://stackoverflow.com/questions/3961851/what-is-the-difference-between-forall-a-a-and-forall-a-a


```hs
-- the difference?

id1 :: ∀ a. a -> a
id2 :: a -> a

t :: forall a. [a]
u :: [forall a. a]
```

The type `forall a. [a]` means that for any single type, it's a list containing that type. This is also what the plain `[a]` means; it is the type of `[]`, 
i.e. the empty list data constructor.

The type `[forall a. a]` is a completely different thing. It means that you have a list of values, each of which is type-polymorphic. That is, each element is a value of any possible type, not necessarily the same type as the other elements of the list have. No possible value can have the type `forall a. a`, so this must also be an empty list. And `⊥`.

```hs
xs :: [forall a. a]
xs = [undefined] -- same error
xs = []          -- with either one

<interactive>:156:7: error:
Illegal polymorphic type: forall a. a
GHC doesn't yet support impredicative polymorphism
```

The difference is then that while the former can be used as a list of any type (by definition, basically), the latter can't be used as a list of any concrete type at all, since there's no way to pin it down to any single type.

An *existential type* is the one that, within some scope, will be instantiated to some *unknown concrete type*. It could be anything, so it is represented by something like the `forall a. a`. To make sure that anything with an existential type is only used within the scope where the actual type will be available, the compiler prevents existential types from "escaping".

It may be helpful to think about the *forall quantifier* as being like a lambda abstraction - it introduces a new type parameter and binds that identifier within some scope. Outside that scope, the identifier has no meaning, which is why `forall a. a` is pretty useless.


- - -

https://stackoverflow.com/questions/3961851/what-is-the-difference-between-forall-a-a-and-forall-a-a?answertab=oldest#tab-top

When used for types `forall` means **intersection**. Thus, `forall a. a` is the intersection of all types, i.e. something like `Int ∩ String ∩ ...` which seems to give the empty set, but each type has an extra element called bottom or ⊥ or `undefined` in Haskell. From this we get that `forall a. a = {⊥}`.

Actually, we can define a type which contains only bottom:

```hs
data Zero
```

After this setup lets look at our types starting with `[forall a. a]`. What it defines is a list of bottoms or [Zero] which has elements [], [undefined], [undefined, undefined],... Lets check it in ghci:

```hs
> l = [undefined, undefined] :: [Zero]
> :t l
l :: [Zero]
```


Similarly, `forall a. [a]` is the intersection of all list types and because the `∩[a] ≡ [∩a]` this is again [Zero].

To do a final check lets define 2 such types and check:

```hs
type L = forall a. [a]
type U = [forall a. a]

> l2 = [undefined, undefined]::L
> l3 = [undefined, undefined]::U

> :t l2
l2 :: [a]

> :t l3
l3 :: U
```

Notice that `l2 :: [a]`, the explanation is that Haskell puts an implicit `forall` in front of all polymorphic types.



## System F

* Polymorphic values are essentially functions on types, but Haskell lacks the proper syntax for type abstraction and type application, leaving them implicit.

* System F has explicit type abstraction and type application. 
For example, the familiar `map` function would be written:

```hs
map :: ∀a. ∀b. (a -> b) -> [a] -> [b]
map  = Λa. Λb. λ(f :: a -> b). λ(xs :: [a]).
    case xs of
        []     -> []
        (y:ys) -> f y : map @a @b f ys
```

map is now a function of 4 arguments: two types (a and b), a fn and a list.
We write a function over types using `Λ` (upper-case lambda), and a function over values using `λ`, as usual. The notation @a is used (as in GHC Core) to denote application of a type arg.

- a term containing `Λ` gives rise to a type containing `∀` (quantifier)
- a term containing `λ` gives rise to a type containing `->` (fn type ctor)

So a value of polymorphic type is like a function from types to values. The caller of a polymorphic function gets to choose the type argument, and the function must comply.

How, then, would we write a term of type `∀a. [a]`?

We know that types containing ∀ come from terms containing Λ:

```hs
term1 :: ∀a. [a]
term1 = Λa. ??
```

Within the body, marked with ??, we must provide a term of type [a]. That is, a term of type `∀a. [a]` means "given any type `a`, I will give you a list of type [a]".

However, we know nothing concrete about a, since it's an argument passed in from the outside. So we can return

```hs
-- an empty list
term1 = Λa. []

-- or an undefined value
term1 = Λa. undefined

-- or a list containing undefined values only
term1 = Λa. [undefined, undefined]
```

but not much else.



What about `[∀a. a]`, then? If ∀ signifies a function on types, then `[∀a. a]` is a list of functions.

```hs
-- We can provide as few as we like:
term2 :: [∀a. a]
term2 = []

-- or as many:
term2 = [f, g, h]
```

But what are our choices for f, g, and h?

```hs
f :: ∀a. a
f = Λa. ?
```

Now we're well and truly stuck. We have to provide a value of type a, but we know nothing whatsoever about the type a. So our only choice is

```hs
f = Λa. undefined
```

So our options for term2 look like

```hs
term2 :: [∀a. a]
term2 = []
term2 = [Λa. undefined]
term2 = [Λa. undefined, Λa. undefined]
-- etc.

-- and let's not forget
term2 = undefined
```


## Existential types

> A value of universal (∀) type is a function *from types to values*.

> A value of existential (∃) type is a *pair of a type and a value*.

Specifically, a value of type `∃x. T` is a pair `(S, v)` where `S` is a type, and where `v :: T`, assuming you bind the type variable `x` to `S` within `T`.


Here's an existential type signature and a few terms with that type:

```hs
term3 :: ∃a. a
term3 = (Int, 3)
term3 = (Char, 'x')
term3 = (∀a. a → Int, Λa. λ(x::a). 4)
```

In other words, we can put any value we like into `∃a. a`, as long as we pair that value with its type.


The consumer (user, client) of a value of type `∀a. a` is in a great position: he can choose any specific type they like, using the type application `@T`, to obtain a term of type `T`.

The producer of a value of type `∀a. a` is in a terrible position: he must be prepared to produce any type asked for, but the only choice is `Λa. undefined`.

The consumer of a value of type `∃a. a` is in a terrible position: the value inside is of some unknown specific type, not a flexible polymorphic value.

The producer of a value of type `∃a. a` is in a great position: he can stick any value they like into the pair.


### Less useless existentials

So, the question is are existentials useless or is there some utility to them.

A less useless existential is *values paired with a binary operation*:

```hs
type Something = ∃a. (a, a → a → a, a → String)

term4_a :: Something
term4_a = (Int,    (1,     (+)  @Int , show @Int))

term4_b :: Something
term4_b = (String, ("foo", (++) @Char, λ(x::String). x))

-- usage:
triple :: Something → String
triple = λ(a, (x :: a, f :: a→a→a, out :: a→String)).
  out (f (f x x) x)

-- result:
triple term4_a  -- "3"
triple term4_b  -- "foofoofoo"
```

We have basically made a module. We've packaged up a type and some operations on that type. The user can apply our operations, but he cannot inspect the concrete value. We cannot pattern-match on `x` within `triple`, since its type (hence set of ctors) is unknown. This is more than a bit like OOP.


### Using existentials for real

The direct syntax for existentials using ∃ and type-value pairs would be quite convenient. UHC partially supports this direct syntax, but GHC does not. To introduce existentials in GHC we need to define new "wrapper" types.

Translating the example above:

```hs
{-# LANGUAGE ExistentialQuantification #-}

data Something = forall a. MkThing a (a -> a -> a) (a -> String)

term_a, term_b :: Something
term_a = MkThing 1 (+) show
term_b = MkThing "foo" (++) id

triple :: Something -> String
triple (MkThing x f out) =
  out (f (f x x) x)
```

There's a couple differences from our theoretical treatment. Type application, type abstraction, and type pairs are implicit. Also, the wrapper is confusingly written with `forall` rather than `exists`. This references the fact that we're declaring an existential type, but the data ctor has universal type:

```hs
MkThing :: forall a. a -> (a -> a -> a) -> (a -> String) -> Something
```

Often, we use existential quantification to "capture" a typeclass constraint. We could do something similar here:

```hs
data SomeMonoid = forall a. (Monoid a, Show a) => MkMonoid a
```


### Further reading

For an introduction to the theory, I highly recommend *Types and Programming Languages* by Pierce. For a discussion of existential types in GHC, see the GHC manual and the Haskell wiki.

- [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/)
- [GHC manual](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/data-type-extensions.html#existential-quantification)
- [Haskell wiki](http://www.haskell.org/haskellwiki/Existential_type)
