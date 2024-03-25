# Functors

## Functors in Haskell

Along with morphisms and natural transformations, functors are a fundamental concept in category theory.

In Haskell, functors are type ctors. In fact, functors are those type ctors that are members of the `Functor` class. Each one has its own implementation of the `fmap` function.

So, in Haskell, a functor is a type-level function, i.e. a type ctor, that maps one type into another using the `fmap` method.

In Haskell, a functor is a *unary type ctor*, `f :: Type -> Type`, that maps Haskell's types to types (which are objects in Hask) and Haskell's functions to functions (which are arrows in Hask). In CT, a functor maps a source category to a target category, but in Haskell, a functor can only map Hask to itself (or perhaps one of its subcategories), but in any case, Haskell's functors are then all *endofunctors*.

For example, the `Maybe` unary type ctor is a functor; for all types (objects) `a` and `b` in Hask, Maybe maps them to `Maybe a` and `Maybe b`. And for all functions `f : a -> b` … well, there are no reasonable functions of this type in Haskell - meaning that Haskell's parametric polymorphism is much stronger restriction on types compared to CT. In Haskell, we have to pick two discrete types, e.g. `a ~ Int` and `b ~ Char`. Then, first Maybe maps `Int` to `Maybe Int`, and `Char` to `Maybe Char`, and the function f is `f :: Int -> Char`.







For each type (object) `a` in Hask, the functor (type ctor) `f` produces a type `f a`.

For example, `Maybe` and `[]`, are functors (unary type ctors), so `Maybe` functor takes a type `a` and maps it into a type `Maybe a`, (a ⟼ f a); and `List` (or `[]`) maps a type `b` into `Maybe b`.

However, Haskell's parametric polymorphism is much stronger condition for the consequential mapping of functor (by NTs). In CT, 






## Functors in CT

A functor is a mapping between categories that preserves the structure of the source category. Preserving the structure means preserving composition and identities.

A functor is a mapping from a source category `C` to a target category `D`, denoted `F : C -> D` that maps
- objects in C to objects in D
  ∀a,b,c ∈ Ob(C). `a ⟼ F a` where `F a` ∈ Ob(D)

- morphisms in C to morphisms in D
  ∀f,g ∈ Ar(C) where f : a -> b, g : b -> c
  ∀f : a -> b ∈ Ar(C). `f ⟼ F f` where `F f` ∈ Ar(D)

- identity morphisms in C to identity morphisms in D:
  ∀1ₐ : a -> a ∈ Ar(C). `1ₐ` ⟼ `F 1ₐ` = `1ꜰₐ`

- composition of morphisms in C to composition of morphisms in D:
  g ∘ f : a -> c
  f ⟼ F f
  g ⟼ F g
  `g ∘ f` ⟼ `F (g ∘ f)` = `F g ∘ F f`
  F (g ∘ f) = F g ∘ F f : F a -> F c

## Types of functors

An *endofunctor* is a mapping from a category back to itself, `F : C -> C`.

An *identity functor* is a distinguished endofunctor that maps a category back to itself, `Iᴄ : C -> C`.

A *constant functor* corresponds to a kind of surjective function that maps every source object to the same target object. So a constant functor maps every object in a source category `C`, ∀a ∈ Ob(C), to the same object `c ∈ D` in the target category, and because of this it is identified by that target object and denoted by subscripting it, `Δᴄ : C -> D`, where c ∈ Ob(D).

`∀a ∈ Ob(C). ∃c ∈ Ob(D). a ⟼ c` by a constant functor `Δᴄ : C -> D`

A *full functor* is surjective when restricted to each set of morphisms that have a given source and target.

A *faithful functor* is injective when restricted to each set of morphisms that have a given source and target.

A *fully faithful functor* is bijective when restricted to each set of morphisms that have a given source and target.






## Refs

https://en.wikipedia.org/wiki/Functor
