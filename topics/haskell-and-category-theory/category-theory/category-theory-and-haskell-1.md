# Category theory and Haskell

https://en.wikibooks.org/wiki/Haskell/Category_theory

This article attempts to give an overview of category theory in so far as it applies to Haskell.

## Category

A category is a collection of objects and arrows between them. Actually, it has one more components, the axioms.

1. A collection of objects
2. A collection of morphisms
3. A notion of composition of arrows
3. Axioms


A collection of morphisms, each of which connects two objects (a source object and a target object) together. If `f` is a morphism with source object `a` and target object `b`, it is denoted by `f : a -> b`.

A notion of composition of arrows: if `f : a -> b` and `g : b -> c` are two morphisms, then they can be composed, resulting in a morphism `g ∘ f : a -> c`.


Lots of things form categories.

`Set` is a category of sets, with sets as objects and functions as morphisms, with composition being the function composition.

`Grp` is the category of groups, with the carrier sets as objects and homomorphisms (preserve group operations) as arrows. For any two groups, `(G,⨀)` and `(H,·)`, a function `k : G -> H` is a morphism (homomorphisms) in `Grp` if `k (u ⨀ v) = k u ∙ k v`.

It may seem that morphisms are always functions, but that needn't be the case.

For example, any partial order `(P, ≤)` defines a category where the objects are the elements of `P`, and there is a morphism between any two objects `a` and `b` iff `a ≤ b`.

Moreover, it's possible to have multiple morphisms with the same source and target objects. In `Set`, for example, `sin` and `cos` are both functions with source `ℝ` and target `[-1,1]`, but they're not the same morphism.
