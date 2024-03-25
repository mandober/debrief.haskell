# Category Theory Definitions

## 0-category
A 0-category is a set.

## n-category
An ordinary category has objects and morphisms, which are called 1-morphisms in the context of higher category theory. A 2-category generalizes this by also including 2-morphisms between the 1-morphisms. Continuing this up to n-morphisms between (n−1)-morphisms gives an n-category.

Just as the Cat, which is the category of small categories and functors is actually a 2-category with natural transformations as its 2-morphisms, the category n-Cat of (small) n-categories is actually an (n+1)-category.

An n-category is defined by induction on n by:

A 0-category is a set, An (n + 1)-category is a category enriched over the category n-Cat.
So a 1-category is just a (locally small) category.

Ordinary categories are 1-category in the context of higher category theory. A 2-category generalizes this by also including 2-morphisms between the 1-morphisms. Continuing this up to `n`-morphisms between `n− 1`-morphisms gives an n-category.


## 1
`1` is the denotation for the one-object one-morphism category. A category with a single object is called a monoid. Monoids may have any number of morphisms, but a special monoidal category that only has one morphism is denoted by `1`. [That morphism is the identity arrow, right? Or, perhaps it is nother arrow, if the identity arrow is considered implicit?]

## Monoid
A monoid is a category with a single object.

## Arrows
(Hughes' arrows)
Arrows are monoids in the category of profunctors.
Arrows are strong monads.
Arrows are roughly monads in the bicategory Prof of profunctors (distributors, modules).

## Dagger Arrows
Dagger arrows are involutive monoids in the category of profunctors.

## Monads
Monads are monoids in the category of endofunctors.
