# Functors

In category theory, a functor is a structure-preserving mapping between categories.


A functor is a design pattern inspired by the category theory.

A functor is a structure-preserving transformation in that a function can be aplied to each element inside a structure described by some generic type, without the function affecting the structure itself.

Functor class is for types that can be *covariantly* mapped over.

Mapping a structure means applying a function to all its elements, while preserving the structure itself. Therefore, a functor is *structure-preserving transformation*.

An intuitive understanding of a functor could be that it represents a container of sorts, along with the mapping functionality.


Another view is that `fmap` lifts a function of values (normal function) into a *function of values in a particular functorial context*.


Generally, the idea of mapping a function over each element of a data structure isn't specific to lists, but can be abstracted further to a wider range of parameterised types. The class of types that support such mapping are called functors.

fmap :: Functor f => (a -> b) -> f a -> f b

A function, (a -> b), is applied to a structure of type `f a` (so with elements of type `a`) by applying the function to each element of the structure, producing the same structures but with possibly different type of elements, `b`. This shows that the functors mapping only effects the elements inside, without changing the structure itself. Because of that it is said that functor transformations preserve structure.


Functor wants a type constructor that takes one type and not a concrete type.






## References

1. Programming in Haskell. Graham Hutton, 2016. Chapter 12. Functors
1. Learn You a Haskell. http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
1. Learn You a Haskell. http://learnyouahaskell.com/functors-applicative-functors-and-monoids
1. https://wiki.haskell.org/Functor
1. https://en.wikipedia.org/wiki/Functor
1. https://en.wikipedia.org/wiki/Functor_(functional_programming)
1. https://medium.com/@dtinth/what-is-a-functor-dcf510b098b6
