# Haskell :: Concepts :: Functors :: Functor variance

- Variance
- Positive and negative positions
- Covariant functors
- Contravariant functors
- Invariant functors
- Nonvariant functors

## Variance

A *variant type system* is one where any form of variance occurs. Otherwise, a type system is called invariant or nonvariant.

Within a type system, variance occurs in several forms:
- covariance preserves the ordering of types (from specific to generic)
- contravariance reverses the ordering of types (from generic to specific)
- bivariance is when both co/contravariance apply at the same time
- invariance or nonvariance is when a type system lacks variance



Variance is associated to a type ctor's type parameters. Each type parameter has a specific variance.

Most types that are functors make for covariant functors. The function type is a canonical example of a type that is both covariant and contravariant. Namely, the function type `a -> b` is contravariant in the input type, i.e. in the type parameter `a`, and covariant in the return type, i.e. in type parameter `b`.

Howeveer, the function type `a -> a` is invariant since the type parameter `a` occurs in both the positive and negative position.



## Positive and negative positions
