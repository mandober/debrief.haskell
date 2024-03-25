# Functor, Applicative and Traversable

We will introduce the 3 abstractions in turn as a generalization of particular list-related functionality:
- mapping (Functor)
- zipping (Applicative)
- mapping with zipping (Traversable)

These list functions provide a good intuition for the mechanics of the generalizations.

However, for applicative functors, the notion of effectful computations provides a much better and more important intuition.

**Effectful computations** are computations that do not only (or necessarily) produce a result. They also do or can do something additional (such as logging, failing, or changing a mutable state) that somehow interacts with the context in which the computation takes place. These are called side effects of computation.

- What are functors?
- What are applicative functors?
- What computational effects can we model with applicative functors?
- How do we map over traversable functors with effectful functions?
