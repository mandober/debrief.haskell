# Haskell :: Concepts :: Functors

- Functors
  - what are functors
- Functors as ...
  - container
  - context
  - computation?
  - structure
- Functorial structure
  - notion of shape
  - notion of type
  - notion of size
  - notion of polymorphic contents
- free theorems, theorems for free
- Functors in ...
  - Functor in CT
  - Functor in PLs
  - Functor in Haskell
- Theoretical aspects
- Practical aspects
- implementation in Haskell as a class
- deriving the concept of functors from first principles
- mapping
- structure-preserving mapping
  - preserves the type of the structure
  - preserves the "size" of the structure (number of elements)
  - preserves identity arrows (in CT)
  - preserves composition (in CT)
- generalizing mapping of list
- functor as a container
- functor as a context
- functor as a computation?
- mutable contents, immutable structure shape
- types of functors
  - constant functor
  - identity functor
  - composition of functors
- variance
  - covariant functor (functor)
  - contravariant functor
- functoriality
  - Functor, fmap
  - Bifunctor, bimap
  - Profunctor, dimap
  - Contravariant, cmap
- applicatives and monads are functors


`Control.Functor.Linear`
The functors in this module are called control functors, which are different from the data functors in `Data.Functor.Linear`.
https://hackage.haskell.org/package/linear-base-0.4.0/docs/Control-Functor-Linear.html

- Data.Functor
- Data.Functor.Contravariant
- Data.Functor.Bifunctor
- Data.Functor.Profunctor
- Data.Functor.Linear
- Control.Functor.Linear


* Data vs Control functors
https://www.tweag.io/blog/2020-01-16-data-vs-control/
