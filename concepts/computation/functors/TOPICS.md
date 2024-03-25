# TOPICS

- CT and Haskell
  - correspondence between Haskell and CT functors
  - implementation of functors in Haskell
  - objects and arrows in CT
  - types (obj) and functions (arr) of the `Hask` category
  - obj, morphisms, iso, functors, NTs in CT vs Haskell



- Type class hierarchy in Haskell
  - Functor ⊆ Applicative ⊆ Monad
  - FAM type classes
    - Functor
    - Applicative
    - Monad
- Functors
  - Functors in CT
  - mapping between categories
  - endofunctors: mapping a cat to itself
  - structure-preserving mapping
  - exact correspondence between CT and Hask functors
  - Functors in Hask(ell)
    - mapping of types to types
    - mapping of functions to functions
    - type ctor `f` as type mapper
    - identity functor vs identity arrow
    - only endofunctors exist in Hask
  - methods of the Functor class
  - fmap, `<$>`, `<$`, `$>`, `<&>`, `void`
  - functoriality
    - variance
      - covariant functors
      - contravariant functors
      - bifunctors
      - profunctiors
    - linear types and multiplicity
      - Data.Functor
      - Control.Functor
    - other factors
      - forgetful functors
      - representable functors
- Applicative
  - Applicative functors
  - Applicative class
- Monads
  - Kleisli arrows
  - Kleisli categories
