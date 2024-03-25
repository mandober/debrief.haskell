# Haskell Type Classes

Haskell builtin type classes can be categorized according to several factors, like their origin (abstract algebra, category theory, discrete data structures), their arity - the kind of the targeted type ctors (nullary, unary, binary, etc.; all except nullary were once called *constructor classes*), whether they use multi parameters (nominal classes, multiparam type classes), whether they use functional dependencies, and other factors.

Base (basic) type classes
- Eq
- Ord
- Bounded
- Enum
- Show
- Read
- Num

The sublass of type classes that pertain to category theory are much bigger:
- Semigroup
- Monoid
- Functor
  - Contravariant (functor)
  - Monoidal (functor)
  - Bifunctor
  - Profunctor
- Foldable
- Traversable
- Applicative
- Monad
  - MonadFail (extracted fail condition re do-notation)
- MonadZero (monad with zero and alternation)
- Alternative (applicative with zero and alternation)

Many builtin type class have gone through some refactoring of the base code so there are still some artefacts, illogical issues and inconsistencies.

There are also some types classes that seem identical except for their superclass - some with e.g. `Applicative`, others with `Monad` superclass. Although their functionality seems the same, the available superclass can make a difference (re available methods it provides), or just because it makes sense to use the lowest common denominator, as opposed to the highest, e.g. Monad is stronger than Applicative which is stronger than Functor although they all do some kind of mapping (and composition).
