# Haskell :: Index :: Type classes

Superclasses are in parens.

- Basic type classes
  - Eq
  - Ord (Eq)
  - Read
  - Show
  - Bounded
  - Enum
- Numeric type classes
  - Num
  - Integral (Real, Enum)
  - Floating (Fractional)
  - Fractional (Num)
  - Real (Ord, Num)
  - RealFloat (RealFrac, Floating)
  - RealFrac (Real, Fractional)
  - Bits (Num)
  - FiniteBits (Bits)
- Utility type classes
  - IsList
  - IsString
  - HasField
  - HasLabel
- Algebraic type classes
  - Semigroup
  - Monoid (Semigroup)
- Categorical type classes
  - Functor                                Control.Monad
  - Apply
  - Bind (Apply)
  - Bifunctor                              Data.Bifunctor
  - Biapply (Bifunctor)                    Data.Bifunctor
  - Foldable                               Data.Foldable
  - Traversable (Functor, Foldable)        Data.Traversable
  - Applictive (Functor)                   Control.Applicative
  - Alternative (Applicative)              Control.Applicative
  - Monad (Applicative)                    Control.Monad
  - MonadPlus (Alternative, Monad)         Control.Monad
  - MonadFail (Monad)                      Control.Monad
  - MonadFix
  - MonadIO
  - MonadState
  - MonadTrans
  - Category                               Control.Category
- package `profunctors`
  - Profunctor                             Data.Profunctor
- package `comonad`
  - Comonad
  - Env
  - Hoist
  - Store
  - Traced
  - Trans
- package `contravariant`
  - Contravariant
  - Divisible (Divisible)
  - Decidable (Divisible)
- package `MonadRandom`
  - MonadRandom
  - MonadSplit
  - MonadInterleave
- package `arrows`
  - Arrow                                  Control.Arrow
- package `mtl`
  - MonadCont (Monad)
  - Error
  - RWS
  - Reader
  - MonadWriter (Monoid, Monad)
  - State
  - Trans
- package `transformers`
- package `transformers-base`
- package `logict`
- package `monad-control-identity`
  - MonadBase (Applicative, Monad)
  - MonadBaseControl (MonadBase)
  - MonadBaseControlIdentity (MonadBaseControl)
- package `semigroupoids`
  - Bifoldable1
  - Bitraversable
  - Bitraversable1 (Bitraversable, Bifoldable1)
  - Traversable1 (Traversable, Foldable1)
  - Apply (Functor), strong lax semimonoidal endofunctor



## Refs

### Packages

https://hackage.haskell.org/package/base
https://hackage.haskell.org/package/mtl
https://hackage.haskell.org/package/profunctors
https://hackage.haskell.org/package/contravariant
https://hackage.haskell.org/package/transformers
https://hackage.haskell.org/package/transformers-base/
https://hackage.haskell.org/package/free
https://hackage.haskell.org/package/comonad
https://hackage.haskell.org/package/semigroupoids
