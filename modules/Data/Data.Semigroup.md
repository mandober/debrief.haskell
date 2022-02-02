# Data.Semigroup

```hs
import Data.Semigroup

type Arg :: Type -> Type -> Type
data Arg a b = Arg a b
type ArgMax :: Type -> Type -> Type
type ArgMax a b = Max (Arg a b)
type ArgMin :: Type -> Type -> Type
type ArgMin a b = Min (Arg a b)
type Data.Semigroup.First :: Type -> Type
newtype Data.Semigroup.First a
  = Data.Semigroup.First {Data.Semigroup.getFirst :: a}
type Data.Semigroup.Last :: Type -> Type
newtype Data.Semigroup.Last a
  = Data.Semigroup.Last {Data.Semigroup.getLast :: a}
type Max :: Type -> Type
newtype Max a = Max {getMax :: a}
type Min :: Type -> Type
newtype Min a = Min {getMin :: a}
type WrappedMonoid :: Type -> Type
newtype WrappedMonoid m = WrapMonoid {unwrapMonoid :: m}
cycle1 :: forall m. Semigroup m => m -> m
diff :: forall m. Semigroup m => m -> Endo m
mtimesDefault :: forall b a. (Integral b, Monoid a) => b -> a -> a
type All :: Type
newtype All = All {getAll :: Bool}
type Any :: Type
newtype Any = Any {getAny :: Bool}
type Dual :: Type -> Type
newtype Dual a = Dual {getDual :: a}
type Endo :: Type -> Type
newtype Endo a = Endo {appEndo :: a -> a}
type Product :: Type -> Type
newtype Product a = Product {getProduct :: a}
type Semigroup :: Type -> Constraint
class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: GHC.Base.NonEmpty a -> a
  stimes :: forall b. Integral b => b -> a -> a
  {-# MINIMAL (<>) #-}
type Sum :: Type -> Type
newtype Sum a = Sum {getSum :: a}
stimesIdempotent :: forall b a. Integral b => b -> a -> a
stimesIdempotentMonoid ::
  forall b a. (Integral b, Monoid a) => b -> a -> a
stimesMonoid :: forall b a. (Integral b, Monoid a) => b -> a -> a
```
