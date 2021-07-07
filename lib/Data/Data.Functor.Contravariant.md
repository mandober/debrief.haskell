# Data.Functor.Contravariant

```hs
type  Contravariant :: (* -> *) -> Constraint
class Contravariant f where
  contramap :: forall a b. (a -> b) -> f b -> f a
  (>$)      :: forall a b.       b  -> f b -> f a
{-# MINIMAL contramap #-}

(>$<)  :: forall (f :: * -> *) a b. Contravariant f => (a -> b) -> f b -> f a
(>$$<) :: forall (f :: * -> *) b a. Contravariant f => f b -> (a -> b) -> f a
($<)   :: forall (f :: * -> *) b a. Contravariant f => f b ->       b  -> f a

type    Op :: * -> * -> *
newtype Op a b = Op { getOp :: b -> a }

type    Comparison :: * -> *
newtype Comparison a = Comparison { getComparison :: a -> a -> Ordering }

type    Equivalence :: * -> *
newtype Equivalence a = Equivalence { getEquivalence :: a -> a -> Bool }

type    Predicate :: * -> *
newtype Predicate a = Predicate { getPredicate :: a -> Bool }

comparisonEquivalence :: forall a. Comparison a -> Equivalence a
defaultComparison     :: forall a. Ord a => Comparison a
defaultEquivalence    :: forall a. Eq a => Equivalence a

phantom :: forall (f :: * -> *) a b. (Functor f, Contravariant f) => f a -> f b
```

## Comparison

```hs
fmap      :: forall a b.     (a -> b)             -> f a   -> f b
contramap :: forall a b.     (a -> b)             -> f b   -> f a
bimap     :: forall a b c d. (a -> b) -> (c -> d) -> p a c -> p b d
first     :: forall a b c.   (a -> b)             -> p a c -> p b c
second    :: forall a c d.               (c -> d) -> p a c -> p a d
```
