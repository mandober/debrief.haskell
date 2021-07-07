# Data.Traversable

```hs
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  
  mapM      :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence  :: Monad m => t (m a) -> m (t a)
-- MINIMAL traverse | sequenceA

fmapDefault    :: Traversable t => (a -> b) -> t a -> t b
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m

for  :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
```
