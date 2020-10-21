# Control.Monad

```hs
void   :: (Functor f) => f a -> f ()


-- flip bind
(=<<)  :: (Monad m) => (a -> m b) -> m a -> m b
-- strict fmap
(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
-- fish: monadic composition (and flip fish)
(>=>)  :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(<=<)  :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
liftM  :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: (Monad m) =>(a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
join   :: (Monad m) => m (m a) -> m a
ap     :: (Monad m) => m (a -> b) -> m a -> m b


forever      :: (Applicative f) => f a -> f b
guard        :: (Alternative f) => Bool -> f ()
unless       :: (Applicative f) => Bool -> f () -> f ()
when         :: (Applicative f) => Bool -> f () -> f ()
replicateM   :: (Applicative m) => Int -> m a -> m [a]
replicateM_  :: (Applicative m) => Int -> m a -> m ()
mapAndUnzipM :: (Applicative m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
zipWithM     :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_    :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
filterM      :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]

foldM     :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
foldM_    :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m ()
forM_     :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ()
mapM_     :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
sequence_ :: (Monad m, Foldable t) => t (m a) -> m ()

forM      :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)

mfilter   :: (MonadPlus m)             => (a -> Bool) -> m a -> m a
msum      :: (MonadPlus m, Foldable t) => t (m a) -> m a




liftM4 :: Monad m =>
   (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r

liftM5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r


class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}

class Monad m => MonadFail (m :: * -> *) where
  fail :: String -> m a
  {-# MINIMAL fail #-}


class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  ...
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  ...


class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  ...
  sequence :: Monad m => t (m a) -> m (t a)
```
