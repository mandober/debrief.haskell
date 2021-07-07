# Control.Monad.Error (deprecated use Control.Monad.Except)

```hs
(<$!>) :: Monad m => (a ->   b) -> m a -> m b
(<=<)  :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(=<<)  :: Monad m => (a -> m b) -> m a -> m b
(>=>)  :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

type  Error :: * -> Constraint
class Error a where
  noMsg :: a
  strMsg :: String -> a

type role ErrorT nominal representational nominal
type      ErrorT :: * -> (* -> *) -> * -> *
newtype   ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}

type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

type                   Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}

type             MonadError :: * -> (* -> *) -> Constraint
class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a
  {-# MINIMAL throwError, catchError #-}

type             MonadFail :: (* -> *) -> Constraint
class Monad m => MonadFail m where
  fail :: String -> m a
  {-# MINIMAL fail #-}

type             MonadFix :: (* -> *) -> Constraint
class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a
  {-# MINIMAL mfix #-}

type  MonadIO :: (* -> *) -> Constraint
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
  {-# MINIMAL liftIO #-}

type                                       MonadPlus :: (* -> *) -> Constraint
class (GHC.Base.Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

type  MonadTrans :: ((* -> *) -> * -> *) -> Constraint
class MonadTrans t where
  lift :: Monad m => m a -> t m a
  {-# MINIMAL lift #-}


ap :: Monad m => m (a -> b) -> m a -> m b

filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]

fix :: (a -> a) -> a

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forever :: Applicative f => f a -> f b
guard :: GHC.Base.Alternative f => Bool -> f ()
join :: Monad m => m (m a) -> m a
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4 :: Monad m =>
  (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5 ::
  Monad m =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> r)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
mapAndUnzipM ::
  Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapErrorT ::
  (m (Either e a) -> n (Either e' b))
  -> ErrorT e m a -> ErrorT e' n b
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  ...
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  ...
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
replicateM :: Applicative m => Int -> m a -> m [a]
replicateM_ :: Applicative m => Int -> m a -> m ()
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  ...
  sequence :: Monad m => t (m a) -> m (t a)
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
unless :: Applicative f => Bool -> f () -> f ()
void :: Functor f => f a -> f ()
when :: Applicative f => Bool -> f () -> f ()
zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_ :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()
```
