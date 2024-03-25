# Control.Monad

https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html


- Control
  - Control.Monad
    - Control.Monad.Cont
      - Control.Monad.Cont.Class
    - Control.Monad.Error
      - Control.Monad.Error.Class
      - Control.Monad.Error.Lens
    - Control.Monad.Except
    - Control.Monad.Fail
    - Control.Monad.Fix
    - Control.Monad.Identity
    - Control.Monad.Instances
    - Control.Monad.IO.Class
    - Control.Monad.List
    - Control.Monad.Reader
      - Control.Monad.Reader.Class
    - Control.Monad.RWS
      - Control.Monad.RWS.Class
      - Control.Monad.RWS.Lazy
      - Control.Monad.RWS.Strict
    - Control.Monad.Signatures
    - Control.Monad.ST
      - Control.Monad.ST.Lazy
        - Control.Monad.ST.Lazy.Safe
        - Control.Monad.ST.Lazy.Unsafe
      - Control.Monad.ST.Safe
      - Control.Monad.ST.Strict
      - Control.Monad.ST.Unsafe
    - Control.Monad.State
      - Control.Monad.State.Class
      - Control.Monad.State.Lazy
      - Control.Monad.State.Strict
    - Control.Monad.STM
    - Control.Monad.Trans
      - Control.Monad.Trans.Accum
      - Control.Monad.Trans.Class
      - Control.Monad.Trans.Cont
      - Control.Monad.Trans.Error
      - Control.Monad.Trans.Except
      - Control.Monad.Trans.Identity
      - Control.Monad.Trans.List
      - Control.Monad.Trans.Maybe
      - Control.Monad.Trans.Reader
      - Control.Monad.Trans.RWS
        - Control.Monad.Trans.RWS.CPS
        - Control.Monad.Trans.RWS.Lazy
        - Control.Monad.Trans.RWS.Strict
      - Control.Monad.Trans.Select
      - Control.Monad.Trans.State
        - Control.Monad.Trans.State.Lazy
        - Control.Monad.Trans.State.Strict
      - Control.Monad.Trans.Writer
        - Control.Monad.Trans.Writer.CPS
        - Control.Monad.Trans.Writer.Lazy
        - Control.Monad.Trans.Writer.Strict
    - Control.Monad.Writer
      - Control.Monad.Writer.Class
      - Control.Monad.Writer.Lazy
      - Control.Monad.Writer.Strict
    - Control.Monad.Zip


```hs
import Control.Monad

-- imported via Control.Monad
(<$!>)  :: Monad m => (a -> b) -> m a -> m b

(<=<)   :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>)   :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]

foldM   :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM_  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()

guard        :: GHC.Base.Alternative f => Bool -> f ()

mfilter      :: MonadPlus m => (a -> Bool) -> m a -> m a

forever      :: Applicative f => f a -> f b
mapAndUnzipM :: Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])

replicateM  :: Applicative m => Int -> m a -> m [a]
replicateM_ :: Applicative m => Int -> m a -> m ()
unless      :: Applicative f => Bool -> f () -> f ()
zipWithM    :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_   :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()

ap      :: Monad m => m (a -> b) -> m a -> m b
forM    :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM_   :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
join    :: Monad m => m (m a) -> m a

liftM  :: Monad m => (a1 -> r) -> m a1 -> m r
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4 :: Monad m
       => (a1 -> a2 -> a3 -> a4 -> r)
       -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5 :: Monad m
       => (a1 -> a2 -> a3 -> a4 -> a5 -> r)
       -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r

mplus   :: MonadPlus m => m a -> m a -> m a
msum    :: (Foldable t, MonadPlus m) => t (m a) -> m a
mzero   :: MonadPlus m => m a
void    :: Functor f => f a -> f ()
when    :: Applicative f => Bool -> f () -> f ()

-- imported via Prelude, Control.Monad
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(>>)  :: Monad m => m a -> m b -> m b
(>>=) :: Monad m => m a -> (a -> m b) -> m b

fail        :: MonadFail m => String -> m a
fmap        :: Functor f => (a -> b) -> f a -> f b
mapM        :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
mapM_       :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
return      :: Monad m => a -> m a
sequence    :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence_   :: (Foldable t, Monad m) => t (m a) -> m ()

class (GHC.Base.Alternative m, Monad m) => MonadPlus (m :: * -> *)

class Functor (f :: * -> *)
class Applicative m => Monad (m :: * -> *)
class Monad m => MonadFail (m :: * -> *)
```


https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html


```hs
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
