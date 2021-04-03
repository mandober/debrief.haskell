# Control.Monad.State

```hs
import Control.Monad.State


-- Defined in Control.Monad.Trans.State.Lazy
type role StateT nominal representational nominal
newtype StateT s (m :: * -> *) a = StateT { runStateT :: s -> m (a, s) }
--                                  ↑
-- the type of the StateT data ctor ↑
StateT :: (s -> m (a, s)) -> StateT s m a

type State s = StateT s Data.Functor.Identity.Identity :: * -> *

-- newVar    ::               a -> ST s (MutVar s a)
-- readVar   :: MutVar s a ->      ST s a
-- writeVar  :: MutVar s a -> a -> ST s ()

put          :: MonadState s m => s -> m ()
get          :: MonadState s m => m s
state        :: MonadState s m => (s -> (a, s)) -> m a

gets         :: MonadState s m => (s -> a) -> m a
modify       :: MonadState s m => (s -> s) -> m ()
modify'      :: MonadState s m => (s -> s) -> m ()

runState    :: State s a -> s -> (a, s)
evalState   :: State s a -> s -> a
execState   :: State s a -> s -> s

runStateT   ::            StateT s m a -> s -> m (a, s)
evalStateT  :: Monad m => StateT s m a -> s -> m a
execStateT  :: Monad m => StateT s m a -> s -> m s

mapState    :: (  (a, s) ->   (b, s)) -> State  s   a -> State  s   b
mapStateT   :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b

withState   :: (s -> s) -> State  s   a -> State  s   a
withStateT  :: (s -> s) -> StateT s m a -> StateT s m a

ap          :: Monad m => m (a -> b) -> m a -> m b

(<$!>)      :: Monad m => (a -> b) -> m a -> m b
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

filterM      :: Applicative m => (a -> m Bool) -> [a] -> m [a]
fix          :: (a -> a) -> a

foldM        :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM_       :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()

forM         :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM_        :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()

forever      :: Applicative f => f a -> f b

guard        :: GHC.Base.Alternative f => Bool -> f ()
join         :: Monad m => m (m a) -> m a

lift         :: (MonadTrans t, Monad m) => m a -> t m a
liftIO       :: MonadIO m => IO a -> m a
liftM        :: Monad m => (a1 -> r) -> m a1 -> m r
liftM2       :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3       :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4       :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5       :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r

mapAndUnzipM :: Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])


mfilter      :: MonadPlus m => (a -> Bool) -> m a -> m a
mfix         :: MonadFix m => (a -> m a) -> m a

mplus       :: MonadPlus m => m a -> m a -> m a
msum        :: (Foldable t, MonadPlus m) => t (m a) -> m a
mzero       :: MonadPlus m => m a

replicateM  :: Applicative m => Int -> m a -> m [a]
replicateM_ :: Applicative m => Int -> m a -> m ()


unless      :: Applicative f => Bool -> f () -> f ()
when        :: Applicative f => Bool -> f () -> f ()

void        :: Functor f => f a -> f ()

zipWithM    :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_   :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()


-- imported via Prelude, reexported from Control.Monad.State
return    :: Monad m => a -> m a

(=<<)     :: Monad m => (a -> m b) -> m a -> m b
(>>=)     :: Monad m => m a -> (a -> m b) -> m b

(>>)      :: Monad m => m a -> m b -> m b

fail      :: MonadFail m => String -> m a
fmap      :: Functor f => (a -> b) -> f a -> f b

mapM      :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
mapM_     :: (Foldable t,    Monad m) => (a -> m b) -> t a -> m ()

sequence  :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence_ :: (Foldable t,    Monad m) => t (m a) -> m ()


class Monad m => MonadState s (m :: * -> *) | m -> s
    get :: m s
    put :: s -> m ()
    state :: (s -> (a, s)) -> m a
{-# MINIMAL state | get, put #-}

class MonadTrans (t :: (* -> *) -> * -> *)

class Monad m => MonadFix (m :: * -> *)

class Monad m => MonadIO  (m :: * -> *)

class (GHC.Base.Alternative m, Monad m) => MonadPlus (m :: * -> *)

class Functor (f :: * -> *)

class Applicative m => Monad     (m :: * -> *)

class Monad       m => MonadFail (m :: * -> *)
```
