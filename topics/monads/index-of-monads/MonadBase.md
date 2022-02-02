# MonadBase

https://wiki.haskell.org/New_monads/MonadBase

Lift computations from the bottom of a transformer stack.

The `liftIO` function from `MonadIO` can be generalized to access whatever the base of a transformer stack happens to be (so there is no need for a `liftSTM`, `liftST`, etc.).

```hs
module MonadBase (MonadBase, liftBase, MonadIO', liftIO') where

import Data.Monoid (Monoid)
import Control.Monad.Trans (lift,MonadTrans)

-- All the base monads with GHC
import Control.Monad.ST.Strict  as S (ST)
import Control.Monad.ST.Lazy    as L (ST)
import Control.Concurrent.STM (STM)
import Control.Monad.Identity (Identity)
import Text.ParserCombinators.Parsec (GenParser)

-- And all the MonadIO instances:
import Control.Monad.List (ListT)
import Control.Monad.Cont (ContT)
import Control.Monad.Error  (ErrorT,Error)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.RWS (RWST)

class (Monad m,Monad b) => MonadBase b m where
    liftBase :: b a -> m a

-- One can recover MonadIO and liftIO from MonadBase
class (MonadBase IO m) => MonadIO' m where
    liftIO' :: IO a -> m a
    liftIO' = liftBase

-- Base monads
instance MonadBase IO IO                                  where liftBase = id
instance MonadBase STM STM                                where liftBase = id
instance MonadBase (GenParser tok st) (GenParser tok st)  where liftBase = id
instance MonadBase (S.ST s) (S.ST s)                      where liftBase = id
instance MonadBase (L.ST s) (L.ST s)                      where liftBase = id
instance MonadBase Maybe Maybe                            where liftBase = id
instance MonadBase [] []                                  where liftBase = id
instance (Error e) => MonadBase (Either e) (Either e)     where liftBase = id
instance MonadBase ((->) a) ((->) a)                      where liftBase = id
instance MonadBase Identity Identity                      where liftBase = id

-- Trans monads
instance MonadBase b m => MonadBase b (ListT m) where
    liftBase = lift . liftBase

instance MonadBase b m => MonadBase b (ContT r m) where
    liftBase = lift . liftBase

instance (Error e, MonadBase b m) => MonadBase b (ErrorT e m) where
    liftBase = lift . liftBase

instance MonadBase b m => MonadBase b (ReaderT r m) where
    liftBase = lift . liftBase

instance MonadBase b m => MonadBase b (StateT s m) where
    liftBase = lift . liftBase

instance (Monoid w, MonadBase b m) => MonadBase b (WriterT w m) where
    liftBase = lift . liftBase

instance (Monoid w, MonadBase b m) => MonadBase b (RWST r w s m) where
    liftBase = lift . liftBase


-- ----------------------------------------------------------------------------
-- And an artificial example
-- ----------------------------------------------------------------------------
import MonadBase
import Control.Monad.Reader
import Control.Monad.Writer

type Foo a = WriterT [Int] (ReaderT String []) a

foo :: Foo String
foo = do
    x <- liftBase [1,2,3]
    s <- ask
    tell [succ x]
    return (s ++ show x)

test = runReaderT (runWriterT foo) "hello"
-- [("hello1",[2]),("hello2",[3]),("hello3",[4])]
```
