# Effect systems

- first only IO
- next came MT and composition of effects
- Service Handle Pattern
- ReaderT Pattern
- Effect systems
  - Final Tagless / mtl
  - PureScript 0.11 Effect System
  - freer-simple
  - extensible-effects
  - freer-effects
  - freer
  - polysemy
  - capability
  - Hierarchical Free Monads
  - Church encoded free monads
  - freer monads


**Effect tracking** means to transform various monads into a stack, so as to constraint possible behavior (interactions) of the code run in that monad stack to only those capabilities prescribed by the constraints (via what is available from those monads).


## Service Handle Pattern

```hs
data Handle = Handle { logger :: (String -> IO ()) }

logSomething :: Handle -> String -> IO ()
logSomething (Handle logger) msg = logger msg

main :: IO ()
main = do
  let handle = Handle { logger = putStrLn }
  logSomething handle "Hello World!"
```

## ReaderT Pattern

```hs
data Env = Env { logger :: (String -> IO ()) }

class HasLog a where
  getLog :: a -> !(String -> IO ())

instance HasLog (String -> IO ()) where
  getLog = id

instance HasLog Env where
  getLog = logger

logSomething :: (MonadReader env m, HasLog env, MonadIO m) => String -> m ()
logSomething msg = do
  env <- ask
  liftIO $ getLog env msg
```

## ReaderT pattern - full example

https://www.fpcomplete.com/blog/readert-design-pattern/

```hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
import Control.Concurrent.STM
import Say

data Env = Env
  { envLog :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

-- HasField class approach:
-- * if a type has the class, delegate to accessor
-- * if not, use id
-- Basically, each field of the Env type gets an HasField class

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasBalance a where
  getBalance :: a -> TVar Int
instance HasBalance (TVar Int) where
  getBalance = id
instance HasBalance Env where
  getBalance = envBalance

-- now we list capabilities as constraints

modify :: (MonadReader env m, HasBalance env, MonadIO m) => (Int -> Int) -> m ()
modify f = do
  env <- ask
  liftIO $ atomically $ modifyTVar' (getBalance env) f

logSomething :: (MonadReader env m, HasLog env, MonadIO m) => String -> m ()
logSomething msg = do
  env <- ask
  liftIO $ getLog env msg


main :: IO ()
main = do
  ref <- newTVarIO 4
  -- initialize/instantiate concrete env
  let env = Env
    { envLog = sayString
    , envBalance = ref
    }
  runReaderT
    -- these may run in parallel (?)
    (concurrently
      (modify (+ 1))
      (logSomething "Increasing account balance")
    )
    env
  balance <- readTVarIO ref
  sayString $ "Final balance: " ++ show balance
```

Holy creeping boilerplate batman! Yes, type signatures get longer, rote instances get written. But our type signatures are now deeply informative, and we can test our functions with ease (if defining all of these classes manually bothers you, you can use `lens`).

```hs
main :: IO ()
main = hspec $ do
  describe "modify" $ do
    it "works" $ do
      var <- newTVarIO (1 :: Int)
      runReaderT (modify (+ 2)) var
      res <- readTVarIO var
      res `shouldBe` 3
  describe "logSomething" $ do
    it "works" $ do
      var <- newTVarIO ""
      let logFunc msg = atomically $ modifyTVar var (++ msg)
          msg1 = "Hello "
          msg2 = "Worldn"
      runReaderT (logSomething msg1 >> logSomething msg2) logFunc
      res <- readTVarIO var
      res `shouldBe` (msg1 ++ msg2)
```

## Effect systems

```hs
-- ----------------------------------------------------------------------------
-- Final Tagless / mtl
-- ----------------------------------------------------------------------------
class Monad m => Logger m where
  logMessage :: LogLevel -> Message -> m ()

class Monad m => Random m where
  getRandomInt :: (Int, Int) -> m Int

class (Logger m, Random m) => Lang m

someFunc :: Lang m => m ()

-- ----------------------------------------------------------------------------
-- PureScript 0.11 Effect System
-- ----------------------------------------------------------------------------
someFunc :: forall e . Eff
  ( avar      :: AVAR
  , exception :: EXCEPTION
  , fs        :: FS
  , console   :: CONSOLE
  | e
  )
  Result

-- ----------------------------------------------------------------------------
-- freer-simple
-- ----------------------------------------------------------------------------
data FileSystem r where
  ReadFile :: FilePath -> FileSystem String
  WriteFile :: FilePath -> String -> FileSystem ()

readFile1 :: Member FileSystem effs => FilePath -> Eff effs String
readFile2 :: FilePath -> Eff '[FileSystem] String
runFileSystem :: Eff (FileSystem ': effs) ~> Eff effs

-- ----------------------------------------------------------------------------
-- extensible-effects
-- ----------------------------------------------------------------------------
data Log v where
  Log :: String -> Log ()

log :: Member Log r => String -> Eff r ()
runLogger :: Eff (Log :> r) a -> Eff r (a, [String])

-- ----------------------------------------------------------------------------
-- freer-effects
-- ----------------------------------------------------------------------------
data State s v where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) r => Eff r s
runState :: Eff (State s ': r) a -> s -> Eff r (a, s)

-- ----------------------------------------------------------------------------
-- freer
-- ----------------------------------------------------------------------------
data Teletype s where
  PutStrLn    :: String -> Teletype ()
  GetLine     :: Teletype String
  ExitSuccess :: Teletype ()

putStrLn' :: Member Teletype r => String -> Eff r ()
runTeletype :: Eff '[Teletype] w -> IO w

-- ----------------------------------------------------------------------------
-- polysemy
-- ----------------------------------------------------------------------------
data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

echo :: Member Teletype r => Sem r ()

runTeletype
  :: Member (Embed IO) r 
  => Sem (Teletype ': r) a -> Sem r a
```
