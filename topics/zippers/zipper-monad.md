```hs
-- A monad implementing The Zipper
-- http://haskell.org/haskellwiki/ZipperMonad

module Zipper where

import Control.Monad.State

data Loc c a = Loc
  { struct :: a
  , cxt    :: c }
  deriving (Show, Eq)

newtype Travel loc a = Travel { unT :: State loc a }
  deriving (Functor, Monad, MonadState loc)

-- Exit Points

-- get out of the monad
traverse :: Loc c a            -- starting location (initial state)
         -> Travel (Loc c a) a -- locational computation to use
         -> a                  -- resulting substructure
traverse start tt = evalState (unT tt) start

-- Mutation

-- modify the substructure at the current node
modifyStruct :: (a -> a) -> Travel (Loc c a) a
modifyStruct f = modify editStruct >> liftM struct get where
  editStruct (Loc s c) = Loc (f s) c

-- put a new substructure at the current node
putStruct :: a -> Travel (Loc c a) a
putStruct t = modifyStruct $ const t

-- get the current substructure
getStruct :: Travel (Loc c a) a
getStruct = modifyStruct id
-- works because modifyTree returns the 'new' tree
```
