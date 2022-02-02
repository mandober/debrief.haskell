# GHC.Base

https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.0.0/GHC-Base.html

- module: `GHC.Base`
- package: base-4.16.0.0
- Basic data types and classes

```hs
augment :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
(++) :: [a] -> [a] -> [a]
build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
foldr :: (a -> b -> b) -> b -> [a] -> b
eqString :: String -> String -> Bool
bindIO :: IO a -> (a -> IO b) -> IO b
returnIO :: a -> IO a
otherwise :: Bool
assert :: Bool -> a -> a
thenIO :: IO a -> IO b -> IO b
breakpoint :: a -> a
breakpointCond :: Bool -> a -> a
map :: (a -> b) -> [a] -> [b]
($) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
join :: Monad m => m (m a) -> m a
class Applicative m => Monad m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
  (>>) :: forall a b. m a -> m b -> m b
  return :: a -> m a
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: Integral b => b -> a -> a
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
type String = [Char]
data NonEmpty a = a :| [a]
data Opaque = forall a. O a
($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
(.) :: (b -> c) -> (a -> b) -> a -> c
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
ap :: Monad m => m (a -> b) -> m a -> m b
asTypeOf :: a -> a -> a
const :: a -> b -> a
divInt :: Int -> Int -> Int
divModInt :: Int -> Int -> (Int, Int)
divModInt# :: Int# -> Int# -> (# Int#, Int# #)
failIO :: String -> IO a
flip :: (a -> b -> c) -> b -> a -> c
getTag :: a -> Int#
iShiftL# :: Int# -> Int# -> Int#
iShiftRA# :: Int# -> Int# -> Int#
iShiftRL# :: Int# -> Int# -> Int#
id :: a -> a
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
mapFB :: (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
maxInt :: Int
minInt :: Int
modInt :: Int -> Int -> Int
ord :: Char -> Int
quotInt :: Int -> Int -> Int
quotRemInt :: Int -> Int -> (Int, Int)
remInt :: Int -> Int -> Int
sequence :: Monad m => [m a] -> m [a]
shiftL# :: Word# -> Int# -> Word#
shiftRL# :: Word# -> Int# -> Word#
unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unsafeChr :: Int -> Char
until :: (a -> Bool) -> (a -> a) -> a -> a
when :: Applicative f => Bool -> f () -> f ()

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

-- rexports:
module GHC.Classes
module GHC.CString
module GHC.Magic
module GHC.Types
module GHC.Prim
module GHC.Prim.Ext
module GHC.Err
module GHC.Maybe
```
