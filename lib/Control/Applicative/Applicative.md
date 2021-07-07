# Control.Applicative

```hs
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$)  :: Functor f => a -> f b -> f a


type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure  :: Applicative f => a -> f a
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b

(*>)    :: Applicative f => f a -> f b -> f b
(<*)    :: Applicative f => f a -> f b -> f a
(<**>)  :: Applicative f => f a -> f (a -> b) -> f b

liftA   :: Applicative f => (a -> b) -> f a -> f b
liftA2  :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3  :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

type Alternative :: (* -> *) -> Constraint
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some  :: f a -> f [a]
  many  :: f a -> f [a]
{-# MINIMAL empty, (<|>) #-}

optional :: Alternative f => f a -> f (Maybe a)



type role WrappedArrow representational nominal nominal
type      WrappedArrow :: (* -> * -> *) -> * -> * -> *
newtype   WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }
-- WrapArrow   :: a b c -> WrappedArrow a b c
-- unwrapArrow :: WrappedArrow a b c -> a b c


type role WrappedMonad representational nominal
type WrappedMonad :: (* -> *) -> * -> *
newtype WrappedMonad m a = WrapMonad {unwrapMonad :: m a}
-- WrapMonad   :: m a -> WrappedMonad m a
-- unwrapMonad :: WrappedMonad m a -> m a


type ZipList :: * -> *
newtype ZipList a = ZipList {getZipList :: [a]}
-- getZipList :: ZipList a -> [a]
-- ZipList    :: [a] -> ZipList a


type role Const representational phantom
type Const :: forall {k}. * -> k -> *
newtype Const a b = Const {getConst :: a}
-- Const    :: forall {k} a (b :: k). a -> Const a b
-- getConst :: forall {k} a (b :: k). Const a b -> a
```
