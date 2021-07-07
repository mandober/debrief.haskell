# Control.Comonad

```hs
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
{-# MINIMAL fmap #-}

type Comonad :: (* -> *) -> Constraint
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  {-# MINIMAL extract, (duplicate | extend) #-}

type ComonadApply :: (* -> *) -> Constraint
class Comonad w => ComonadApply w where
  (<@>) :: w (a -> b) -> w a -> w b
  (@>)  :: w a -> w b -> w b
  (<@)  :: w a -> w b -> w a
  default (<@>) :: Applicative w => w (a -> b) -> w a -> w b

type role Cokleisli representational nominal representational
type      Cokleisli :: forall {k}. (k -> *) -> k -> * -> *
newtype   Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

(<$>) :: Functor f => (a -> b) -> f a -> f b
($>)  :: Functor f => f a -> b -> f b

(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(=<=) :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
(=>>) :: Comonad w => w a -> (w a -> b) -> w b

cfix  :: Comonad w => (w a -> a) -> w a
wfix  :: Comonad w => w (w a -> a) -> a
liftW :: Comonad w => (a -> b) -> w a -> w b

liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
kfix   :: ComonadApply w => w (w a -> a) -> w a

--                 (<*>) :: m (a -> b) -> m a -> m b
--            flip (<*>) :: m a -> m (a -> b) -> m b
(<@@>) :: ComonadApply w => w a -> w (a -> b) -> w b
```
