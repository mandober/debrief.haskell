# Applicative

## Applicative functions

Applicative functions
- methods
  - pure
  - (<*>)
  - (*>)
  - (<*)
  - liftA2
- minimal class definition: pure, ((<*>) | liftA2)
- associated functions
  - (<**>)
  - liftA
  - liftA3
  - when
  - ap


## Applicative class

```hs
class Functor f => Applicative f where
  -- | Lift a value.
  pure :: a -> f a

  -- | Sequential application.
  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 id

  -- | Lift a binary function to actions.
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x = (<*>) (fmap f x)

  -- | Sequence actions, discarding the value of the first argument.
  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2

  -- | Sequence actions, discarding the value of the second argument.
  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const

  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```

## Applicative: additional methods

```hs
-- | A variant of <*> with the args flipped. It differs from `flip (<*>)` in that the effects are resolved in the order the args are presented.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)

-- | Lift a function to actions.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = f <$> a

-- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

-- | Conditional execution of Applicative expressions.
when :: (Applicative f) => Bool -> f () -> f ()
when p s = if p then s else pure ()

-- | ap is fmap with Applicative constraints
ap :: Applicative f => (a -> b) -> f a -> f b
ap = fmap
```
