# Lifting

https://wiki.haskell.org/Lifting

Lifting is a concept which allows you to transform a function into a corresponding function within another, usually more general, setting.

- Lifting in general
- Applicative lifting
- Monad lifting
- Arrow lifting

## Lifting in general

If we restore the parens (redundant due to the associativity) of `fmap`, then it can be thought as a lifting operation: it transforms ("lifts") a function between simple types `a` and `b`, into a function within a context, `f a -> f b`

```hs
-- takes 2 params: a funcarg and a param
fmap :: Functor f => (a -> b) -> f a -> f b
-- takes 1 funcarg and transforms it
fmap :: Functor f => (a -> b) -> (f a -> f b)
```

Considering a (covariant) functor like a pair `(a,b)`, we must realize that a 2-tuple is made a `Functor` instance by fixing the first component, `a`, such that fmap operates exclusively on the second component `b` (and ignores `a`). This has to be done because the `Functor` class expects a type ctor to have the kind `* -> *`, and the pair type ctor `(,)` has kind `* -> * -> *`; it's only when we partially apply the first type arg, obtaining `(,) a`, that it has the suitable kind.

```hs
type (,) :: * -> * -> *
data (,) a b = (,) a b

type (,) :: * -> * -> *
type (,) a :: * -> *
type (,) a b :: *
```

We can define a homogeneous pair type as `Pair a = (a, a)` and make it a `Functor` instance. The `fmap` is already a lifting operation: it transforms a function between simple types `a` and `b` into a function between these types in a context.

```hs
data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- UNARY LIFT (fmap)
lift :: (a -> b) -> Pair a -> Pair b
lift = fmap

plus2 :: Pair Int -> Pair Int
plus2 = lift (+2)
-- plus2 (Pair 2 3) ~~> Pair 4 5


-- actual 'lift' from Control.Monad.Trans.Class.MonadTrans
lift :: (MonadTrans t, Monad m) => m a -> t m a
```

However, not all functions between `Pair a` and `Pair b` can be "lifted", e.g. obviously a function like `\(x, _) -> (x, 0)` cannot be lifted.

A functor can only lift unary functions, but we want to lift functions of any arity.

```hs
-- (place in context, also called "lift")
lift0 :: a -> Pair a
lift0 x = Pair x x

-- BINARY LIFT
lift2 :: (a -> b -> r) -> (Pair a -> Pair b -> Pair r)
lift2 f (Pair x1 x2) (Pair y1 y2) = Pair (f x1 y1) (f x2 y2)

plus :: Pair Int -> Pair Int -> Pair Int
plus = lift2 (+)
-- plus (Pair 1 2) (Pair 3 4) ~~> Pair 4 6
```

In a similar way, we can define lifting operations for all containers that have "a fixed size", like `Double -> a` functions (might be thought of as values that are varying over time), and, e.g. `\t -> if t < 2.0 then 0 else 2` repr a value which switches at time 2.0 from 0 to 2. Using lifting, these functions can be manipulated at a very high-level.

In fact, this kind of lifting operation is already defined in `Control.Monad.Reader`, which provides Functor, Applicative, Monad, MonadFix and MonadReader instances for the type `(->) r` i.e. functions with fixed input, aka "readers". The `liftM{,2,3,4,5}` functions of the Reader monad are precisely the lifting operations we seek.

If the size of the containers isn't fixed, it's not always clear how to make lifting operations for them. For example, list type could be lifted using the 'zipWith' family of functions or using `liftM` from the list monad.

```hs
fmap   :: Functor f => (a -> b) -> f a -> f b
liftM  :: Monad m   => (a -> b) -> m a -> m b

liftM  :: Monad m
       => (a1 -> r)
       -> m a1
       -> m r

liftM2 :: Monad m
       => (a1 -> a2 -> r)
       -> m a1
       -> m a2
       -> m r
```


## Applicative lifting

This should only provide a definition of what lifting means (in the usual cases, not in the arrow case). It's not a suggestion for an implementation. I start with the (simplest?) basic operations zipL, which combines to containers into a single one and zeroL, which gives a standard container for ().

```hs
class Functor f => Liftable f where
    zipL :: f a -> f b -> f (a, b)
    zeroL :: f ()
    
liftL :: Liftable f => (a -> b) -> (f a -> f b)
liftL = fmap

liftL2 :: Liftable f => (a -> b -> c) -> (f a -> f b -> f c)
liftL2 f x y = fmap (uncurry f) $ zipL x y

liftL3 :: Liftable f => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
liftL3 f x y z = fmap (uncurry . uncurry $ f) $ zipL (zipL x y) z

liftL0 :: Liftable f => a -> f a
liftL0 x = fmap (const x) zeroL 

appL :: Liftable f => f (a -> b) -> f a -> f b
appL = liftL2 ($)
```

We need to postulate a few laws so that the definitions make sense.

```hs
assoc :: ((a, b), c) -> (a, (b, c))
assoc ~(~(x, y), z) = (x, (y, z))

{-
Identity:
fmap snd $ zipL zeroL x === x
fmap fst $ zipL x zeroL === x

Associativity:
fmap assoc $ zipL (zipL x y) $ z === zipL x $ zipL y z
-}
Today we have the Applicative class that provides Applicative functors. It is equivalent to the Liftable class.

pure = liftL0
(<*>) = appL

zeroL = pure ()
zipL = liftA2 (,)
```


## Monad lifting

Lifting is often assoc with monads. The members of the `liftM*` family of functions take a function and perform the corresponding computation within the monad.

```hs
return :: (Monad m) => a -> m a
liftM  :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
```

Consider for example the list monad, `MonadList`. It performs a nondeterministic calculation, returning all possible results. `liftM2` just turns a deterministic function into a nondeterministic function.

```hs
plus :: [Int] -> [Int] -> [Int]
plus = liftM2 (+)
-- plus [1,2,3] [3,6,9] -- [4,7,10, 5,8,11, 6,9,12]
-- plus [1..] []        -- ⟘
-- plus [] [1..]        -- []
```

Every Monad can be made an instance of `Liftable` using these default implementations:

```hs
{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE AllowUndecidableInstances #-}

import Control.Monad

instance (Functor m, Monad m) => Liftable m where 
  zipL  = liftM2 (\x y -> (x,y))
  zeroL = return ()
```


Lifting becomes especially interesting when there are more levels to lift between. `Control.Monad.Trans` defines the `MonadTrans` class for this.

```hs
class MonadTrans t where
  -- lifts a value from the inner monad m to the transformed monad t m
  -- could be called lift0
  lift :: Monad m => m a -> t m a
```

The `lift` takes the side effects of a monadic computation within the inner monad `m` and lifts them into the transformed monad `t m`.

We can easily define functions that lift functions between inner monads to functions between transformed monads.

Then we can perform 3 different lifting operations:
- `liftM` transforms a pure function to a function between inner monads
- `liftM` transforms a pure function between transformed monads
- `lift`  transforms from the inner monad to the transformed monad

Because of the purity of Haskell, we can only lift "up".


## Arrow lifting

Until now, we have only considered lifting from functions to other functions. John Hughes' arrows (see Understanding arrows) are a generalization of computation that aren't functions anymore. An arrow a b c stands for a computation which transforms values of type b to values of type c. The basic primitive `arr`, aka `pure`, is also a lifting operation.

```hs
arr :: (Arrow a) => (b -> c) -> a b c
```
