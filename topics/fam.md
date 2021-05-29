# Functors

<!-- TOC -->

- [Functors](#functors)
- [Applicative Functors](#applicative-functors)
- [Monads](#monads)

<!-- /TOC -->


It's all about function application.

* applying a function `a -> b` to an arg  `a`  produces `b`
  - `apply :: (a -> b) -> a -> b`
  - succ 1

* applying a function `a -> b` to an arg `[a]` produces `[b]`
  - `map :: (a -> b) -> [a] -> [b]`
  - map succ []
  - map succ [1]
  - map succ [1,2]

* applying a function `a -> b` to an arg `f a` produces `f b`
  - `fmap :: Functor (f :: * -> *) => (a -> b) -> f a -> f b`
  - fmap succ (Just 1)
  - fmap succ [1]
  - fmap succ [1,2]

* applying a function `a -> b -> c` to args `f a` and `f b` produces `f c`
  - `(<*>) :: Applicative (f :: * -> *) => f (a -> b) -> f a -> f b`
  - pure succ <*> Just 1
  - pure (+) <*> Just 1 <*> Just 2



## Functors

Functors are about mapping the values of a data structure while preserving the structure. Functors generalize the idea of mapping a function to a family of data structures.

The introduction of functors can be prompted by considering the elegance and easy of everyday function application. Given a function `f`, you apply it to an arg `x`, by doing `f x`, and you have mapped a value.

However, we'd like a way to just as easily apply `f` to a list of values. In fact, we want to generalize this pattern of mapping a function to a whole bunch of structures, no matter how weirdly-shaped they may be.

The issue of mapping a list is that regular function application has no knowledge how to apply a function like `f` to the structured data, in this case, to a list. What we need is a specialized function, so we define `map` that is a HOF, the same as the `apply`, in that they both take as arg a regular function. They also take its arg and set up the application of the former to the latter, forwarding (returning) their result.

```hs
-- applying a function to a value
apply :: (a -> b) -> a -> b
apply f x = f x
-- f x ≅ apply f x

-- applying a function to a list of values
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
```

The `map` nicely generalizes the function application to the mapping of lists, but we can generalize further to cover the mapping of other data structure as well (`Maybe a`, `(,) a`, `Either a`, trees, even functions, etc.).

Thanks to algebraic data types, we can group different data types by parametrizing their type ctor into a type class called `Functor`.

The type param `f` used to represent the instance has the kind `* -> *`, i.e. not just `*`, which is the kind of saturated types. This means that only the types whose form fits the pattern `f a` are applicable, and these are the types such as `[] a` i.e. `[a]`, `Maybe a`.

However, the form such as `Maybe a` represents the saturated type ctor, e.g. `Maybe` type ctor with its type param `a` *already applied* (to e.g. `Int` thereby producing `Maybe Int`), which has the wrong kind to be an instance of the Functor class.

```hs
-- Fully saturated type ctors always have the kind star
ghci> :k Maybe Int :: *
ghci> :k Maybe Float :: *
ghci> :k Maybe (Func Int Int) :: * -- given:
newtype Func a b = F (a -> b)

-- General form of the Maybe type ctor where its type
-- param is applied (`a` stands for some concrete type)
ghci> :k Maybe a :: *

-- Unsaturated (unapplied) type ctor Maybe.
-- This is what the Functor wants!
ghci> :k Maybe :: * -> *
```

the Functor class a

the applicable form for Functor's instances is `Maybe`, and that is exactly what is made an instance of the Functor class.


```hs
class Functor (f :: * -> *) where
```






## Applicative Functors

Applicative functors generalize the idea of applying a function to a data structure, but a function that may have more then one argument (applying a unary function is covered by the functors).

```hs
fmap0 :: a -> f a
fmap1 :: (a -> b) -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
...

pure = fmap0
fmap = fmap1

fmap f x = pure f <*> x
(<*>) :: f (a -> b) -> f a -> f b

fmap0 a                   = f a
fmap1 g (f a)             = f (g a)
fmap2 g (f a) (f b)       = f (g a b)
fmap3 g (f a) (f b) (f c) = f (g a b c)
...


-- applying a UNARY function to a data structure (to data in a context)
fmap (+3)    (Just 5) -- Just 8
     (+3) <$> Just 5  -- Just 8
pure (+3) <*> Just 5  -- Just 8
-- lifting a UNARY function

-- applying a BINARY function
     (+)          5          7 --      12
pure (+) <*> Just 5 <*> Just 7 -- Just 12
-- lifting a BINARY function


-- lifting a TERNARY function
pure (\x y z -> x * y ^ z) <*> Just 4 <*> Just 3 <*> Just 2 -- Just 36

-- applicative form:
pure f <*> x <*> y <*> z ...

pure succ <*> Just 4            -- Just 5
Just succ <*> Just 4            -- Just 5
pure succ <*> [4]               -- [ 5 ]
[succ]    <*> [4]               -- [ 5 ]
[succ]    <*> [4,5]             -- [ 5, 6 ]
[(+1), (+2), (+3)] <*> [4,6]    -- [5,7, 6,8, 7,9]

-- Composing functors
ffmap = fmap . fmap == fmap fmap fmap
ffmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)

ffmap succ (Just [1]) -- Just [2]
```


## Monads

Monadic map

The added benefit of a monad over a functor mainly lies in letting `f` produce a value in a monadic type.

```hs
x :: [a]
f :: a -> [b]
-- then
map f :: [a] -> [[b]]
map f x :: [[b]]

-- so we need to flatten the m m a (e.g. list-of-lists)
(>>=) :: m a -> (a -> m b) -> m b
(>>=) @[] :: [a] -> (a -> [b]) -> [b]
xs >>= k = concat $ map k xs


-- functorial (functoric?) map
fmap :: Functor f => (a -> b) -> f a -> f b

-- monadic map
mapM :: (Traversable t, Monad m)
     => (a -> m b)
     -> t a
     -> m (t b)

-- [] as Traversable
mapM @[]        :: Monad m  => (a -> m b) -> [a] -> m [b]
-- [] as Traversable, [] as Monad
mapM @[] @[]    :: (a -> [b]) -> [a] -> [[b]]
-- [] as Traversable, Maybe as Monad
mapM @[] @Maybe :: (a -> Maybe b) -> [a] -> Maybe [b]
-- Maybe as Traversable
mapM @Maybe     :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
-- Maybe as Traversable, [] as Monad
mapM @Maybe @[] :: (a -> [b]) -> Maybe a -> [Maybe b]
-- Maybe as Traversable, Maybe as Monad
mapM @Maybe @Maybe :: (a -> Maybe b) -> Maybe a -> Maybe (Maybe b)

mapM @[] :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do
  y  <- f x
  ys <- mapM f xs
  return (y:ys)
```
