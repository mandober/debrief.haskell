# Functions and their signatures

Significant functions and their signatures in order of appearance:


Monadic values:
- IO, [], Maybe, Either e, functions, Monad ((,) a)
- functions, `(->) r`, are functors too

- instance Monad (Either e)           [Data.Either][Data.Either]
- instance Monad []                   [GHC.Base][GHC.Base]
- instance Monad Maybe                [GHC.Base][GHC.Base]
- instance Monad IO                   [GHC.Base][GHC.Base]
- instance Monad ((->) r)             [GHC.Base][GHC.Base]
- instance Monoid a => Monad ((,) a)  [GHC.Base][GHC.Base]


## fmap

```hs
    fmap  :: Functor f => (a -> b) -> f a -> f b
flip fmap :: Functor f => f a -> (a -> b) -> f b
    (<$>) :: Functor f => (a -> b) -> f a -> f b

    fmap :: (a -> b) -> IO a -> IO b
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap :: (a -> b) -> [a] -> [b]

ff = fmap (+3) (*2)
x = ff 5 -- (+3) (*2) 5 = 13

instance Functor ((->) r) where
  fmap f g = f . g

(+) <$> (Just 5)          -- Just (+5)
Just (+5) <$> (Just 4)    -- ERROR
Just (+5) <*> (Just 4)    -- Just 9
(*) <$> Just 5 <*> Just 3 -- Just 15

-- There's a function called liftA2 that does the same thing:
liftA2 (*) (Just 5) (Just 3) -- Just 15
```

Functions, `(->) r`, are functors too.

When you use fmap on a function, you're just doing function composition!


## List

```hs
data [] a = a : [a]
```

Data.List.isSubsequenceOf :: Eq a => [a] -> [a] -> Bool

(!!) :: [a] -> Int -> a
(++) :: [a] -> [a] -> [a]
(base-4.12.0.0:Data.OldList.\\) :: Eq a => [a] -> [a] -> [a]
all :: Foldable t => (a -> Bool) -> t a -> Bool
and :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
break :: (a -> Bool) -> [a] -> ([a], [a])
concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
cycle :: [a] -> [a]
base-4.12.0.0:Data.OldList.delete :: Eq a => a -> [a] -> [a]
base-4.12.0.0:Data.OldList.deleteBy ::
  (a -> a -> Bool) -> a -> [a] -> [a]
base-4.12.0.0:Data.OldList.deleteFirstsBy ::
  (a -> a -> Bool) -> [a] -> [a] -> [a]
drop :: Int -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
base-4.12.0.0:Data.OldList.dropWhileEnd ::
  (a -> Bool) -> [a] -> [a]
class Foldable (t :: * -> *) where
  ...
  elem :: Eq a => a -> t a -> Bool
  ...
base-4.12.0.0:Data.OldList.elemIndex ::
  Eq a => a -> [a] -> Maybe Int
base-4.12.0.0:Data.OldList.elemIndices :: Eq a => a -> [a] -> [Int]
filter :: (a -> Bool) -> [a] -> [a]
Data.Foldable.find :: Foldable t => (a -> Bool) -> t a -> Maybe a
base-4.12.0.0:Data.OldList.findIndex ::
  (a -> Bool) -> [a] -> Maybe Int
base-4.12.0.0:Data.OldList.findIndices ::
  (a -> Bool) -> [a] -> [Int]
class Foldable (t :: * -> *) where
  ...
  foldl :: (b -> a -> b) -> b -> t a -> b
  ...
class Foldable (t :: * -> *) where
  ...
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
  ...
class Foldable (t :: * -> *) where
  ...
  foldl1 :: (a -> a -> a) -> t a -> a
  ...
GHC.List.foldl1' :: (a -> a -> a) -> [a] -> a
class Foldable (t :: * -> *) where
  ...
  foldr :: (a -> b -> b) -> b -> t a -> b
  ...
class Foldable (t :: * -> *) where
  ...
  foldr1 :: (a -> a -> a) -> t a -> a
  ...
base-4.12.0.0:Data.OldList.genericDrop ::
  Integral i => i -> [a] -> [a]
base-4.12.0.0:Data.OldList.genericIndex ::
  Integral i => [a] -> i -> a
base-4.12.0.0:Data.OldList.genericLength :: Num i => [a] -> i
base-4.12.0.0:Data.OldList.genericReplicate ::
  Integral i => i -> a -> [a]
base-4.12.0.0:Data.OldList.genericSplitAt ::
  Integral i => i -> [a] -> ([a], [a])
base-4.12.0.0:Data.OldList.genericTake ::
  Integral i => i -> [a] -> [a]
base-4.12.0.0:Data.OldList.group :: Eq a => [a] -> [[a]]
base-4.12.0.0:Data.OldList.groupBy ::
  (a -> a -> Bool) -> [a] -> [[a]]
head :: [a] -> a
init :: [a] -> [a]
base-4.12.0.0:Data.OldList.inits :: [a] -> [[a]]
base-4.12.0.0:Data.OldList.insert :: Ord a => a -> [a] -> [a]
base-4.12.0.0:Data.OldList.insertBy ::
  (a -> a -> Ordering) -> a -> [a] -> [a]
base-4.12.0.0:Data.OldList.intercalate :: [a] -> [[a]] -> [a]
base-4.12.0.0:Data.OldList.intersect :: Eq a => [a] -> [a] -> [a]
base-4.12.0.0:Data.OldList.intersectBy ::
  (a -> a -> Bool) -> [a] -> [a] -> [a]
base-4.12.0.0:Data.OldList.intersperse :: a -> [a] -> [a]
base-4.12.0.0:Data.OldList.isInfixOf :: Eq a => [a] -> [a] -> Bool
base-4.12.0.0:Data.OldList.isPrefixOf :: Eq a => [a] -> [a] -> Bool
base-4.12.0.0:Data.OldList.isSuffixOf :: Eq a => [a] -> [a] -> Bool
iterate :: (a -> a) -> a -> [a]
GHC.List.iterate' :: (a -> a) -> a -> [a]
last :: [a] -> a
class Foldable (t :: * -> *) where
  ...
  length :: t a -> Int
  ...
lines :: String -> [String]
lookup :: Eq a => a -> [(a, b)] -> Maybe b
map :: (a -> b) -> [a] -> [b]
Data.Traversable.mapAccumL ::
  Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
Data.Traversable.mapAccumR ::
  Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
class Foldable (t :: * -> *) where
  ...
  maximum :: Ord a => t a -> a
  ...
Data.Foldable.maximumBy ::
  Foldable t => (a -> a -> Ordering) -> t a -> a
class Foldable (t :: * -> *) where
  ...
  minimum :: Ord a => t a -> a
  ...
Data.Foldable.minimumBy ::
  Foldable t => (a -> a -> Ordering) -> t a -> a
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
base-4.12.0.0:Data.OldList.nub :: Eq a => [a] -> [a]
base-4.12.0.0:Data.OldList.nubBy :: (a -> a -> Bool) -> [a] -> [a]
class Foldable (t :: * -> *) where
  ...
  null :: t a -> Bool
  ...
or :: Foldable t => t Bool -> Bool
base-4.12.0.0:Data.OldList.partition ::
  (a -> Bool) -> [a] -> ([a], [a])
base-4.12.0.0:Data.OldList.permutations :: [a] -> [[a]]
class Foldable (t :: * -> *) where
  ...
  product :: Num a => t a -> a
repeat :: a -> [a]
replicate :: Int -> a -> [a]
reverse :: [a] -> [a]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
GHC.List.scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]
base-4.12.0.0:Data.OldList.sort :: Ord a => [a] -> [a]
base-4.12.0.0:Data.OldList.sortBy ::
  (a -> a -> Ordering) -> [a] -> [a]
base-4.12.0.0:Data.OldList.sortOn ::
  Ord b => (a -> b) -> [a] -> [a]
span :: (a -> Bool) -> [a] -> ([a], [a])
splitAt :: Int -> [a] -> ([a], [a])
base-4.12.0.0:Data.OldList.stripPrefix ::
  Eq a => [a] -> [a] -> Maybe [a]
base-4.12.0.0:Data.OldList.subsequences :: [a] -> [[a]]
class Foldable (t :: * -> *) where
  ...
  sum :: Num a => t a -> a
  ...
tail :: [a] -> [a]
base-4.12.0.0:Data.OldList.tails :: [a] -> [[a]]
take :: Int -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]
base-4.12.0.0:Data.OldList.transpose :: [[a]] -> [[a]]
GHC.List.uncons :: [a] -> Maybe (a, [a])
base-4.12.0.0:Data.OldList.unfoldr ::
  (b -> Maybe (a, b)) -> b -> [a]
base-4.12.0.0:Data.OldList.union :: Eq a => [a] -> [a] -> [a]
base-4.12.0.0:Data.OldList.unionBy ::
  (a -> a -> Bool) -> [a] -> [a] -> [a]
unlines :: [String] -> String
unwords :: [String] -> String
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
base-4.12.0.0:Data.OldList.unzip4 ::
  [(a, b, c, d)] -> ([a], [b], [c], [d])
base-4.12.0.0:Data.OldList.unzip5 ::
  [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
base-4.12.0.0:Data.OldList.unzip6 ::
  [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
base-4.12.0.0:Data.OldList.unzip7 ::
  [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
words :: String -> [String]
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
base-4.12.0.0:Data.OldList.zip4 ::
  [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
base-4.12.0.0:Data.OldList.zip5 ::
  [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
base-4.12.0.0:Data.OldList.zip6 ::
  [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
base-4.12.0.0:Data.OldList.zip7 ::
  [a]
  -> [b]
  -> [c]
  -> [d]
  -> [e]
  -> [f]
  -> [g]
  -> [(a, b, c, d, e, f, g)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
base-4.12.0.0:Data.OldList.zipWith4 ::
  (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
base-4.12.0.0:Data.OldList.zipWith5 ::
  (a -> b -> c -> d -> e -> f)
  -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
base-4.12.0.0:Data.OldList.zipWith6 ::
  (a -> b -> c -> d -> e -> f -> g)
  -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
base-4.12.0.0:Data.OldList.zipWith7 ::
  (a -> b -> c -> d -> e -> f -> g -> h)
  -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]



---

[GHC.Base]: ../../../haskell/vendor/ghc-source/ghc-8.6.5/libraries/base/GHC/Base.hs

[Data.Either]: ../../../haskell/vendor/ghc-source/ghc-8.6.5/libraries/base/Data/Either.hs

[Data.Maybe]: ../../../haskell/vendor/ghc-source/ghc-8.6.5/libraries/base/Data/Maybe.hs


GHC.Maybe
GHC.CString
GHC.Magic
GHC.Types
GHC.Err


GHC.Prim
Has no implementation. It defines built-in things, and by importing it you bring them into scope. The source file is GHC.Prim.hi-boot, which is just copied to make GHC.Prim.hi
