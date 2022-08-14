# Data.List.NonEmpty

https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List-NonEmpty.html

A `NonEmpty` list is one which always has at least one element, but is otherwise identical to the traditional list type in complexity and in terms of API. You will almost certainly want to import this module qualified.


*OverloadedLists* enable using the list syntax for any IsList type

```hs
ne :: NonEmpty Int
ne = [42]
```

The functions `toList`, `fromList` and `fromListN` are all methods of the `IsList` class, which is itself exported from the `GHC.Exts` module.


```hs
import Data.List.NonEmpty

type NonEmpty :: * -> *
data NonEmpty a = a :| [a]

(<|) :: a -> NonEmpty a -> NonEmpty a
cons :: a -> NonEmpty a -> NonEmpty a

fromList :: [a] -> NonEmpty a
toList   :: NonEmpty a -> [a]

(!!)   :: NonEmpty a -> Int -> a
length :: NonEmpty a -> Int

head :: NonEmpty a -> a
tail :: NonEmpty a -> [a]
init :: NonEmpty a -> [a]
last :: NonEmpty a -> a

map  :: (a -> b) -> NonEmpty a -> NonEmpty b

inits :: Foldable f => f a -> NonEmpty [a]
tails :: Foldable f => f a -> NonEmpty [a]

nonEmpty :: [a] -> Maybe (NonEmpty a)

break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
cycle :: NonEmpty a -> NonEmpty a
drop :: Int -> NonEmpty a -> [a]
dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
filter :: (a -> Bool) -> NonEmpty a -> [a]

group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group1 :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith1 ::Ord b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith1 :: Eq b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)

insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
intersperse :: a -> NonEmpty a -> NonEmpty a
isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
iterate :: (a -> a) -> a -> NonEmpty a
nub :: Eq a => NonEmpty a -> NonEmpty a
nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
repeat :: a -> NonEmpty a
reverse :: NonEmpty a -> NonEmpty a

scanl ::Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b
scanl1 ::(a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr ::Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b
scanr1 ::(a -> a -> a) -> NonEmpty a -> NonEmpty a

some1 :: GHC.Base.Alternative f => f a -> f (NonEmpty a)
sort :: Ord a => NonEmpty a -> NonEmpty a
sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a
span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
splitAt :: Int -> NonEmpty a -> ([a], [a])

take :: Int -> NonEmpty a -> [a]
takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unzip :: Functor f => f (a, b) -> (f a, f b)
xor :: NonEmpty Bool -> Bool
zip ::NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zipWith ::(a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
```
