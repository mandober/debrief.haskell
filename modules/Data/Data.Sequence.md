# Data.Sequence

https://hackage.haskell.org/package/containers/docs/Data-Sequence.html


```hs
import Data.Sequence


(<|) :: a -> Seq a -> Seq a
(|>) :: Seq a -> a -> Seq a

(><) :: Seq a -> Seq a -> Seq a
(!?) :: Seq a -> Int -> Maybe a

(:<) :: a -> Seq a -> ViewL a
(:>) :: Seq a -> a -> ViewR a

pattern (:<|) :: a -> Seq a -> Seq a
pattern (:|>) :: Seq a -> a -> Seq a
pattern Empty :: Seq a

EmptyL :: ViewL a
EmptyR :: ViewR a

newtype Seq a = Data.Sequence.Internal.Seq
               (Data.Sequence.Internal.FingerTree
               (Data.Sequence.Internal.Elem a))

data ViewL a = EmptyL | a :< (Seq a)
data ViewR a = EmptyR | (Seq a) :> a


empty :: Seq a

adjust           :: (a -> a) -> Int -> Seq a -> Seq a
adjust'          :: (a -> a) -> Int -> Seq a -> Seq a

breakl           :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakr           :: (a -> Bool) -> Seq a -> (Seq a, Seq a)

chunksOf         :: Int -> Seq a -> Seq (Seq a)
cycleTaking      :: Int -> Seq a -> Seq a
deleteAt         :: Int -> Seq a -> Seq a

dropWhileL       :: (a -> Bool) -> Seq a -> Seq a
dropWhileR       :: (a -> Bool) -> Seq a -> Seq a

elemIndexL       :: Eq a => a -> Seq a -> Maybe Int
elemIndexR       :: Eq a => a -> Seq a -> Maybe Int
elemIndicesL     :: Eq a => a -> Seq a -> [Int]
elemIndicesR     :: Eq a => a -> Seq a -> [Int]

findIndexL       :: (a -> Bool) -> Seq a -> Maybe Int
findIndexR       :: (a -> Bool) -> Seq a -> Maybe Int
findIndicesL     :: (a -> Bool) -> Seq a -> [Int]
findIndicesR     :: (a -> Bool) -> Seq a -> [Int]

foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Seq a -> m
foldlWithIndex   :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldrWithIndex   :: (Int -> a -> b -> b) -> b -> Seq a -> b

fromArray        :: GHC.Arr.Ix i => GHC.Arr.Array i a -> Seq a
fromFunction     :: Int -> (Int -> a) -> Seq a
fromList         :: [a] -> Seq a

tails             :: Seq a -> Seq (Seq a)
inits            :: Seq a -> Seq (Seq a)

index            :: Seq a -> Int -> a
insertAt         :: Int -> a -> Seq a -> Seq a
intersperse      :: a -> Seq a -> Seq a
iterateN         :: Int -> (a -> a) -> a -> Seq a
mapWithIndex     :: (Int -> a -> b) -> Seq a -> Seq b
partition        :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
singleton         :: a -> Seq a

replicateA       :: Applicative f => Int -> f a -> f (Seq a)
replicateM       :: Applicative m => Int -> m a -> m (Seq a)

sort              :: Ord a => Seq a -> Seq a
sortBy            :: (a -> a -> Ordering) -> Seq a -> Seq a
sortOn            :: Ord b => (a -> b) -> Seq a -> Seq a
unstableSort      :: Ord a => Seq a -> Seq a
unstableSortBy    :: (a -> a -> Ordering) -> Seq a -> Seq a
unstableSortOn    :: Ord b => (a -> b) -> Seq a -> Seq a

spanl             :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanr             :: (a -> Bool) -> Seq a -> (Seq a, Seq a)

takeWhileL        :: (a -> Bool) -> Seq a -> Seq a
takeWhileR        :: (a -> Bool) -> Seq a -> Seq a

traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Seq a -> f (Seq b)

unfoldl           :: (b -> Maybe (b, a)) -> b -> Seq a
unfoldr           :: (b -> Maybe (a, b)) -> b -> Seq a

update            :: Int -> a -> Seq a -> Seq a

viewl             :: Seq a -> ViewL a
viewr             :: Seq a -> ViewR a

unzipWith         :: (a -> (b, c)) -> Seq a -> (Seq b, Seq c)
zip4              :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a, b, c, d)
zipWith4          :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e


Data.Sequence.drop      :: Int -> Seq a -> Seq a
Data.Sequence.filter    :: (a -> Bool) -> Seq a -> Seq a
Data.Sequence.length    :: Seq a -> Int
Data.Sequence.lookup    :: Int -> Seq a -> Maybe a
Data.Sequence.null      :: Seq a -> Bool
Data.Sequence.replicate :: Int -> a -> Seq a
Data.Sequence.reverse   :: Seq a -> Seq a
Data.Sequence.scanl     :: (a -> b -> a) -> a -> Seq b -> Seq a
Data.Sequence.scanl1    :: (a -> a -> a) -> Seq a -> Seq a
Data.Sequence.scanr     :: (a -> b -> b) -> b -> Seq a -> Seq b
Data.Sequence.scanr1    :: (a -> a -> a) -> Seq a -> Seq a
Data.Sequence.splitAt   :: Int -> Seq a -> (Seq a, Seq a)
Data.Sequence.take      :: Int -> Seq a -> Seq a
Data.Sequence.unzip     :: Seq (a, b) -> (Seq a, Seq b)
Data.Sequence.zip       :: Seq a -> Seq b -> Seq (a, b)
Data.Sequence.zip3      :: Seq a -> Seq b -> Seq c -> Seq (a, b, c)
Data.Sequence.zipWith   :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
Data.Sequence.zipWith3  :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
```
