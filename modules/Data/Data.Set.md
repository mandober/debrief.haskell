# Data.Set

## Data.Set as S

```hs
import Data.Set qualified as S

S.Set                   S.foldl                 S.member
S.\\                    S.foldl'                S.minView
S.alterF                S.foldr                 S.notMember
S.cartesianProduct      S.foldr'                S.null
S.delete                S.fromAscList           S.partition
S.deleteAt              S.fromDescList          S.powerSet
S.deleteFindMax         S.fromDistinctAscList   S.showTree
S.deleteFindMin         S.fromDistinctDescList  S.showTreeWith
S.deleteMax             S.fromList              S.singleton
S.deleteMin             S.insert                S.size
S.difference            S.intersection          S.spanAntitone
S.disjoint              S.isProperSubsetOf      S.split
S.disjointUnion         S.isSubsetOf            S.splitAt
S.drop                  S.lookupGE              S.splitMember
S.dropWhileAntitone     S.lookupGT              S.splitRoot
S.elemAt                S.lookupIndex           S.take
S.elems                 S.lookupLE              S.takeWhileAntitone
S.empty                 S.lookupLT              S.toAscList
S.filter                S.lookupMax             S.toDescList
S.findIndex             S.lookupMin             S.toList
S.findMax               S.map                   S.union
S.findMin               S.mapMonotonic          S.unions
S.fold                  S.maxView               S.valid
```

## Data.Set

```hs
import Data.Set

-- imported via Data.Set
type role Set nominal
data Set a = ...
(\\) :: Ord a => Set a -> Set a -> Set a
cartesianProduct :: Set a -> Set b -> Set (a, b)
delete :: Ord a => a -> Set a -> Set a
deleteAt :: Int -> Set a -> Set a
deleteFindMax :: Set a -> (a, Set a)
deleteFindMin :: Set a -> (a, Set a)
deleteMax :: Set a -> Set a
deleteMin :: Set a -> Set a
difference :: Ord a => Set a -> Set a -> Set a
disjoint :: Ord a => Set a -> Set a -> Bool
disjointUnion :: Set a -> Set b -> Set (Either a b)
Data.Set.drop :: Int -> Set a -> Set a
dropWhileAntitone :: (a -> Bool) -> Set a -> Set a
elemAt :: Int -> Set a -> a
elems :: Set a -> [a]
empty :: Set a
Data.Set.filter :: (a -> Bool) -> Set a -> Set a
findIndex :: Ord a => a -> Set a -> Int
findMax :: Set a -> a
findMin :: Set a -> a
fold :: (a -> b -> b) -> b -> Set a -> b
Data.Set.foldl :: (a -> b -> a) -> a -> Set b -> a
foldl' :: (a -> b -> a) -> a -> Set b -> a
Data.Set.foldr :: (a -> b -> b) -> b -> Set a -> b
foldr' :: (a -> b -> b) -> b -> Set a -> b
fromAscList :: Eq a => [a] -> Set a
fromDescList :: Eq a => [a] -> Set a
fromDistinctAscList :: [a] -> Set a
fromDistinctDescList :: [a] -> Set a
fromList :: Ord a => [a] -> Set a
insert :: Ord a => a -> Set a -> Set a
intersection :: Ord a => Set a -> Set a -> Set a
isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf :: Ord a => Set a -> Set a -> Bool
lookupGE :: Ord a => a -> Set a -> Maybe a
lookupGT :: Ord a => a -> Set a -> Maybe a
lookupIndex :: Ord a => a -> Set a -> Maybe Int
lookupLE :: Ord a => a -> Set a -> Maybe a
lookupLT :: Ord a => a -> Set a -> Maybe a
lookupMax :: Set a -> Maybe a
lookupMin :: Set a -> Maybe a
Data.Set.map :: Ord b => (a -> b) -> Set a -> Set b
mapMonotonic :: (a -> b) -> Set a -> Set b
maxView :: Set a -> Maybe (a, Set a)
member :: Ord a => a -> Set a -> Bool
minView :: Set a -> Maybe (a, Set a)
notMember :: Ord a => a -> Set a -> Bool
Data.Set.null :: Set a -> Bool
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
powerSet :: Set a -> Set (Set a)
showTree :: Show a => Set a -> String
showTreeWith :: Show a => Bool -> Bool -> Set a -> String
singleton :: a -> Set a
size :: Set a -> Int
spanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
split :: Ord a => a -> Set a -> (Set a, Set a)
Data.Set.splitAt :: Int -> Set a -> (Set a, Set a)
splitMember :: Ord a => a -> Set a -> (Set a, Bool, Set a)
splitRoot :: Set a -> [Set a]
Data.Set.take :: Int -> Set a -> Set a
takeWhileAntitone :: (a -> Bool) -> Set a -> Set a
toAscList :: Set a -> [a]
toDescList :: Set a -> [a]
toList :: Set a -> [a]
union :: Ord a => Set a -> Set a -> Set a
unions :: (Foldable f, Ord a) => f (Set a) -> Set a
valid :: Ord a => Set a -> Bool
```
