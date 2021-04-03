# Data.Map

https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Map.html
https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Map-Strict.html


```hs
import Data.Map

-- imported via Data.Map
fold ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "Data.Map.fold is gone. Use foldr." =>
  (a -> b -> b) -> b -> Map k a -> b
foldWithKey ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "Data.Map.foldWithKey is gone. Use foldrWithKey." =>
  (k -> a -> b -> b) -> b -> Map k a -> b
insertLookupWithKey' ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "Data.Map.insertLookupWithKey' is gone. Use Data.Map.Strict.insertLookupWithKey." =>
  (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertWith' ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "Data.Map.insertWith' is gone. Use Data.Map.Strict.insertWith." =>
  (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "Data.Map.insertWithKey' is gone. Use Data.Map.Strict.insertWithKey." =>
  (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
(!) :: Ord k => Map k a -> k -> a
(!?) :: Ord k => Map k a -> k -> Maybe a
type role Map nominal representational
data Map k a = ...
(\\) :: Ord k => Map k a -> Map k b -> Map k a
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alterF ::
  (Functor f, Ord k) =>
  (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
assocs :: Map k a -> [(k, a)]
delete :: Ord k => k -> Map k a -> Map k a
deleteAt :: Int -> Map k a -> Map k a
deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteMax :: Map k a -> Map k a
deleteMin :: Map k a -> Map k a
difference :: Ord k => Map k a -> Map k b -> Map k a
differenceWith ::
  Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey ::
  Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
disjoint :: Ord k => Map k a -> Map k b -> Bool
Data.Map.drop :: Int -> Map k a -> Map k a
dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
elemAt :: Int -> Map k a -> (k, a)
elems :: Map k a -> [a]
empty :: Map k a
Data.Map.filter :: (a -> Bool) -> Map k a -> Map k a
filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
findIndex :: Ord k => k -> Map k a -> Int
findMax :: Map k a -> (k, a)
findMin :: Map k a -> (k, a)
findWithDefault :: Ord k => a -> k -> Map k a -> a
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
Data.Map.foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
Data.Map.foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
fromAscList :: Eq k => [(k, a)] -> Map k a
fromAscListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWithKey ::
  Eq k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromDescList :: Eq k => [(k, a)] -> Map k a
fromDescListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWithKey ::
  Eq k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctDescList :: [(k, a)] -> Map k a
fromList :: Ord k => [(k, a)] -> Map k a
fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey ::
  Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromSet :: (k -> a) -> Data.Set.Internal.Set k -> Map k a
insert :: Ord k => k -> a -> Map k a -> Map k a
insertLookupWithKey ::
  Ord k =>
  (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertWith ::
  Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey ::
  Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
intersection :: Ord k => Map k a -> Map k b -> Map k a
intersectionWith ::
  Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey ::
  Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOfBy ::
  Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOfBy ::
  Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
keys :: Map k a -> [k]
keysSet :: Map k a -> Data.Set.Internal.Set k
Data.Map.lookup :: Ord k => k -> Map k a -> Maybe a
lookupGE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupIndex :: Ord k => k -> Map k a -> Maybe Int
lookupLE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupMax :: Map k a -> Maybe (k, a)
lookupMin :: Map k a -> Maybe (k, a)
Data.Map.map :: (a -> b) -> Map k a -> Map k b
mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey ::
  (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey ::
  (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey ::
  (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith ::
  Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
maxView :: Map k a -> Maybe (a, Map k a)
maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
member :: Ord k => k -> Map k a -> Bool
mergeWithKey ::
  Ord k =>
  (k -> a -> b -> Maybe c)
  -> (Map k a -> Map k c)
  -> (Map k b -> Map k c)
  -> Map k a
  -> Map k b
  -> Map k c
minView :: Map k a -> Maybe (a, Map k a)
minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
notMember :: Ord k => k -> Map k a -> Bool
Data.Map.null :: Map k a -> Bool
partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)

partitionWithKey ::
  (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)

restrictKeys ::
  Ord k => Map k a -> Data.Set.Internal.Set k -> Map k a

showTree ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "showTree has moved to Data.Map.Internal.Debug.showTree." =>
  Map k a -> String

showTreeWith ::
  containers-0.6.2.1:Utils.Containers.Internal.TypeError.Whoops
    "showTreeWith has moved to Data.Map.Internal.Debug.showTreeWith." =>
  (k -> a -> String) -> Bool -> Bool -> Map k a -> String

singleton :: k -> a -> Map k a
size :: Map k a -> Int
spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
split :: Ord k => k -> Map k a -> (Map k a, Map k a)
Data.Map.splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitLookup :: Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitRoot :: Map k b -> [Map k b]
Data.Map.take :: Int -> Map k a -> Map k a
takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a

toAscList :: Map k a -> [(k, a)]
toDescList :: Map k a -> [(k, a)]
toList :: Map k a -> [(k, a)]

traverseMaybeWithKey ::
  Applicative f => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseWithKey ::
  Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)

union :: Ord k => Map k a -> Map k a -> Map k a
unionWith ::
  Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey ::
  Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unions :: (Foldable f, Ord k) => f (Map k a) -> Map k a
unionsWith ::
  (Foldable f, Ord k) => (a -> a -> a) -> f (Map k a) -> Map k a

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateLookupWithKey ::
  Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateWithKey ::
  Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a

valid :: Ord k => Map k a -> Bool

withoutKeys :: Ord k => Map k a -> Data.Set.Internal.Set k -> Map k a
```
