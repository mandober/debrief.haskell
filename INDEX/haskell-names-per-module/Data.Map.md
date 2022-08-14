# Data.Map

```hs
import Data.Map (Map)
import Data.Map qualified as M

-- M.<TAB>
-- Display all 132 possibilities? (y or n)

M.!
M.!?
M.Map
M.\\
M.adjust
M.adjustWithKey
M.alter
M.alterF
M.assocs
M.compose
M.delete
M.deleteAt
M.deleteFindMax
M.deleteFindMin
M.deleteMax
M.deleteMin
M.difference
M.differenceWith
M.differenceWithKey
M.disjoint
M.drop
M.dropWhileAntitone
M.elemAt
M.elems
M.empty
M.filter
M.filterWithKey
M.findIndex
M.findMax
M.findMin
M.findWithDefault
M.fold
M.foldMapWithKey
M.foldWithKey
M.foldl
M.foldl'
M.foldlWithKey
M.foldlWithKey'
M.foldr
M.foldr'
M.foldrWithKey
M.foldrWithKey'
M.fromAscList
M.fromAscListWith
M.fromAscListWithKey
M.fromDescList
M.fromDescListWith
M.fromDescListWithKey
M.fromDistinctAscList
M.fromDistinctDescList
M.fromList
M.fromListWith
M.fromListWithKey
M.fromSet
M.insert
M.insertLookupWithKey
M.insertLookupWithKey'
M.insertWith
M.insertWith'
M.insertWithKey
M.insertWithKey'
M.intersection
M.intersectionWith
M.intersectionWithKey
M.isProperSubmapOf
M.isProperSubmapOfBy
M.isSubmapOf
M.isSubmapOfBy
M.keys
M.keysSet
M.lookup
M.lookupGE
M.lookupGT
M.lookupIndex
M.lookupLE
M.lookupLT
M.lookupMax
M.lookupMin
M.map
M.mapAccum
M.mapAccumRWithKey
M.mapAccumWithKey
M.mapEither
M.mapEitherWithKey
M.mapKeys
M.mapKeysMonotonic
M.mapKeysWith
M.mapMaybe
M.mapMaybeWithKey
M.mapWithKey
M.maxView
M.maxViewWithKey
M.member
M.mergeWithKey
M.minView
M.minViewWithKey
M.notMember
M.null
M.partition
M.partitionWithKey
M.restrictKeys
M.showTree
M.showTreeWith
M.singleton
M.size
M.spanAntitone
M.split
M.splitAt
M.splitLookup
M.splitRoot
M.take
M.takeWhileAntitone
M.toAscList
M.toDescList
M.toList
M.traverseMaybeWithKey
M.traverseWithKey
M.union
M.unionWith
M.unionWithKey
M.unions
M.unionsWith
M.update
M.updateAt
M.updateLookupWithKey
M.updateMax
M.updateMaxWithKey
M.updateMin
M.updateMinWithKey
M.updateWithKey
M.valid
M.withoutKeys

M.update              :: Ord k =>
  (a -> Maybe a)      -> k   -> Map k a -> Map k a

M.updateWithKey       :: Ord k =>
  (k -> a -> Maybe a) -> k   -> Map k a -> Map k a

M.updateAt            ::
  (k -> a -> Maybe a) -> Int -> Map k a -> Map k a

M.updateLookupWithKey :: Ord k =>
  (k -> a -> Maybe a) -> k   -> Map k a -> (Maybe a, Map k a)


M.fromAscList     :: Eq k =>                  [(k, a)] -> Map k a
M.fromAscListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> Map k a

-- fromAscListWith: build a map from an ascending list in linear time with a combining function for equal keys.

M.insert :: Ord k => k -> a -> Map k a -> Map k a
```
