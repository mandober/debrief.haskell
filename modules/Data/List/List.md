# Data.List

- module: `Data.List` in package `base`
- related: Data.OldList (hidden module in base package)
- latest version: `base-4.17.0.0`
- docs:
  - http://hackage.haskell.org/package/base/docs/Data-List.html
  - https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html
- src:
  - https://hackage.haskell.org/package/base/docs/src/Data.List.html
  - https://hackage.haskell.org/package/base/docs/src/Data.OldList.html
  - https://hackage.haskell.org/package/base/docs/src/Data.Foldable.html
  - https://hackage.haskell.org/package/base/docs/src/Data.Traversable.html
  - https://hackage.haskell.org/package/base/docs/src/GHC.List.html
  - https://hackage.haskell.org/package/base/docs/src/GHC.Base.html

## Contents




## List functions signatures
<!-- #region sigs -->

```hs
-- | Basic functions
head   :: [a] -> a                                                -- p
last   :: [a] -> a                                                -- p
tail   :: [a] -> [a]                                              -- p
init   :: [a] -> [a]                                              -- p

(++)   :: [a] -> [a] -> [a]                                       -- t
uncons :: [a] -> Maybe (a, [a])                                   -- t
null   :: Foldable t => t a -> Bool                               -- t
length :: Foldable t => t a -> Int                                -- t


-- | List transformations
map          :: (a -> b) -> [a] -> [b]                            -- T

reverse      :: [a] -> [a]                                        -- 
intersperse  :: a -> [a] -> [a]                                   -- 
intercalate  :: [a] -> [[a]] -> [a]                               -- 
subsequences :: [a] -> [[a]]                                      -- 
permutations :: [a] -> [[a]]                                      -- 
transpose    :: [[a]] -> [[a]]                                    -- 


-- | Folding lists
foldl   :: Foldable t => (b -> a -> b) -> b -> t a -> b      -- ~ acc
foldr   :: Foldable t => (a -> b -> b) -> b -> t a -> b      -- ~

foldl1  :: Foldable t => (a -> a -> a) -> t a -> a           --   (M)
foldr1  :: Foldable t => (a -> a -> a) -> t a -> a           --   (M)

foldl'  :: Foldable t => (b -> a -> b) -> b -> t a -> b      -- !
foldl1' ::               (a -> a -> a) -> [a] -> a           -- ! (M)

-- | Building lists: Scans
scanr  :: (a -> b -> b) -> b -> [a] -> [b]                     -- ~
scanl  :: (b -> a -> b) -> b -> [a] -> [b]                     -- ~

scanl' :: (b -> a -> b) -> b -> [a] -> [b]                     -- !

scanl1 :: (a -> a -> a) -> [a] -> [a]                          -- (M)
scanr1 :: (a -> a -> a) -> [a] -> [a]                          -- (M)

-- | Building lists: Accumulating maps
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

-- | Folding lists: Special folds
concat    :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

and :: Foldable t => t Bool -> Bool
or  :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool
sum :: (Foldable t, Num a) => t a -> a

product :: (Foldable t, Num a) => t a -> a
maximum :: forall a. (Foldable t, Ord a) => t a -> a
minimum :: forall a. (Foldable t, Ord a) => t a -> a

-- | Building infinite lists
iterate   :: (a -> a) -> a -> [a]
iterate'  :: (a -> a) -> a -> [a]
repeat    :: a -> [a]
replicate :: Int -> a -> [a]
cycle     :: [a] -> [a]

-- | Building lists: Unfolding
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]


-- | Sublists: Extracting sublists
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
span :: (a -> Bool) -> [a] -> ([a], [a])
break :: (a -> Bool) -> [a] -> ([a], [a])
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
group :: Eq a => [a] -> [[a]]
inits :: [a] -> [[a]]
tails :: [a] -> [[a]]

-- Sublists: Predicates
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool

-- Searching lists: Search by equality
elem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- Searching lists: Search with a predicate
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
filter :: (a -> Bool) -> [a] -> [a]
partition :: (a -> Bool) -> [a] -> ([a], [a])

-- Indexing lists
(!!) :: [a] -> Int -> a
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndices :: Eq a => a -> [a] -> [Int]
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndices :: (a -> Bool) -> [a] -> [Int]

-- Zipping and unzipping lists
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])

-- Special lists: Functions on strings
lines :: String -> [String]
words :: String -> [String]
unlines :: [String] -> String
unwords :: [String] -> String

-- Special lists: Set operations
nub :: Eq a => [a] -> [a]
delete :: Eq a => a -> [a] -> [a]
(\\) :: Eq a => [a] -> [a] -> [a]
union :: Eq a => [a] -> [a] -> [a]
intersect :: Eq a => [a] -> [a] -> [a]

-- Special lists: Ordered lists
sort :: Ord a => [a] -> [a]
sortOn :: Ord b => (a -> b) -> [a] -> [a]
insert :: Ord a => a -> [a] -> [a]

-- Generalized functions: The "By" operations
nubBy :: (a -> a -> Bool) -> [a] -> [a]
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- Generalized functions: User-supplied comparison
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a

-- Generalized functions: generic operations
genericLength :: Num i => [a] -> i
genericTake :: Integral i => i -> [a] -> [a]
genericDrop :: Integral i => i -> [a] -> [a]
genericSplitAt :: Integral i => i -> [a] -> ([a], [a])
genericIndex :: Integral i => [a] -> i -> a
genericReplicate :: Integral i => i -> a -> [a]
```
<!-- #endregion -->



## Basic functions






## List transformations




## Reducing lists (folds)




## Special folds




## Building lists




## Scans




## Accumulating maps




## Infinite lists




## Unfolding




## Sublists




## Extracting sublists




## Predicates




## Searching lists




## Searching by equality




## Searching with a predicate




## Indexing lists




## Zipping and unzipping lists




## Special lists




## Functions on strings




## "Set" operations




## Ordered lists




## Generalized functions




## The "By" operations




## User-supplied equality (replacing an Eq context)




## User-supplied comparison (replacing an Ord context)




## The "generic" operations
