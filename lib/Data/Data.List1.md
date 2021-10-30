# Data.List

```hs
import Data.List

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool


(\\)            :: Eq a => [a] -> [a] -> [a]

delete         :: Eq a => a -> [a] -> [a]
deleteBy       :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]

dropWhileEnd   :: (a -> Bool) -> [a] -> [a]

elemIndex      :: Eq a => a -> [a] -> Maybe Int
elemIndices    :: Eq a => a -> [a] -> [Int]

find           :: Foldable t => (a -> Bool) -> t a -> Maybe a
findIndex      :: (a -> Bool) -> [a] -> Maybe Int
findIndices    :: (a -> Bool) -> [a] -> [Int]

foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl1' :: (a -> a -> a) -> [a] -> a

genericDrop      :: Integral i => i -> [a] -> [a]
genericIndex     :: Integral i => [a] -> i -> a
genericLength    :: Num i => [a] -> i
genericReplicate :: Integral i => i -> a -> [a]
genericSplitAt   :: Integral i => i -> [a] -> ([a], [a])
genericTake      :: Integral i => i -> [a] -> [a]

group            :: Eq a => [a] -> [[a]]
groupBy          :: (a -> a -> Bool) -> [a] -> [[a]]

inits            :: [a] -> [[a]]

insert   :: Ord a => a -> [a] -> [a]
sortOn   :: Ord b => (a -> b) -> [a] -> [a]
sort     :: Ord a => [a] -> [a]
sortBy   :: (a -> a -> Ordering) -> [a] -> [a]
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]


intercalate      :: [a] -> [[a]] -> [a]
intersect        :: Eq a => [a] -> [a] -> [a]
intersectBy      :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersperse      :: a -> [a] -> [a]

isInfixOf        :: Eq a => [a] -> [a] -> Bool
isPrefixOf       :: Eq a => [a] -> [a] -> Bool
isSuffixOf       :: Eq a => [a] -> [a] -> Bool

mapAccumL    :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR    :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

maximumBy    :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy    :: Foldable t => (a -> a -> Ordering) -> t a -> a

nub          :: Eq a => [a] -> [a]
nubBy        :: (a -> a -> Bool) -> [a] -> [a]

partition    :: (a -> Bool) -> [a] -> ([a], [a])
permutations :: [a] -> [[a]]
scanl' :: (b -> a -> b) -> b -> [a] -> [b]


stripPrefix  :: Eq a => [a] -> [a] -> Maybe [a]
subsequences :: [a] -> [[a]]
tails        :: [a] -> [[a]]

transpose    :: [[a]] -> [[a]]

uncons       :: [a] -> Maybe (a, [a])
unfoldr      :: (b -> Maybe (a, b)) -> b -> [a]

union        :: Eq a => [a] -> [a] -> [a]
unionBy      :: (a -> a -> Bool) -> [a] -> [a] -> [a]

unzip4       :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip5       :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip6       :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip7       :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])

zip4         :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip5         :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip6         :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip7         :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zipWith4     :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith5     :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith6     :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith7     :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]

iterate' :: (a -> a) -> a -> [a]

-- imported via Prelude, Data.List
(!!) :: [a] -> Int -> a
(++) :: [a] -> [a] -> [a]

and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool

all :: Foldable t => (a -> Bool) -> t a -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool

concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

break :: (a -> Bool) -> [a] -> ([a], [a])
span :: (a -> Bool) -> [a] -> ([a], [a])
splitAt :: Int -> [a] -> ([a], [a])

drop :: Int -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]

elem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
null :: Foldable t => t a -> Bool
length :: Foldable t => t a -> Int
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
lookup :: Eq a => a -> [(a, b)] -> Maybe b

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a

head :: [a] -> a
tail :: [a] -> [a]
init :: [a] -> [a]
last :: [a] -> a


maximum :: (Foldable t, Ord a) => t a -> a
minimum :: (Foldable t, Ord a) => t a -> a

cycle :: [a] -> [a]
iterate :: (a -> a) -> a -> [a]
repeat :: a -> [a]
replicate :: Int -> a -> [a]
reverse :: [a] -> [a]

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]


product :: (Foldable t, Num a) => t a -> a
sum :: (Foldable t, Num a) => t a -> a

take :: Int -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]

lines :: String -> [String]
unlines :: [String] -> String

words :: String -> [String]
unwords :: [String] -> String

unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])

zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
```
