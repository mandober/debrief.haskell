# GHC.List

```hs
import GHC.List

GHC.List.all :: (a -> Bool) -> [a] -> Bool
GHC.List.and :: [Bool] -> Bool
GHC.List.any :: (a -> Bool) -> [a] -> Bool
GHC.List.concat :: [[a]] -> [a]
GHC.List.concatMap :: (a -> [b]) -> [a] -> [b]
GHC.List.elem :: Eq a => a -> [a] -> Bool
errorEmptyList :: String -> a
GHC.List.foldl :: (b -> a -> b) -> b -> [a] -> b
foldl' :: (b -> a -> b) -> b -> [a] -> b
GHC.List.foldl1 :: (a -> a -> a) -> [a] -> a
foldl1' :: (a -> a -> a) -> [a] -> a
GHC.List.foldr1 :: (a -> a -> a) -> [a] -> a
iterate' :: (a -> a) -> a -> [a]
GHC.List.length :: [a] -> Int
GHC.List.maximum :: Ord a => [a] -> a
GHC.List.minimum :: Ord a => [a] -> a
GHC.List.notElem :: Eq a => a -> [a] -> Bool
GHC.List.null :: [a] -> Bool
GHC.List.or :: [Bool] -> Bool
GHC.List.product :: Num a => [a] -> a
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
GHC.List.sum :: Num a => [a] -> a
uncons :: [a] -> Maybe (a, [a])
GHC.List.foldr :: (a -> b -> b) -> b -> [a] -> b

-- imported via Prelude, GHC.List
(!!)      :: [a] -> Int -> a
break     :: (a -> Bool) -> [a] -> ([a], [a])
cycle     :: [a] -> [a]
drop      :: Int -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
filter    :: (a -> Bool) -> [a] -> [a]
head      :: [a] -> a
init      :: [a] -> [a]
iterate   :: (a -> a) -> a -> [a]
last      :: [a] -> a
lookup    :: Eq a => a -> [(a, b)] -> Maybe b
repeat    :: a -> [a]
replicate :: Int -> a -> [a]
reverse   :: [a] -> [a]
scanl     :: (b -> a -> b) -> b -> [a] -> [b]
scanl1    :: (a -> a -> a) -> [a] -> [a]
scanr     :: (a -> b -> b) -> b -> [a] -> [b]
scanr1    :: (a -> a -> a) -> [a] -> [a]
span      :: (a -> Bool) -> [a] -> ([a], [a])
splitAt   :: Int -> [a] -> ([a], [a])
tail      :: [a] -> [a]
take      :: Int -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]
unzip     :: [(a, b)] -> ([a], [b])
unzip3    :: [(a, b, c)] -> ([a], [b], [c])
zip       :: [a] -> [b] -> [(a, b)]
zip3      :: [a] -> [b] -> [c] -> [(a, b, c)]
zipWith   :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3  :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(++)      :: [a] -> [a] -> [a]
map       :: (a -> b) -> [a] -> [b]
```
