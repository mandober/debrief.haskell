# GHC.Arr

```hs
class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  unsafeIndex :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  unsafeRangeSize :: (a, a) -> Int

data Array i e = Array !i !i !Int (Array# e)

data STArray s i e = STArray !i !i !Int (MutableArray# s e)

array     :: Ix i => (i, i) -> [(i, e)] -> Array i e
listArray :: Ix i => (i, i) -> [e]      -> Array i e

arrEleBottom :: a


negRange :: Int
safeRangeSize :: Ix i => (i, i) -> Int

(!) :: Ix i => Array i e -> i -> e

safeIndex :: Ix i => (i, i) -> Int -> i -> Int
badSafeIndex :: Int -> Int -> Int

indices :: Ix i => Array i e -> [i]
elems   :: Array i e -> [e]
bounds  :: Array i e -> (i, i)

numElements :: Array i e -> Int
numElementsSTArray :: STArray s i e -> Int

assocs :: Ix i => Array i e -> [(i, e)]

accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e

adjust :: (e -> a -> e) -> MutableArray# s e -> (Int, a) -> STRep s b -> STRep s b

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e

amap :: (a -> b) -> Array i a -> Array i b

accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
ixmap :: (Ix i, Ix j) => (i, i) -> (i -> j) -> Array j e -> Array i e

eqArray  :: (Ix i, Eq e)  => Array i e -> Array i e -> Bool

cmpArray :: (Ix i, Ord e) => Array i   e -> Array i   e -> Ordering
cmpIntArray     :: Ord e  => Array Int e -> Array Int e -> Ordering

newSTArray    :: Ix i => (i, i) -> e -> ST s (STArray s i e)
boundsSTArray :: STArray s i e -> (i, i)

readSTArray   :: Ix i => STArray s i e -> i -> ST s e
writeSTArray  :: Ix i => STArray s i e -> i -> e -> ST s ()

freezeSTArray :: STArray s i e -> ST s (Array i e)
thawSTArray   :: Array i e -> ST s (STArray s i e)

fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
done :: i -> i -> Int -> MutableArray# s e -> STRep s (Array i e)

foldlElems :: (b -> a -> b) -> b -> Array i a -> b
foldlElems' :: (b -> a -> b) -> b -> Array i a -> b
foldl1Elems :: (a -> a -> a) -> Array i a -> a
foldrElems :: (a -> b -> b) -> b -> Array i a -> b
foldrElems' :: (a -> b -> b) -> b -> Array i a -> b
foldr1Elems :: (a -> a -> a) -> Array i a -> a

lessSafeIndex :: Ix i => (i, i) -> Int -> i -> Int

unsafeArray :: Ix i => (i, i) -> [(Int, e)] -> Array i e
unsafeArray' :: (i, i) -> Int -> [(Int, e)] -> Array i e
unsafeAt :: Array i e -> Int -> e
unsafeReplace :: Array i e -> [(Int, e)] -> Array i e
unsafeAccumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(Int, a)] -> Array i e
unsafeAccumArray' :: (e -> a -> e) -> e -> (i, i) -> Int -> [(Int, a)] -> Array i e
unsafeAccum :: (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeReadSTArray :: STArray s i e -> Int -> ST s e
unsafeWriteSTArray :: STArray s i e -> Int -> e -> ST s ()
unsafeFreezeSTArray :: STArray s i e -> ST s (Array i e)
unsafeThawSTArray :: Array i e -> ST s (STArray s i e)
```
