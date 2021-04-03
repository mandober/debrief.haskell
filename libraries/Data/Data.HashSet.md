# Data.HashSet

```hs
import Data.HashSet


type role HashSet nominal

newtype HashSet a = (...)ETC

delete :: (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) => a -> HashSet a -> HashSet a
difference ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  HashSet a -> HashSet a -> HashSet a
empty :: HashSet a
Data.HashSet.filter :: (a -> Bool) -> HashSet a -> HashSet a
foldl' :: (a -> b -> a) -> a -> HashSet b -> a
Data.HashSet.foldr :: (b -> a -> a) -> a -> HashSet b -> a
fromList ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  [a] -> HashSet a
fromMap ::
  unordered-containers-0.2.10.0:Data.HashMap.Base.HashMap a ()
  -> HashSet a
insert ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  a -> HashSet a -> HashSet a
intersection ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  HashSet a -> HashSet a -> HashSet a
Data.HashSet.map ::
  (hashable-1.3.0.0:Data.Hashable.Class.Hashable b, Eq b) =>
  (a -> b) -> HashSet a -> HashSet b
member ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  a -> HashSet a -> Bool
Data.HashSet.null :: HashSet a -> Bool
singleton ::
  hashable-1.3.0.0:Data.Hashable.Class.Hashable a => a -> HashSet a
size :: HashSet a -> Int
toList :: HashSet a -> [a]
toMap ::
  HashSet a
  -> unordered-containers-0.2.10.0:Data.HashMap.Base.HashMap a ()
union ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  HashSet a -> HashSet a -> HashSet a
unions ::
  (Eq a, hashable-1.3.0.0:Data.Hashable.Class.Hashable a) =>
  [HashSet a] -> HashSet a
```
