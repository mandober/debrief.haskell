
```hs
class Foldable (t :: * -> *) where
    foldl                :: (b -> a -> b) -> b -> t a -> b
    foldr                :: (a -> b -> b) -> b -> t a -> b
    foldl1               :: (a -> a -> a) -> t a -> a
    foldr1               :: (a -> a -> a) -> t a -> a
    foldMap              :: Monoid m => (a -> m) -> t a -> m
    Data.Foldable.fold   :: Monoid m => t m -> m
    Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
    Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
    null                 :: t a -> Bool
    length               :: t a -> Int
    Data.Foldable.toList :: t a -> [a]
    maximum     :: Ord a => t a -> a
    minimum     :: Ord a => t a -> a
    sum         :: Num a => t a -> a
    product     :: Num a => t a -> a
    elem        :: Eq  a => a -> t a -> Bool
-- MINIMAL: foldMap | foldr
```
