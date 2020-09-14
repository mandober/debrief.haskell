# Operators

Operators consisting of special characters

```hs
(==) ::  Eq a => a -> a -> Bool
(/=) ::  Eq a => a -> a -> Bool

(<)  :: Ord a => a -> a -> Bool
(<=) :: Ord a => a -> a -> Bool
(>)  :: Ord a => a -> a -> Bool
(>=) :: Ord a => a -> a -> Bool

(&&) :: Bool -> Bool -> Bool         -- infixr 3 &&
(||) :: Bool -> Bool -> Bool         -- infixr 2 ||

(!!) :: [a] -> Int -> a

(+)  :: Num a => a -> a -> a          -- infixl 6 +
(-)  :: Num a => a -> a -> a          -- infixl 6 -
(*)  :: Num a => a -> a -> a          -- infixl 7 *
(/)  :: Fractional a => a -> a -> a   -- infixl 7 /
(**) :: Floating a => a -> a -> a     -- infixr 8 **

($)  :: (a -> b) -> a -> b            -- infixr 0 $! -- apply low assoc, lazy
($!) :: (a -> b) -> a -> b            -- infixr 0 $! -- apply low assoc, strict
(.)  :: (b -> c) -> (a -> b) -> a -> c
(++) :: [a] -> [a] -> [a]             -- infixr 5 ++

(<$)  :: Functor f => a -> f b -> f a
(<$>) :: Functor f => (a -> b) -> f a -> f b

(<*)  :: Applicative f => f a -> f b -> f a
(*>)  :: Applicative f => f a -> f b -> f b         -- infixl 4
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

(>>)  :: Monad m => m a -> m b -> m b
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```
