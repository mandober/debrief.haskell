# Trial: Functions


## Composition

```hs
id         :: a -> a
const      :: a -> b -> a

flip       :: (a -> b -> c) -> (b -> a -> c)
(.)        :: (b -> c) -> (a -> b) -> a -> c

-- applying flip to compose
flip (.)   :: (a -> b) -> (b -> c) -> a -> c

-- composing flip and compose
flip . (.) :: (b -> c) -> b -> a -> c

-- composing flip and compose, then applying to const and id
flip . (.) const id :: (b -> c) -> b -> a -> c
```


## Kleisli Categories

```hs
-- normal unary fn
f :: a -> b
-- decorated unary fn (String is the log status message of a fn)
-- the sig remains compatible
f :: a -> (b, String)

eId :: a -> (a, String)
eId x = (x, "")

-- defining composition for decorated fns, g >=> f (instead of g . f)
(>=>) :: (b -> (c, String)) -> (a -> (b, String)) -> (a -> (c, String))
g >=> f = \a ->
    let (b, flog) = f a
        (c, glog) = g b
    in  (c, glog ++ flog)


(>=>) :: (b -> (c, String)) -> (a -> (b, String)) -> (a -> (c, String))
(>=>) :: (b -> Writer c) -> (a -> Writer b) -> (a -> Writer c)
-- compare sig to monad's bind
(=<<) :: Monad m => (a -> m b) -> m a -> m b
--                 f:x -> my   -> mx  -> my


(\x -> Just $ x + 5) =<< Just 6 :: Num b => Maybe b
--Just 11
```
