# flip

```hs
flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a


-- but flipping unary function is a trip!

flip id :: a -> (a -> b) -> b
(flip id) 2 (+5) -- 7
flip id ≡ flip ($)

flip (flip id) :: (a -> b) -> a -> b
flip (flip id) ≡ ($)

flip flip id :: (a -> (b -> b) -> c) -> a -> c


fst :: (a, b) -> a
flip fst :: a -> (a -> c, b) -> c
flip fst 3 ((*5), 7)    -- 15

snd :: (a, b) -> b
flip snd :: b -> (a, b -> c) -> c
flip snd 3 (7, (*5))    -- 15

flip flip :: b -> (a -> b -> c) -> a -> c
flip flip 3 (+) 5       -- 8


flip (+4) :: Num  (b -> c) => b -> (b -> c) -> c
flip succ :: Enum (b -> c) => b -> (b -> c) -> c
```
