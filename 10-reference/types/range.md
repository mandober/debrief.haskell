# Ranges

Generate range from `m` to `n` (inclusive)

Predefined:
enumFrom       :: Enum a => a -> [a]                [m..∞]
enumFromThen   :: Enum a => a -> a -> [a]           [i_1,i_2,...,∞]
enumFromTo     :: Enum a => a -> a -> [a]           [m..n]
enumFromThenTo :: Enum a => a -> a -> a -> [a]      [i_1,i_2,...,i_n]

Custom: from 1 to n (define by partially applying 1 to enumFromTo)
enumTo n = enumFromTo 1     [1..n]

```hs
enumFrom :: Enum a => a -> [a]
enumFrom 1   -- [1,2,3,...,∞] to infinity! <WARNING>

enumFromThen   :: Enum a => a -> a -> [a]
enumFromThen 1 3 -- [1,3,5,...,∞] to infinity! <WARNING>


enumFromTo :: Enum a => a -> a -> [a]
enumFromTo 2 5   -- [2,3,4,5]

enumFromThenTo :: Enum a => a -> a -> a -> [a]
enumFromThenTo 1 3 11  -- [1,3,5,7,9,11]

-- there's no version with lower bound defaulting to 1
-- make it by partially appling m as 1, m=1
enumTo :: Enum a => a -> [a]
enumTo n = enumFromTo 1

-- ranges
[1..5] :: (Enum t, Num t) => [t]
[1..5]   -- [1,2,3,4,5]

-- ops
map (+1) [1..4]
map (\x -> x + 5) [5..15]
sum [1..10]

-- Haskell knows Greek
putStrLn ['α'..'ω']
```
