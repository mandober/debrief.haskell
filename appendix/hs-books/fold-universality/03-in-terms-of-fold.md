# Universality and expressiveness of fold

"A tutorial on the universality and expressiveness of fold" by Graham Hutton

## In terms of fold

Various functions on lists can be defined in terms of `foldr`:

```haskell
(+) :: [Int] -> Int
(+) = fold (+) 0

(*) :: [Int] -> Int
(*) = fold (*) 1


and :: [Bool] -> Bool
and = fold and True

or :: [Bool] -> Bool
or = fold or False


(++) :: [a] -> [a] -> [a]
(++ ys) = foldl (:) ys


length :: [a] -> Int
length = fold (\x n -> 1 + n) 0

reverse :: [a] -> [a]
reverse = fold (\x xs -> xs ++ [x]) []


map :: (a -> b) -> [a] -> [b]
map f = fold (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = fold (\x xs -> if p x then x : xs else xs) []
```
