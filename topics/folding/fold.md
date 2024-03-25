# folding functions


## `fold`

```hs
-- Identifier defined in Data.Foldable
fold :: (Foldable t, Monoid m) => t m -> m
```

Given a structure with elements whose type is a `Monoid`, combine them via the monoid's `<>` operator.

>`fold` is right-associative and lazy in the accumulator.

When you need a strict left-associative fold, use `foldMap'` instead, with `id` as the map.

foldMap' id


* Examples

```hs
xss = [[1, 2, 3], [4, 5], [6], []]
fold xss == foldr (<>) []           -- [1,2,3,4,5,6]


fold $ Node (Leaf (Sum 1)) (Sum 3) (Leaf (Sum 5)) -- Sum {getSum = 9}
```


* Folds of unbounded structures do not terminate when the monoid's `<>` operator is strict:

```hs
fold (repeat Nothing) -- Hangs forever
```


* Lazy corecursive folds of unbounded structures are fine:

```hs
take 12 $ fold $ map (\i -> [i .. i + 2]) [0 ..]
-- [0,1,2,1,2,3,2,3,4,3,4,5]

sum $ take 4000000 $ fold $ map (\i -> [i .. i + 2]) [0 ..]
-- 2666668666666
```

## `foldl1`

```hs
-- Identifier defined in 'Data.Foldable'
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
```

A variant of `foldl` that has no base case, and thus may only be applied to non-empty structures.

This function is non-total and will raise a runtime exception if the structure happens to be empty.

>foldl1 f = List.foldl1 f . toList


```hs
-- Examples

>>> foldl1 (+) [1..4]
// 10

>>> foldl1 (+) []
// *** Exception: Prelude.foldl1: empty list

>>> foldl1 (+) Nothing
// *** Exception: foldl1: empty structure

>>> foldl1 (-) [1..4]
// -8

>>> foldl1 (&&) [True, False, True, True]
// False

>>> foldl1 (||) [False, False, True, True]
// True

>>> foldl1 (+) [1..]
// * Hangs forever *
```
