# Polykinded data types

* Level 0: term-level lists containing elements of the same ground type

- the kind `Type`
- the kind `Type -> Type`

```hs
-- [Int] :: Type
l1 :: [Int]
l1 = [1,2,3]

-- [Int -> Int] :: Type
l2 :: [Int -> Int]
l2 = [(+1), (*2), (^3)]
```
