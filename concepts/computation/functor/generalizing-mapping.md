# Generalizing mapping

## Properties of mapping

1. Mapping may change the contents (the type of elements), but never the shape of the structure (the type of the structure, the number of elements)


## Mapping lists

The `map` function maps a list with a given a unary function, by applying that function to all elements of the list.

```hs
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
```

If the list is empty, map returns an empty list, but of the different type, hence justifying the article "an" (empty list) vs "the" empty list, as would be the case in math. Notably, the expected type of the input list is [a], while the output is [b], although a list's type need not change. That is, if the list does chnage it's type, e.g. in case we are mapping the function `length` over a list of strings, then, in the empty case, the type of the empty list does change from `[] :: [String]` into `[] :: [Int]`.
