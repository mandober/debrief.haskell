# Generating tuples
(A tutorial on the universality and expressiveness of fold - Graham Hutton)

As an example of using fold to generate tuples, consider the function `sumlength` that calculates the `sum` and `length` of a list of numbers:

```hs
sumlength :: [Int] -> (Int, Int)
sumlength xs = (sum xs, length xs)
```

By a straightforward combination of the definitions of the functions sum and length using `fold`, the function sumlength can be redefined as a single
application of fold that generates a pair of numbers from a list of numbers:

```hs
sumlength = fold (\ n (x, y) -> (n + x, 1 + y)) (0, 0)
```
