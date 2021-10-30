# Lazy evaluation

```hs
xs = [1 :: Int .. ]
:sprint xs
xs = _
ys = drop 2 xs :: [Int]
:sprint ys
ys = _
_ = take 1 ys -- [3]
:sprint ys
ys = 3 : _
:sprint xs
xs = 1 : 2 : 3 : _
```

This also shows that list values are shared: when the list `ys` is forced, the list `xs` gets forced as well, meaning their values are shared.

```
xs
↓
1 : 2 : 3 : _
        ↑
        ys
```


## :sprint and ad hoc polymorphic values

For example, `xs' = [1..5]` is by default given the type `Num a => [a]`.

```hs
xs = [1 .. ]
:sprint xs
xs = _
ys = drop 2 xs
:sprint ys
ys = _
_ = take 1 ys
:sprint ys
ys = _
:sprint xs
xs = _
```

To evaluate such an expression, the type for `a` must be filled in, meaning that in `:sprint xs'`, the value `xs'` is different from its first definition.

## Shared values

A shared value can be either a performance essential or an ugly space leak.

Consider the following seemingly similar scenarios:
(run with `ghci +RTS -M20m` to not throttle your computer)

```hs
foldl' (+) 0 [1..10^6]  -- 500000500000

let xs = [1..10^6] :: [Int]
foldl' (+) 0 xs     -- <interactive>: Heap exhausted;
```

By just assigning the list to a variable `xs`, we exhausted the heap of a calculation that worked just fine previously. In the first calculation, the list could be garbage-collected as we folded over it. But in the second scenario, we kept a reference to the head of the linked list. Then the calculation blows up, because the elements cannot be garbage-collected due to the reference to `xs`.
