# Universality as a definition principle
(A tutorial on the universality and expressiveness of fold - Graham Hutton)

As well as being used as a proof principle, the *universal property of fold* can also be used as a *definition principle* that guides the transformation of recursive functions into definitions using fold.

Consider the recursively defined function `sum` that sums a list of numbers:

```hs
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

Now, suppose that we want to redefine `sum` using `fold`; that is, we want to solve the equation `sum = fold f v` for a function `f` and a value `v`.

We begin by observing that the equation matches the RHS of the *universal property*, `sum = fold f v` ≡ `g = foldr f v`, from which we conclude that the equation is equivalent to the following two equations:

```hs
-- universal property
g []     = v
g (x:xs) = f x (g xs)       ⇔       g = foldr f v
-- sum matches the UP's RHS:       sum = fold f v

-- subst g for sum                  -- def of sum
sum [] = v                          sum [] = 0
sum (x:xs) = f x (sum xs)           sum (x:xs) = x + (sum xs)
```

From the first equation and the definition of `sum`, it is clear that `v = 0`. From the second equation, we calculate that `f` = `(+)` since:

```
sum [] = v <=>
sum [] = 0                        [base step of sum]
--------------
v = 0

sum (x:xs) = f x (sum xs)  <=>    [rec step definition of UP]
sum (x:xs) = x + (sum xs)  ==>    [rec step definition of sum]
f x (sum xs) ==                   [equality]
x + (sum xs)
 f  x y ==                        [generalising `sum xs` to y] †
(+) x y                           [prefixing +]
----------
f = (+)
```

So using the universal property we have calculated that: `sum = fold (+) 0`.

† the key step in calculating a definition for `f` is the generalisation of the expression `sum xs` to a fresh variable `y`. In fact, such a generalisation step is not specific to the `sum` function, but will be a key step in the transformation of any recursive function into a definition using fold in this manner.

Of course, this example with sum is rather artificial because the definition of sum using fold is immediate. However, there are many examples of functions whose definition using fold is not so immediate. Consider the recursively defined function `map f` that applies a function `f` to each list element:

```hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

To redefine `map f` using `fold` we must solve the equation `map f = fold g v` for a function `g` and a value `v`. By appealing to the universal property, we conclude that this equation is equivalent to the following two equations:

```hs
-- universal property
g []     = v
g (x:xs) = f x (g xs)       ⇔       g = fold f v
-- map matches the RHS:          map f ≡ fold g v
-- so we can now subst
-- (map f) for every (g) on the LHS of UP:

-- def of map                     -- def of UP
map f []     = []                 g []     = v
map f (x:xs) = f x : map f xs     g (x:xs) = f x (g xs)

-- subst: g -> map f
map f []     = v
map f (x:xs) = g x (map f xs)
```

And now we have:

```
map f [] = [] <=>                 [base case]
map f [] = v
-------------
v = []
```

From the first equation and the definition of map it is immediate that `v=[]`. From the second equation, we calculate a definition for `g` as follows:

```
map f (x:xs)   = g x (map f xs) ⇔      { Definition of map }
f x : map f xs = g x (map f xs) ⇐      { Generalising `map f xs` -> `ys` }
f x : ys       = g x ys         ⇔      { Functions }
g    x ys      = f x : ys
g = \x xs ->     f x : xs
```

That is, using the universal property we have calculated that:

map f = fold (\ x ys -> f x : ys) []


> In general, any function on lists that can be expressed using the fold operator can be transformed into such a definition using the universal property of fold.
