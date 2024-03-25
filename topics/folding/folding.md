# Folding

The fold operators are more general than other operations on lists in that they can convert lists into other kinds of values.

## fold

The fold operators come in two flavours, `foldr` and `foldl`.

The informal definition of `foldr` is

`foldr f e [x₀, x₁, …, xₙ] = f x₀ (f x₁ ( … (f xₙ e) …))`

an equivalent formulation using `⊕` as the reducer binary operator

`foldr (⊕) e [x₀, x₁, …, xₙ] = x₀ ⊕ (x₁ ⊕ ( … (xₙ ⊕ e) …))`

The parenthesis always group to the right, explaining the name `foldr` as a right fold.

From this definition we can infer that the second argument of `⊕` must have the same type as the result of `⊕`, but that, in general, the first argument may have a different type. Thus, the most general type for `foldr` is:

`foldr :: (a -> b -> b) -> b -> [a] -> b`

Often `a` and `b` will be instantiated to the same type; this will happen, for example, if `⊕` denotes an associative operation.

Using `foldr` we can define:
- sum     = foldr (+)  0
- product = foldr (*)  1
- concat  = foldr (++) []
- and     = foldr (&&) True
- or      = foldr (||) False

All the examples share an important property: in the expression `foldr (⊕) z` the function `⊕` is associative and has identity element `e`. In other words, we have, `∀ x y z` that:

```hs
x ⊕ (y ⊕ z) = (x ⊕ y) ⊕ z   -- assoc
x ⊕ e = x = e ⊕ x            -- identity
```

which means that `⊕` and `e` form a monoid, in which case

```hs
foldr (⊕) e []
foldr (⊕) e [x₀, x₁, …, xₙ] = x₀ ⊕ x₁ ⊕ … ⊕ xₙ ⊕ e
```

because eliding the parenthesis has no effect on the meaning.

Here are some examples where the arguments to `foldr` do not form a monoid

```hs
length :: [a] -> Int
length = foldr (const (+ 1)) 0

reverse :: forall a. [a] -> [a]
reverse = foldr (\x xs -> xs ++ [x]) []

takewhile :: Foldable t => (a -> Bool) -> t a -> [a]
takewhile p = foldr (★) []
  where
  x ★ xs | p x = x : xs
         | otherwise = []

-- takewhile (< 3) [1..4] = 1 ★ (2 ★ (3 ★ (4 ★ [])))
-- = [1] ++ ([2] ++ ([]))
-- = [1, 2]
```

## foldl

The informal definition of `foldl` is

`foldl f e [x₀, x₁, …, xₙ] = ( … ((e ⊕ x₀) ⊕ x₁) … ) ⊕ xₙ`

The parenthesis always group to the left, explaining the name `foldl` as a left fold.

In particular, we have:

```hs
foldl (⊕) e []        =   e
foldl (⊕) e [x]       =   e ⊕ x
foldl (⊕) e [x, y]    =  (e ⊕ x) ⊕ y
foldl (⊕) e [x, y, z] = ((e ⊕ x) ⊕ y) ⊕ z
-- ...
```

The type for `foldr` is:

`foldl :: (b -> a -> b) -> b -> [a] -> b`

The type of `foldl` is almost the same as the type of `foldr` except that the reducer function takes the arguments in reveresed order (first the acc, then an element).

When `⊕` is associative, both `a` and `b` are instantiated to the same type, and so `foldr` and `foldl` have the same type in such a case.

An example of the use of `foldl` is given by:

```hs

pack :: (Foldable t, Num a) => t a -> a
pack = foldl (⊕) 0
  where
  n ⊕ x = 10 * n + x

pack [1,2,3,4] == 1234
```

This codes a sequence of digits as a single number, assuming the most significant digit comes first.

## Laws

There are a number of important laws concerning foldr and foldl. The first 3 are called duality theorems.

**The first duality theorem** states that:

```hs
foldr (⊕) e xs = foldl (⊕) e xs
```

whenever `⊕` and `e` form a monoid and `xs` is a finite list. Thus, *`foldr` and `foldl` define the same function over monoids*.

However, it is sometimes more efficient to define a function using foldr and sometimes more efficient to use foldl. For example, we could have defined `sum` and `product` using `foldl` instead of `foldr`, and we shall see that using `foldl` is indeed more efficient. On the other hand, `concat`, `and`, and `or` are better defined using `foldr`.

**The second duality theorem** is a generalisation of the first: suppose `⊕` and `⊗` and `e` are such that `∀ x y z` we have:

```hs
x ⊕ (y ⊗ z) = (x ⊕ y) ⊗ z
x ⊕ e = e ⊗ x
```

In other words, `⊕` and `⊗` associate with each other, and `e` on the right of `⊕` is equivalent to `e` on the left of `⊗`. Under these conditions, for any finite list `xs`, we have:

```hs
foldr (⊕) e xs = foldl (⊗) e xs
```

**The third duality theorem** states that:


```hs
foldr (⊕) e xs = foldl (flip ⊗) e (reverse xs)
```

Moreover, for all lists `xs`, we have that:

```hs
xs = foldr cons [] xs

-- from the third duality theorem we have:
foldr cons [] xs = foldl (flip cons) [] (reverse xs)

-- and so:
xs = reverse (reverse xs)
```

for any finite list `xs`


There are many other useful identities concerning foldr and foldl. For example, if `⊕` and `e` *form a monoid*, then, for all lists `xs` and `ys`, we have:

```hs
foldr (⊕) e (xs ++ ys) = foldr (⊕) e xs ⊕ foldr (⊕) e ys
foldl (⊕) e (xs ++ ys) = foldl (⊕) e xs ⊕ foldl (⊕) e ys
```

Also, for arbitrary `f` and `e`, we have that, for all lists `xs` and `ys`:

```hs
foldr f e (xs ++ ys) = foldr f (foldr f e xs) ys
foldl f e (xs ++ ys) = foldl f (foldl f e xs) ys
```

## foldl1 and foldr1

`foldl1` and `foldr1` are variants of respective fold functions without the initial value `e` (identity element, accumulator), defined by:

```hs
foldl1 f [x₀, x₁, …, xₙ] = ( … (x₀ ⊕ x₁) … ) ⊕ xₙ
foldr1 f [x₀, x₁, …, xₙ] = x₀ ⊕ (x₁ ⊕ ( … (xₙ˗₁ ⊕ xₙ) …))


-- In particular, for foldl1 we have:
foldl1 (⊕) [x]       =   x
foldl1 (⊕) [x,y]     =   x ⊕ y
foldl1 (⊕) [x,y,z]   =  (x ⊕ y) ⊕ z
foldl1 (⊕) [x,y,z,w] = ((x ⊕ y) ⊕ z) ⊕ w

-- And for foldr1 we have:
foldr1 (⊕) [x]       = x
foldr1 (⊕) [x,y]     = x ⊕ y
foldr1 (⊕) [x,y,z]   = x ⊕ (y ⊕ z)
foldr1 (⊕) [x,y,z,w] = x ⊕ (y ⊕ (z ⊕ w))
```

>Both `foldl1` and `foldr1` are undefined on the empty list.

The type of both `foldl1` and `foldr1` is:

```hs
foldl1, foldr1 :: (a -> a -> a) -> [a] -> a
```

Defining a function `maximum` on a list using `foldr` would require the initial value `e` (identity element) to be passed in, which we don't have (as an external value; but using the head element of the list would work). 

Using `foldr1` we can define `maximum` as:

```hs
maximum :: Ord a => [a] -> a
maximum = foldr1 max

-- i.e.
maximum' :: Ord a => [a] -> a
maximum' xs = foldr max (head xs) (tail xs)
```

On the other hand, we can define `fold*1` in terms of `fold*`:

```hs
foldl1 (⊕) xs = foldl (⊕) (head xs) (tail xs)
-- foldl1 (⊕) (x:xs) = foldl (⊕) x xs

foldr1 (⊕) xs = foldr (⊕) (head xs) (tail xs)
-- foldr1 (⊕) (x:xs) = foldr (⊕) x xs
```

## Scan

Sometimes, it is convenient to apply a `foldl` operation to every initial segment of a list. This is done by the function `scan`, which can be defined informally in the following way:

```hs
scan (⊕) e [x₀, x₁, …, xₙ] = [e, e ⊕ x₀, (e ⊕ x₀) ⊕ x₁) …]

-- In general:
scan (⊕) e [x₀, x₁, …, xₙ] =
  [ e ⊕ x₀            -- term 0
  , T₀ ⊕ x₁           -- term 1
  , T₁ ⊕ x₂           -- term 2
  , …
  , T ₙ˗₂ ⊕ x ₙ˗₁        -- term (n - 1)
  , T ₙ˗₁ ⊕ x ₙ          -- term n
  ]


-- In particular:
scan (⊕) e [x, y, z] =
  [   e
  ,   e ⊕ x
  ,  (e ⊕ x) ⊕ y
  , ((e ⊕ x) ⊕ y) ⊕ z


scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
```

In a `scan`, the first element (term `T₀`) of the resulting list is always the term obtained by combining `e` with the first element of the input list `x₀`, that is, the first term is `T₀ = e ⊕ x₀`; any next term, `Tᵢ`, is obtained by combining the previous term, `Tᵢ˗₁` with the next element of the input list, `xᵢ`, that is `Tᵢ = Tᵢ˗₁ ⊕ xᵢ` (the current term is the previous term combined with the current element).

It follows that the last element of list `scan (⊕) e xs` is just the value of `foldl (⊕) e xs`. Hence,

`foldl (⊕) e = last . scan (⊕) e`

## Identities

```hs
-- (:) is related to (++) in that (∀ x xs) we have:
(x : xs) ++ ys == x : (xs ++ ys)

xs : [] == [xs]


```
