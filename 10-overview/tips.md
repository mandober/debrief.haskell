# Tips


## Defining recursive funtions

Defining functions very often means defining recursive functions, but in any case, we should start with the types/values involved.

For example, to define a right fold over a list, as say, `rightFold`, we'll definitely be working with a list. Whenever a list is involved, we know we'll have a two-piece function: the first piece that handles the empty list, and the second piece that handles the general case. And there's our first problem: what should the `rightFold` return if the list is empty?

```hs
-- so far, we know we have a list
rightFold :: [a] -> ...
rightFold [] = ???
rightFold (x:xs) = ...
```

We know the `rightFold` and other fold functions, can do many thing with a list; they can sum up the values of an `Int` list, returning an `Int`, but folds can also return a new list, `[a]`. The return type of the fold functions is not at all fixed. Hell, we don't even know if it's gonna be a scalar value (like `Int`) or a compound type like a list of some values, `[a]`.

Even if we focus for a moment on case where a fold sums up a list, returning 0 in case of the empty list doesn't seems right. Zero should be returned when we sum up a list like `[-5, 3, 2]`, not when the list is empty. And returning the empty list, `[]`, would mean that we sometimes return [a] (the type of the empty list) and sometimes an `Int`. And that shit won't be tolerated by the compiler.

This sort of problem when there is more than one candidate for the return type, or when the return type seems elusive to pinpoint, usually means introducing a default value. This value is to be some kind of a neutral element that's returned in the case a list is empty. However, if we return this default value of unknown type `b`, then the type of the result in the general case must also have the type `b`.

```hs
-- now we know we'll need a default value of some unknown type
rightFold :: [a] -> b -> ... -> b
rightFold []     dflt = dflt
rightFold (x:xs) dflt = ...
```

If we again focus just on the case when we want to sum up a list of `Int`s, now it's more clear how to go about it, i.e. what to return in the case the list is empty. In that case we return the passed in defult value that must be of the same type as the result in the general case, an `Int`. We now know that rightFold should take a list of integers, [Int], a default value that's also an `Int`, as is the rightFold's return type. However, we still need the function that will sum up the list elements. That is, we need to determine the type of that function. As we're working with Ints here, surely they wil be involved, so we could just go with a function type `Int -> Int`; it takes an Int and returns an Int.

```hs
-- exploring just a list summation
sumup :: [Int] -> Int -> (Int -> Int) -> Int
sumup []     d f = d
sumup (x:xs) d f = ...
```

(TBC)













* Avoid parenthesis by
  - knowing your fixities
  - using the `$` function
  - using composition
  - using composition with point-free style
  - infixing a function/operator


```hs
-- know thy fixities (infixr 5 :, ++)
1 : 2 : [3..6] ++ [7..10] === 1 : (2 : ([3..6] ++ [7..10]))

1 : 2 : 3 : [5] ++ [7] ++ [9]

x : (xs ++ ys) === x : xs ++ ys


head (concat xs ys)
head $ concat xs ys
head $ xs `concat` ys

x : xs ++ ys

(x:) . (++) xs

isMember x xs = any ((==) x) xs
isMember x xs = any (x==) xs
isMember x    = any (x==)
isMember      = any . (==)

-- infixing a function
eq (x:xs) (y:ys) = eq x `eq` y
-- sometimes even in pattern matching:
eq (x:xs) (y:ys) = x `eq` y


(k1 :> v1) `lt` (k2 :> v2) = k1 `lt` k2
```
