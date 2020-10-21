# Universality and expressiveness of fold

(A tutorial on the universality and expressiveness of fold" by Graham Hutton)


## The universal property of fold

For finite lists, **the universal property** can be stated as the following equivalence between two definitions for a function `g` that processes lists:

```haskell
-- definition of foldr
foldr f v []     = v                      -- (1) base case
foldr f v (x:xs) = f x (foldr f v xs)     -- (2) rec case

-- extracting (g = foldr f v) from the definition above, we obtain:

g []     = v
g (x:xs) = f x (g xs)       ⇔       g = foldr f v
--                      LHS ⇔ RHS

-- LHS holds iff (g = foldr f v)
```

Substituting `g` (on the left side) with `fold f v`  gives the recursive definition for fold:

```haskell
foldr f v []     = v
foldr f v (x:xs) = f x (foldr f v xs)
```

Conversely, the two equations for `g` (on the left side) are precisely the *assumptions* required to show that `g = fold f v`.


```haskell
-- replace [] with v
fold f v [] = v
-- replace each (:) with f
fold f v (x : xs) = f x (fold f v xs)
```

Folding means replacing all cons `(:)` with `f` and `[]` with `v`:

```haskell
-- foldl  f   v     list
fold     (+)   0    [0,1,2,3]
0 : 1 : 2 : 3 : []
  +   +   +   + 0
-- replace each (:) with f=(+) and [] with v=0
```

Taken as a whole, the **universal property** states that for finite lists the function `fold f v` is not just a solution to its defining equations, but in fact the **unique solution**.

The key to the utility of the universal property is that it makes explicit the two assumptions required for a certain pattern of inductive proof.

For specific cases then, by verifying the two assumptions (which can typically be done without the need for induction) we can then appeal to the universal property to complete the inductive proof that `g = fold f v`.

In this manner, the *universal property of fold* encapsulates a simple *pattern of inductive proof* concerning lists, just as the *fold operator* itself encapsulates a simple *pattern of recursion* for processing lists.
