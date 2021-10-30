# Universality and expressiveness of fold

*A tutorial on the universality and expressiveness of fold* by Graham Hutton

## The fold operator

In Haskell, the right-folding function for lists, `foldr`, is defined as:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v                      -- base case
foldr f v (x:xs) = f x (foldr f v xs)     -- rec case
```

`foldr³` is a ternary function that takes:
- a binary, `step`, function, `step² :: a -> b -> b`
- an initial value, `v :: b`
- and a list, `l :: [a]`

```hs
          element   acc         list
          ↓         ↓           ↓
foldr :: (a -> b -> b) -> b -> [a] -> b
               ↑          ↑           ↑
               acc (list) acc         acc
```


> Every data structure has the canonical folding function that takes as its arguments expressions where each corresponds to a particular data ctor of the structure. To fold a data structure means to replace its data constructors with the corresponding expression (usually a function).


Given     
...a binary function `f :: a → b → b`    
...an initial value `v :: b`    
...and a list `s :: [a]`     
the function `fold f v s`     
...processes a list of type `[a]`    
...returning a value of type `b`    
by replacing:    
...the *nil ctor* `[]` with __`v`__     
...and each *cons ctor* `(:)` with __`f`__


```haskell
[1, 2, 3]                 -- nominal list form

1  :   2  :   3 :   []    -- de-sugared list form
1  :  (2  :  (3 :   []))  -- cons (:) is right-associative
1 (:) (2 (:) (3 (:) []))  -- preparing cons for prefix position
(:) 1 ((:) 2 ((:) 3 []))  -- cons as prefix
 f  1 ( f  2 ( f  3 v ))  -- replacing [] with `v` and each cons with `f`
 f  x ( f  x ( f  x v ))

1  :  2  :  3  :  []      -- de-sugared list form
x⁰ f  x¹ f  x¹ f  v       -- foldr  f  v     s
x⁰ +  x¹ +  x¹ +  0       -- foldr (+) 0 [1, 2, 3]
1  +  2  +  3  +  0       -- 5
```

In this manner, the fold operator encapsulates a simple pattern of recursion for processing lists, in which the two constructors for lists are replaced by other values and functions (cons is replaced with a combining function and nul ctro by the initial value).
