# A tutorial on universality and expressiveness of fold
by Graham Hutton, 1999

http://www.cs.nott.ac.uk/-gmh (old url that redirects to)
https://www.cs.nott.ac.uk/~pszgmh/
https://www.cs.nott.ac.uk/~pszgmh/bib.html#fold

## 1. Summary

```hs
-- universal property of fold
g []     = e
g (x:xs) = f x (g xs)  <=>  g = fold f e

-- fusion property of fold
h w       = e
h (g x y) = f x (h y)  ==>  h . fold g w = fold f e

-- More generally, by replacing (+) by an arbitrary assoc operator ⊕
-- a simple application of fusion shows that:
(⊕ a) . fold (⊕) b = fold (⊕) (b ⊕ a)
```

## 2. The fold operator

Given a function `f :: a -> b -> b` and a value `e :: b `, the function `fold f e` processes a list of type [a] to give a value of type `b` by replacing the nil ctor `[]` at the end of the list by the value `e`, and each cons ctor `:` within the list by the function `f`.

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = f x (foldr f e xs)
```

In this manner, the fold operator encapsulates a simple pattern of recursion for processing lists, in which the two ctors for lists are simply replaced by other values and functions.

A number of familiar functions on lists have a simple definition using fold.

```hs
sum :: [Int] -> Int
sum = foldr (+) 0

len :: [Int] -> Int
len = foldr (+ 1) 0

product :: [Int] -> Int
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True
```

The notational device, called *sectioning*, is often useful when defining simple functions using fold. If required, one of the arguments can also be enclosed in the parentheses. For example, the function (++) that appends two lists to give a single list can be defined as follows:

```hs
(⫲) :: [a] -> [a] -> [a]
-- (⫲ ys) = fold (:) ys
(⫲) = \ys -> (++ ys)
```

In the examples so far, the constructor (:) is replaced by a built-in function. However, in most applications of fold (:) is replaced by a user-defined lambda.

```hs
length :: [a] -> Int
length = foldr (\ _ n -> 1 + n) 0
length = foldr (const succ) 0

reverse :: [a] -> [a]
reverse = foldr (\ x xs -> xs ++ [x]) []

map :: (a -> b) -> ([a] -> [b])
map f = foldr (\ x xs -> f x : xs) []

filter :: (a -> Bool) -> ([a] -> [a])
filter p = foldr (\ x xs -> if p x then x:xs else xs) []
```

## 3. The universal property of fold

As with the fold operator itself, the universal property of fold also has its origins in recursion theory. The first systematic use of the universal property in functional programming was by Malcolm (1990a), in his generalisation of Bird and Meerten's theory of lists (Bird, 1989; Meertens, 1983) to arbitrary regular datatypes.

For finite lists, *the universal property of fold* can be stated as an equivalence between two definitions for a function `g` that processes lists:

```hs
g []     = e
g (x:xs) = f x (g xs)  <=>  g = fold f e
```

In the right-to-left direction, substituting `g = fold f e` into the two equations for `g` gives the recursive definition for fold. Conversely, in the left-to-right direction the two equations for `g` are precisely the assumptions required to show that `g = fold f e`.

Taken as a whole, the **universal property** states that for finite lists the function `fold f e` is not just a solution to its defining equations, but in fact the *unique solution*.

The key to the utility of the universal property is that it makes explicit the two assumptions required for a certain pattern of inductive proof. For specific cases then, by verifying the two assumptions (which can typically be done without the need for induction) we can then appeal to the universal property to complete the inductive proof that `g = fold f e`. In this manner, the universal property of fold encapsulates a simple pattern of inductive proof concerning lists, just as the fold operator itself encapsulates a simple pattern of recursion for processing lists. The universal property of fold can be generalised to handle partial and infinite lists (Bird, 1998).

### 3.1 Universality as a proof principle

The primary application of the universal property of fold is as a proof principle that avoids the need for inductive proofs.

```hs
g []     = e
g (x:xs) = f x (g xs)  <=>  g = fold f e
```

As an example, consider the following equation between functions that process a list of numbers:

    (+ 1) ∘ sum = fold (+) 1

The left-hand function sums a list and then increments the result. The right-hand function processes a list by replacing each (:) by (+) and the empty list by 1. The equation asserts that these two functions always give the same result when applied to the same list.

To prove this equation, we begin by observing that it matches the right-hand side `g = fold f e` of the universal property of fold, with `g = (+ 1) ∘ sum`, `f = (+)`, and `e = 1`.

```hs
g []     = e
g (x:xs) = f x (g xs) <=> g = fold f e

(+ 1) . sum           <=> fold (+) 1

g = fold f   e
g = fold (+) 1

f = (+)
e = 1

g []     = e
g (x:xs) = f x (g xs)

g []     = 1
g (x:xs) = (+) x (g xs)

g = (+ 1) . sum

((+ 1) . sum) []     = 1
((+ 1) . sum) (x:xs) = (+) x (((+ 1) . sum) xs)
```

Hence, by appealing to the universal property, we conclude that the equation to be proved is equivalent to the following two equations:

```hs
((+ 1) . sum) []     = 1
((+ 1) . sum) (x:xs) = (+) x (((+ 1) . sum) xs)
```

At first sight, these may seem more complicated than the original equation. However, simplifying using the definitions of composition and sectioning gives

```hs
1 + sum []     = 1
1 + sum (x:xs) = 1 + x + sum xs
```

which can now be verified by simple calculations, shown here in two columns:

```hs
sum :: [Int] -> Int
sum []     = 0            -- eq.1
sum (x:xs) = x + sum xs   -- eq.2

sum []     + 1 = 1
sum (x:xs) + 1 = x + sum xs + 1

-- (1)
sum [] + 1 = 1
     0 + 1 = 1                      { sum definition eq.1 }
         1 = 1                      { simplifying }

-- (2)
  sum (x:xs) + 1 = x + sum xs + 1
(x + sum xs) + 1 = x + sum xs + 1   { sum definition eq.2}
      x + sum xs = x + sum xs       { subtracting 1 from both sides}
          sum xs = sum xs           { subtracting x from both sides}
-- refl
```

Normally this proof would have required an explicit use of induction. However, in the above proof the use of induction has been encapsulated in the universal property of fold.

In general, any two functions on lists that can be proved equal by induction can also be proved equal using the universal property of the fold operator, provided that the functions can be expressed using fold.

### 3.2 The fusion property of fold

Now we generalize from the sum example and consider the following equation between functions that process a list of values:

>h . fold g w = fold f e

This pattern of equation occurs frequently when reasoning about programs written using fold. It is not true in general, but we can use the universal property of fold to calculate conditions under which the equation is true.

The equation matches the right-hand side of the universal property, from which we conclude that the equation is equivalent to the following 2 equations:

```hs
-- universal property of fold
G []     = e
G (x:xs) = f x (G xs) <=> G = fold f e

-- current equation
h . fold g w = fold f e

G = fold f e
G = h . fold g w
-- We substitute G on the left with (h . fold g w), getting these 2 equations
(h . fold g w) []     = e
(h . fold g w) (x:xs) = f x ((h . fold g w) xs)

-- simplifying
h (fold g w [])     = e
h (fold g w (x:xs)) = f x (h (fold g w xs))

-- (definition of fold)
fold f e []     = e
fold f e (x:xs) = f x (fold f e xs)

-- this can now be further simplified by these 2 calculations:
-- eq.1
h (fold g w []) = e
h w             = e                             {definition of fold}
-- eq.2
h (fold g w (x:xs))   = f x (h (fold g w xs))
h (g x (fold g w xs)) = f x (h (fold g w xs))   {definition of fold}
h (g x y)             = f x (h y)               {generalising y = fold g w xs}

-- we get (y = fold g w xs)
h w       = e
h (g x y) = f x (h y)
```

Using the universal property of fold we have calculated - without explicitly using induction - two simple conditions that are together sufficient to ensure, for all finite lists, that the composition of an arbitrary function `h` and a fold can be *fused together into a single fold*.

Following this interpretation, this property is called **the fusion property of the fold operator**, stated as:

```hs
-- the fusion property of the fold operator
h w       = e
h (g x y) = f x (h y)  ==>  h . fold g w = fold f e
```

As with the universal property, the primary application of the fusion property is as a proof principle that avoids the need for inductive proofs. In fact, for many practical examples the fusion property is often preferable to the universal property.

#### Example: fusing sum and fold

Consider again the equation:

```hs
(+ 1) . sum = fold (+) 1               { current equation }
```

In the previous section this equation was proved using the universal property of
fold. However, the proof is simpler using the fusion property.

First, we replace `sum` by its definition in terms of fold:

```hs
-- def of sum in terms of fold
sum :: [a] -> Int
sum = fold (+) 0

(+ 1) . fold (+) 0 = fold (+) 1        { def of sum in terms of fold }
h     . fold g   w = fold f e          { conclusion of fusion property }

h = (+ 1)
g = (+), w = 0
f = (+), e = 1

(+ 1) . sum = fold (+) 1               { current equation }
```

The equation now matches the conclusion of the fusion property, from which we conclude that the equation follows from the following two assumptions:

```hs
-- fusion property of fold
h w       = e
h (g x y) = f x (h y)  ==>  h . fold g w = fold f e

-- The current step matches the consequent of the fusion prop
-- we now substitute in the antecedent of the fusion property
h w       = e
h (g x y) = f x (h y)  ==>  (+ 1) . sum = fold (+) 1

-- with:
h = (+ 1), g = f = (+), w = 0, e = 1

-- obtaining the following
(+ 1) 0         = 1
(+ 1) ((+) x y) = (+) x ((+ 1) y)  ==>  (+ 1) . sum = fold (+) 1

-- the left side further simplifies to
1 + 0     = 1
1 + x + y = x + y + 1

-- and we finally get refl, thereby proving it
1 = 1
x + y = x + y
```

More generally, by replacing the use of (+) in this example by an arbitrary associative infix operator `⊕`, a simple application of fusion shows that:

```hs
-- top to bottom derivation
         sum        = fold (+)  0         { def of sum}
         sum        = fold (+)  b         { parameterizing }
(+ 1)  . sum        = fold (+)  1         { current eq }
(+ 1)  . sum        = fold (+)  (0 + 1)   { expanding }
(+ 1)  . sum        = fold (+)  (b + 1)   { expanding }
(+ a)  . sum        = fold (+)  (b + a)   { parameterizing }

-- bottom to top derivation
((+) 1)  . sum        = fold (+) 1         { current eq }
((+) 1)  . sum        = fold (+) (0 + 1)   { a = 1 }
((+) a)  . sum        = fold (+) (0 + a)   { b = 0 in sum = fold (+) b }
((+) a)  . sum        = fold (+) (b + a)   { sum = fold (+) b }
((+) a)  . fold (+) b = fold (+) (b + a)

((⊕) a) . fold (⊕) b = fold (⊕) (b ⊕ a)
```

#### Example: fusing map and fold

For a more interesting example, consider the following well-known equation, which asserts that `map` distributes over function composition:

>map f ∘ map g = map (f ∘ g)

By replacing the 2nd and 3rd occurrences of `map` in the equation by its definition in terms of fold, the equation can be rewritten in a form that matches the conclusion of the fusion property:

Equation to prove in terms of fusion:

```hs
map f . map g = map (f . g)
```

First, we recall the definition of map in terms of fold:

```hs
map :: (a -> b) -> [a] -> [b]
map f = fold ((:) . f) []
```

Now we replace the 2nd occurrence of `map` with its definition as fold:
```hs
map f . map g = map (f . g)
--      ↑↑↑↑↑
-- first we target 2nd map on the left side

map fn = fold ((:) . fn) [] -- map as fold
map g  = fold ((:) . g ) [] -- map as fold in terms of g (due to map g)

-- we replace: (map g) with (fold ((:) . g) [])
map f . (fold ((:) . g) []) = map (f . g)
--                            ↑↑↑↑↑↑↑↑↑↑↑
-- now we taget the map on the right side

map fn      = fold ((:) . fn)      []  -- map as fold
map (f . g) = fold ((:) . (f . g)) []  -- map as fold in terms of (f . g)

-- we replace: (map (f . g)) with (fold ((:) . (f . g)) [])
map f . (fold ((:) . g) []) = (fold ((:) . (f . g)) [])
```

We now have the following equation in a form that matches the conclusion of the fusion property:

```hs
map f . fold ((:) . g) [] = fold ((:) . (f . g)) []  -- this matches…
-- ↑           ↗        ↑          ↗              ↑
   h  . fold g'         w = fold f'               e  -- …conclusion of fusion

-- determined mappings:
h  = map f
g' = (:) . g
w  = []
f' = (:) . f . g
e  = []
```

Appealing to the fusion property and then simplifying, gives the final 2 equations, which are trivially true by the definitions of `map` and `(.)`.

```hs
-- antecedent of fusion  ==>  consequent of fusion
h w        = e           (1)
h (g' x y) = f' x (h y)  (2)  ==>  h . fold g' w = fold f' e

-- current equation
map f . fold ((:) . g) [] = fold ((:) . (f . g)) []

-- determined mappings:
h  = map f
g' = (:) . g
w  = []
f' = (:) . f . g
e  = []

-- We now recreate the 2 equations that make the antecedent of the fusion
-- property in terms of our current equation and determined mappings.

-- (1)
h w      = e
map f w  = e     -- h = map f
map f [] = e     -- w = []
map f [] = []    -- e = []

-- (2)
h     (g'        x y) = f'              x (h     y)
map f (g'        x y) = f'              x (map f y)  -- h = map f
map f (((:) . g) x y) = f'              x (map f y)  -- g' = ((:) . g)
map f (((:) . g) x y) = ((:) . (f . g)) x (map f y)  -- f' = ((:) . (f . g))

-- We get these two equations
map f [] = []                                          (1)
map f (((:) . g) x y) = ((:) . (f . g)) x (map f y)    (2)
```

Simplifying

```hs
-- (1)
map f [] = []

-- (2)
map f (((:) . g) x y)  =  ((:) . (f . g)) x  (map f y)
map f ((:) (g x) y)    =  ((:) . f . g) x  (map f y)
map f (g x : y)        =  (:) f (g x)     (map f y)
map f (g x : y)        =  f (g x) : map f y
map f (g x : y)        =  (f . g) x : map f y
map f (g x : y) = (f . g) x : map f y

-- The final two equations:
map f [] = []                                         (1)
map f (g x : y) = (f . g) x : map f y                 (2)
```

Gives us the final two equations:

```hs
map f [] = []
map f (g x : y) = (f . g) x : map f y
```

In addition to the fusion property, there are a number of other useful properties of the fold operator that can be derived from the universal property (Bird, 1998).

However, the fusion property suffices for many practical cases, and one can always revert to the full power of the universal property if fusion is not appropriate.


### 3.3 Universality as a definition principle

As well as being used as a proof principle, the universal property of fold can also be used as a definition principle that guides the transformation of recursive functions into definitions using fold.

#### sum

As an example, consider again the recursively defined function `sum` that calculates the sum of a list of numbers:

```hs
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

Suppose now that we want to redefine `sum` using fold. That is, we want to solve the equation `sum = fold f e` for function `f` and value `e`.

We begin by observing that the equation matches the right-hand side of the *universal property of fold*, from which we conclude that the equation is equivalent to the equations marked (1) and (2):

```hs
-- universal property
g []       = e               {- (L₁) -}
g (x:xs)   = f   x (g   xs)  {- (L₂) -}  <=>  g = fold f e  -- (R)
-- recursive sum def
sum []     = 0
sum (x:xs) = (+) x (sum xs)

-- we want to instead express sum as fold, i.e. solve: sum = fold f e
-- since g = sum, and find: f=? and e=?

-- sum matches the left side of the universal property, from which we
-- conclude that the equation is equivalent to these two equations:
sum []     = e               -- (1)  g = sum, e = 0
sum (x:xs) = f x (sum xs)    -- (2)  g = sum, f = (+)
```

From equation (1) and the definition of `sum`, it is immediate that `e = 0`. From equation (2), we calculate `f` which turns out to be `(+)`. Proof:

```hs
sum (x:xs) = f x (sum xs)
x + sum xs = f x (sum xs)   { by recursive def of sum }
x + y      = f x y          { generalizing: y = sum xs } †
(+) x y    = f x y          { (+) as infix }
(+) = f                     { eta contraction }
```

Using the universal property we have calculated that `sum = fold (+) 0`.

(†) Note that the key step in calculating the definition for `f` is the generalization of the exp `sum xs` to a fresh variable `y`. In fact, such a generalization step is not specific to `sum`, but will be a key step in the transformation of any recursive function into a definition using fold in this manner.

#### map

Of course, the sum example above is rather artificial, because the definition of sum using fold is immediate. However, there are many examples of functions whose definition using fold is not so immediate. For example, consider the recursively defined function `map f` that applies a function `f` to each element of a list:

```hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

To redefine `map f` using fold we must solve the equation `map f = fold g e` for a function `g` and value `e`.

By appealing to the universal property, we conclude that this equation is equivalent to the equations (1) and (2)

```hs
-- universal property
g     []     = e                {- (L₁) -}
g     (x:xs) = f x   (g    xs)  {- (L₂) -}  <=>  g = fold f e  -- (R)

-- rec def of map
map f []     = []
map f (x:xs) = f x : map f xs
map f (x:xs) = (:) (f x) (map f xs)
map f (x:xs) = ((:) . f) x) (map f xs) -- g = ((:) . f)

-- 2 new equations
map f []     = e                -- (1)  e = [], g = map f
map f (x:xs) = g x  (map f xs)  -- (2)  map f = g = fold f' e
--             g = ((:) . f)
```

From equation (1) and the definition of `map`, it is immediate that `e = []`. From equation (2), we calculate a definition for `g` as follows:

```hs
map f (x:xs)     = g x (map f xs)
f x : map f xs   = g x (map f xs)        { def of map }
f x : ys         = g x ys                { generalising ys = map f xs }
(:) (f x) ys     = g x ys                { generalising ys = map f xs }
(((:) . f) x) ys = g x ys                { infix to prefix }
((:) . f) x      = g x                   { eta contraction of ys }
(:) . f          = g                     { eta contraction of x }

-- using the universal property we have
map f = fold (\x ys -> f x : ys) []
map f = fold (\x ys -> (:) (f x) ys) []
map f = fold (\x -> (:) f x) []
map f = fold ((:) . f) []
```

Using the universal property we have calculated that:
>map f = fold ((:) . f) []


In general, any function on lists that can be expressed using the fold operator can be transformed into such a definition using the universal property of fold.

## 4. Increasing the power of fold: generating tuples

### sumlength

As an example of the use of fold to generate tuples, consider the function `sumlength` that calculates the sum and length of a list of numbers:

```hs
sumlength :: [Int] → (Int, Int)
sumlength xs = (sum xs, length xs)
```

By a straightforward combination of the definitions of the functions `sum` and `length` using fold given earlier, the function `sumlength` can be redefined as a single application of fold that generates a pair of numbers from a list of numbers:

```hs
sumlength = fold (\n (x, y) → (n + x, 1 + y)) (0, 0)
```

This definition is more efficient than the original definition, because it only makes a single traversal over the argument list, rather than two separate traversals.

Generalising from this example, *any pair of applications of fold to the same list can always be combined to give a single application of fold that generates a pair*, by appealing to the so-called 'banana split' property of fold (Meijer, 1992).

### dropWhile

Now consider the function `dropWhile p` that removes initial elements from a list while all the elements satisfy the predicate `p`:

```hs
dropWhile :: (a -> Bool) -> ([a] -> [a])
dropWhile p []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x : xs
```

Suppose now that we want to redefine `dropWhile p` using the fold operator.

By appealing to the *universal property*, we conclude that the equation `dropWhile p = fold f e` is equivalent to the following two equations:

```hs
-- dropWhile
dropWhile p []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x : xs

-- universal property of fold
g           []     = e
g           (x:xs) = f x (g xs)  <=>  g = fold f e

g = dropWhile p
-- but
g = fold f e
-- so
g = dropWhile p = fold f e

-- from which we get these 2 equations:
dropWhile p []     = e                             -- (1)
dropWhile p (x:xs) = f x (dropWhile p xs)          -- (2)

e = ?
f = ?
```

From equation (1), it is immediate that `e = []`. 
From equation (2), we attempt to calculate a definition for `f` as usual:

```hs
dropWhile p (x:xs) = f x (dropWhile p xs)    -- (2)
-- { def of dropWhile (second equation) }
dropWhile p (x:xs)   = if p x then dropWhile p xs else x:xs
-- { equality between the two is thus }
f x (dropWhile p xs) = if p x then dropWhile p xs else x:xs
-- { generalising, ys = dropWhile p xs }
f x ys               = if p x then ys else x:xs
```

Unfortunately, the final line is not a valid definition for `f`, because the variable `xs` occurs freely.

(A/N: but `p` does also? No, cos `ys = dropWhile p xs`. But then `xs` is covered if `p` is covered, no? Hmm..., well, it seems both `p` and `xs` are free, it's unclear why wasn't `p` also mentioned. Also, why not just revert to the equation before it where all vars are bound? That one doesn't define `f` or what. We'll see...).

>In fact, it is not possible to redefine `dropWhile p` *directly using fold*. However, it is possible indirectly,

In order to do it, we need to define a more general version of `dropWhile` that returns a pair of the resulting list and the original list. Then that more general version of `dropWhile` can be redefined using fold.

```hs
-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs

-- dropWhile' is a more general dropWhile def in terms of dropWhile
dropWhile' :: (a -> Bool) -> [a] -> ([a], [a])
dropWhile' p xs        = (dropWhile p xs, xs)

-- standalone def of a more general dropWhile' function
dropWhile' :: (a -> Bool) -> [a] -> ([a], [a])
dropWhile' p []     = ([], [])
dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs, xs)
```

By appealing to the universal property, we conclude that the equation `dropWhile' p = fold f e` is equivalent to the following two equations:

```hs
-- standalone def of a more general dropWhile' function
dropWhile' p []     = ([], [])
dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs, xs)

-- dropWhile' in terms of dropWhile
dropWhile' p xs     = (dropWhile p xs, xs)

-- universal property of fold
g           []      = e
g           (x:xs)  = f x (g xs)  <=>  g = fold f e

-- now we have these equalities:
g = dropWhile' p
-- but
g = fold f e
-- so
g = dropWhile' p = fold f e

-- from which we get these 2 equations:
dropWhile' p []     = ([], [])                       -- (1)
dropWhile' p (x:xs) = f x (dropWhile' p xs)          -- (2)

e = ?
f = ?
```

From equation (1), we calculate that `e = ([], [])`. 
From equation (2), we calculate a definition for `f` as follows:

```hs
-- [A] equation (2) from above
dropWhile' p (x:xs)                          = f x (dropWhile' p xs)

-- [B] def of dropWhile' in terms of dropWhile
dropWhile' p xs                              =     (dropWhile p xs, xs)

-- [C] substitute (dropWhile' p xs) with (dropWhile p xs, xs)
-- on the right side of the equation [B]
dropWhile' p (x:xs)                          = f x (dropWhile p xs, xs)

-- [D] substitute (dropWhile' p xs) with (dropWhile p xs, xs)
-- on the left side of the equation [C]
-- that is: (x:xs) --> (x:xs), (x:xs)
-- so xs := xs, xs
-- dropWhile' p xs                <=> (dropWhile p xs, xs)
-- dropWhile' p (x:xs)            <-- (dropWhile p xs, xs)
-- ls := (x:xs)
-- dropWhile' p ls                <-- (dropWhile p xs, xs)
-- \ls -> dropWhile' p ls         <-- \ls -> (dropWhile p ls, ls)
-- \(x:xs) -> dropWhile' p (x:xs) <-- \(x:xs) -> (dropWhile p (x:xs), (x:xs))
-- dropWhile' p (x:xs)            <-- (dropWhile p (x:xs), (x:xs))
(dropWhile p (x:xs), (x:xs))                 = f x (dropWhile p xs, xs)
-- tidy up
(dropWhile p (x:xs), x:xs)                   = f x (dropWhile p xs, xs)

-- [E] by def of dropWhile, which is
-- dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs
-- we get
(if p x then dropWhile p xs else x:xs, x:xs) = f x (dropWhile p xs, xs)

-- [F] generalising, ys = dropWhile p xs
(if p x then ys else x:xs, x:xs)             = f x (ys, xs)

-- so `f` is
f x (ys, xs) = (if p x then ys else x:xs, x:xs)
```

Note that the final line above is a valid definition for `f`, because all the variables are bound.

<!-- #region AN -->

<details><summary>Author's note</summary>

...except `p`. Again. At least `xs` is now bound. The last time we had

```hs
-- failed def of `f`. Its 2nd param is weird...
f x (dropWhile p xs) = if p x then dropWhile p xs else x:xs
-- ys = dropWhile p xs
f x ys = if p x then ys else x:xs
-- ...and now the 2nd param is ok, but `xs` is unbound
```

which wasn't good cos `xs` was unbound - even though we explicitly unbounded it by substituting `dropWhile p xs` with a fresh var `ys`.

This time, we had:

```hs
f x (dropWhile p xs, xs) = (if p x then dropWhile p xs else x:xs, x:xs)
-- ys = dropWhile p xs
f x (ys, xs) = (if p x then ys else x:xs, x:xs)
```
and now all is hunky dory (even though `p` remain free, but ok) The point is `f` is defined without mentioning `dropWhile` like the last time. This time it has proper params, `x` and `y`, where `y` is `(ys, xs)`.

</details>

<!-- #endregion -->

In summary, using the universal property we have calculated that:

```hs
dropWhile' p = fold f e
  where
  f x (ys, xs) = (if p x then ys else x : xs, x : xs)
  e = ([], [])
```

This definition satisfies the equation `dropWhile' p xs = (dropWhile p xs, xs)` without using the original `dropWhile` function in its definition. Hence, the function `dropWhile` itself can now be redefined by

```hs
dropWhile p = fst . dropWhile' p
```

In conclusion, by first generalising to a function `dropWhile'` that pairs the desired result with the argument list, we have now shown how the function `dropWhile` can be redefined in terms of fold, as required.

This result is an instance of a general theorem (Meertens, 1992) stating that
>any function on finite lists defined by pairing the desired result with the arg list can always be redefined in terms of fold, *although not always in a way that does not make use of the original (possibly recursive) definition for the function*.

## 4.1 Primitive recursion
