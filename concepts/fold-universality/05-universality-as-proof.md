# Universality and expressiveness of fold

(A tutorial on the universality and expressiveness of fold" by Graham Hutton)


## Universality as a proof principle

The primary application of the universal property of fold is as a proof principle that avoids the need for inductive proofs. 

As a simple first example, consider the following equation between functions that process a list of numbers:

```haskell
(+1) . sum = fold (+) 1
```

The LHS function sums a list and then increments the result. The RHS function processes a list by replacing each (:) by the addition function (+) and the empty list [] by the constant 1. The equation asserts that these two functions always give the same result when applied to the same list.

To prove the above equation, we begin by observing that it matches `g` on the RHS of the universal property of fold:

```haskell
g []     = v           (1)
g (x:xs) = f x (g xs)  (2)  ⇔   g     = foldr  f   v
                                 ↓              ↓   ↓  it's a match!
                            (+1) . sum = foldr (+)  1
so we have:
g = (+1) . sum
f = (+)
v = 1
```

Hence, by appealing to the universal property, we conclude that the equation to be proved is equivalent to the following two equations:

```haskell
      g        []   = v                              (1)
((+1) . sum)   []   = 1                              (1.a)

      g      (x:xs) =  f  x (       g       xs)      (2)
((+1) . sum) (x:xs) = (+) x ( ((+1) . sum)  xs)      (2.a)
```

Now, we simplify using the definitions of composition and sectioning:

```haskell
sum []     + 1 = 1                                   (1.b)
sum (x:xs) + 1 = x + (sum xs + 1)                    (2.b)
```

## Again, case by case

First, the definitions of composition and sectioning:

- composition: `f . g (x) = f (g x)`
- sectioning: 
  - `(l⊛) x ≡ (\x -> l ⊛ x)`
  - `(⊛r) x ≡ (\x -> x ⊛ r)`
  - `(+1) x = x + 1`


```haskell
-- (1)
      g      [] = v
((+1) . sum) [] = 1
((+1) (sum [])  = 1     -- by def of composition
sum [] + 1      = 1     -- by def of sectioning
0      + 1      = 1     -- by def of sum = fold (+) 0 ~> sum [] = 0
              1 = 1     -- QED (1)

-- (2)
      g      (x:xs) =  f  x (      g      xs)
((+1) . sum) (x:xs) = (+) x (((+1) . sum) xs)
(+1) (sum (x:xs))   = (+) x ( (+1) (sum xs) ) -- by def of composition
sum (x:xs) + 1      = x + (sum xs + 1)        -- by def of sectioning
(x + sum xs) + 1    = x + (sum xs + 1)        -- by def of sum (x:xs)=x+sum xs
x + (sum xs + 1)    = x + (sum xs + 1)        -- assoc of addition
x + sum xs + 1      = x + sum xs + 1          -- QED (2)
```

This completes the proof. Normally this proof would have required an explicit use of induction. However, in the above proof the use of induction has been encapsulated in the universal property of fold, with the result that the proof is reduced to a simplification step followed by two simple calculations.

In general, any two functions on lists that can be proved equal by induction can also be proved equal using the universal property of the fold operator, provided, of course, that the functions can be expressed using fold. The expressive power of the fold operator will be addressed later on in the article.
