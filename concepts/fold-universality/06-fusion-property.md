# Universality and expressiveness of fold

(A tutorial on the universality and expressiveness of fold" by Graham Hutton)

## The fusion property of fold

Let's generalize the previous example with sum and consider the following equation between list processing functions:

h . fold g w = fold f v

This pattern of equation occurs frequently when reasoning about programs written using fold. It is not true in general, but we can use the universal property of fold to calculate conditions under which the equation will be true.

The equation matches the right-hand side of the universal property, from which we conclude that the equation is equivalent to the following two equations:

```haskell
g []     = v           (1)
g (x:xs) = f x (g xs)  (2)  ⇔   g     = foldr  f   v
                                ||       ↓      ↓   ↓  it's a match!
                          h . fold g w = foldr  f   v
-- so we have:
g = h . fold g w

-- replacing g with its value in the universal property equations yields:

g              []  = v                    (1)  -- universal property (1)
(h . fold g w) []  = v                    (1)  -- replacing g by (h . fold g w)
h   (fold g w  []) = v                    (1)  -- by def of composition
h           w      = v                    (1)  -- by def of foldr
h w = v                                   (1)  -- END

g              (x:xs)  = f x (g xs)                      (2)
(h . fold g w) (x:xs)  = f x ((h . fold g w) xs)         (2)
h   (fold g w  (x:xs)) = f x ( h  (fold g w  xs))        (2)
h (g x (fold g w xs))  = f x (h   (fold g w xs) )        (2)
-- substituting: y = fold g w xs
h (g x        y      ) = f x (h          y      )        (2)
h (g x y) = f x (h y)                                    (2)  -- END

-- we obtain these 2 equations:
h w       = v
h (g x y) = f x (h y)
```

Using the universal property of fold we have calculated 
(without an explicit use of induction) 
the two conditions that are together sufficient to ensure that 

> the composition of 
an arbitrary function 
and a fold 
can be fused together 
to give a single fold.

Following this interpretation, this property is called **the fusion property of the fold operator**, and can be stated as follows:


```haskell
h w       = v
h (g x y) = f x (h y)     ⇒     h . fold g w = fold f v
       ↑           ↑
  (for y = fold g w xs)
```

As with the universal property, *the primary application* of the fusion property is as a proof principle that avoids the need for inductive proofs. In fact, for many practical examples, the fusion property is often preferable to the universal property.

As a simple first example, consider again the equation:

(+1) · sum = fold (+) 1

We have demonstrated the proof above using the universal property of fold, but the proof gets much simpler using the fusion property instead.
