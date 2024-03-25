# The laws

## map distributivity

```hs
map  (f . g) =  map f .  map g
fmap (f . g) = fmap f . fmap g
```

## map promotion

```hs
map f . concat = concat . map (map f)
    f . head   = head   .      map f

-- in general
f . g = g . map f
  where
  g = head, tail, init, last, filter p
  g ≠ sum, product
```






map f = foldl ((:) . f) []                        -- map as fold

map f (xs ⨂ ys) = map f xs ⨂ map f ys
map f (xs ++ ys) = map f xs ++ map f ys
map f (g xs ys) = g (map f xs) (map f ys)



## Horner's rule

```hs
maximum . map sum . tails ≡ foldl (⨂) 0   -- Horner's rule


-- hmm, this checks out
x1 = [1,2,3,4]
x2 = tails x1
x3 = map sum x2
x4 = maximum x3
x4 ≡ fold (+) 0 x1
```


## Homomorphism lemma

A function `h` on lists is called a **list homomorphism** if there exists an associative binary operator `⊕` and neutral element `e` such that this holds:

```hs
h ([] ++ []) = e
h (xs ++ ys) = h xs `g` h ys

h = foldl g e . map f
```

The **homomorphism lemma** states that `h` is a homomorphism iff there exists an operator `⊕` and a function `f` such that `h = foldl g e . map f`.
