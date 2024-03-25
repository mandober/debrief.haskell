# Fold-fusion theorem

https://flolac.iis.sinica.edu.tw/flolac07/files/handouts/mu-derivation-handouts.pdf

The *Fold-fusion theorem* is about whether a (pre-)composition of a function and a fold can be expressed as a fold.

Fold-Fusion Theorem:
  given     
    `f :: a → b → b`     
    `e :: b`             
    `h :: b → c`         
    `g :: a → c → c`     
  we have     
    __h . foldr f e = foldr g (h e)__     
  if `∀ x y`     
    *h (f x y) = g x (h y)*


>if   forall x y.
>       h (f x y) = g x (h y)
>then
>       h . foldr f e <=> foldr g (h e)


The `h` precomposed with `foldr` as 
`h . foldr f e` 
could be fused into a fold 
`foldr g (h e)` 
if we can find the function `g` such that 
forall x y. 
`h (f x y) = g x (h y)`

Smells like prim rec, doesn't it?
- hᵏ = ρ(fᵏ⁻¹,gᵏᐩ¹)

- `h¹    0  = f⁰`
- `h¹ (S x) = g² x (h¹ x)`

- `h²    0  y = f¹ y`
- `h² (S x) y = g³ x (h² x y) y`



So, for a program derivation, we are usually given `f`, `e` and `h` and we have to find `g`.


### Tracing an example

```hs
  h (foldr f e [a,b,c])
                        { by definition of foldr }
= h (f a (f b (f c e)))
                        { since h (f x y) = g x (h y) }
= g a (h (f b (f c e)))
                        { since h (f x y) = g x (h y) }
= g a (g b (h (f c e)))
                        { since h (f x y) = g x (h y) }
= g a (g b (g c (h e)))
                        { by definition of foldr }
= foldr g (h e)[a,b,c]
```

### Sum of Squares

Now consider `sum . map square`     
using the fact that    
`map f = foldr (mf f) []`      
  where `mf f x xs = f x : xs`

```hs
square :: Integer -> Integer
square x = x * x

sumsquares :: [Integer] -> Integer
sumsquares = sum . maps square

maps :: (a -> b) -> [a] -> [b]
maps f = foldr (mf f) []

mf :: (a -> b) -> a -> [b] -> [b]
mf f x xs = f x : xs
```

`sum . map square` is a fold,    
if we can find `ssq` (as g) such that    
`ssq x (sum xs)` = `sum (mf square x xs)`.


Let's try:

```hs
sum (mf square x xs)
                        = { definition of mf }
sum (square x : xs)
                        = { definition of sum }
square x + sum xs
                        = { let ssq x y = square x + y }
ssq x (sum xs)
```

Therefore, `sum . map square = foldr ssq 0` where `ssq x (sum xs)`
