# Point-free

Point-free tips and reminders

1. Last declaration occurrence + last application occurrence (of a param)

```hs
-- 1.1
h1,h2,h3,h4 :: (a -> b -> c) -> a -> b -> c
h1 g x y = g x y
h2 g x   = g x
h3 g     = g
h4       = id

-- 1.2
w1,w2 :: (a -> b) -> (c -> a) -> c -> b
w1 g f x = g (f x)
w2 g f   = g . f

g1,g2,g3,g4 :: 
g1 g f x = g x $ f x
g2 g f   = g x $ f
```


```hs
func f   = f   ≡
func     = id
```



```h
kmap :: (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r)
kmap f k g = k \x -> g $ f x
kmap f k g = k $ g . f
kmap f k g = k $ (. f) g
kmap f k   = (k $) . (. f)
```

# Index of Invariants


Converting an expresion between point-full and point-free style

```hs
divisorsMin :: Integral a => a -> [a]
-- point-free
divisorsMin = tail . init . divisors
-- point-full
divisorsMin xs = tail $ init .$ divisors xs
```



```hs
 g a . f      -- composition
(g . f) x     -- composition must be parethesised if arg introduuced
 g $ f x      -- which is the same as (.) becoming a ($)
 g ( f x )
```

Composing a binary function g partially applied to an arg x and a unary function f, as in `g x . f`, does not require parenthesis because function application has the precedence (infixl -1).

- infixl -1     (function application is left-assoc)
- infixr  0 $   ($ is right-assoc)
- infixr  9 .   (. is right-assoc)


CS 3110 Design and Analysis of Algorithms - Lecture Notes
