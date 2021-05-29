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
