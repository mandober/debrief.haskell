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
