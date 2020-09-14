# Invariants


(g ∘ f) x  ⇔
 g $ f  x
- When an arg is introduced, the composition, (`.`) becomes (`$`)
- When an arg is abstracted (in point-free),  (`$`) becomes (`.`)


¿
⁈
⁇



```hs
 g a . f      -- composition
(g . f) x     -- composition must be parethesised if arg introduuced
 g $ f x      -- which is the same as (.) becoming a ($)
 g ( f x )
```

When composing a binary function, `g`, that is partially applied to an arg `a`, denoted here as `g a`, with another unary function `f`, do not stress about parenthesis because function application binds the tightest.

- infixl -1     function application, `f x` is left-assoc.
- infixr  0 $   more precedent fn application is right-assoc.
- infixr  9 .   composition is right-assoc.


BTW, fn application binds the tightest
