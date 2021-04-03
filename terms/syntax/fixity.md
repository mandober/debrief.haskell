# Fixity




- 3 infix keywords: `infixl`, `infix`, `infixr`
- fixity is precedence and associativity, rolled into one
- associativity may be left `infixl`, right `infixr`, or neutral `infix`
- precedence is defined by the 0-10 scale (lo-hi)
- function application has the biggest precedence (10)
- fn ctor (->) aka fn application is `infixr -1 ->`
- eponymous type and data ctor must have same fixity
- *has-type* (`::`) has the lowest precedence, f x :: Int == (f x) :: Int
- precedence of TypeApplications, `map @Int @[]`

`infix{,l,r} n`

NOTE: *maximal munch rule*: conditionals, let-expressions, do-expressions and lambda abstractions extend to the right as far as possible (respecting unbalanced parenthesis).


**Fixity** is Haskell's economic term that denotes associativity and precedence rolled into a single keyword (the less keywords the better).

Fixity primarily affects (infix) operators. Each operator is assigned a precedence level, expressed as a number in range 0-10. The lower the number the tighter an operator "grips" to its arguments. Function application, the mother of all operations, has the special fixity `infixl -1`, meaning it is the strongest (assured by the compiler that assigns it the out-of-range precedence of -1), therefore *it is evaluated first*. 

```hs
fx x y z = 104 + f x / 3 * g y - 2 * h z
    where f = (+9)
          g = (2^)
          h = (7*)
```





the direction 

the strength number of their

and it may specify an operator's associativity, but it must specify its precedence.

as being left or right (or leave it unspecified/neutral), 



## Operators

Operators with unspecified associativity

Right-associative operators

Left-associative operators



## Application

* unsigned (default) function application:
  - a b c d ⇔ a (b (c d)) ⇔ a $ b $ c d
  - f x . g ~~> (f x . g) y ⇔ f x $ g y
* lazy function application
  - a b c d
* strict function application
  - a b c d
* composition
  - a b c d
* lifted application
  - a b c d


fixity       | dir   | s | desc                 | example
-------------|-------|---|----------------------|---------------------------
infixl -1 -> | left  | ∞ | function application | a b c d ⇔ a (b (c d)) ⇔ a $ b $ c d
infixr  9 .  | right | 9 | function composition | a . b . c ⇔ a . (b . c)




infixr 9 . function composition

infixr 8  ^, ^^, **
infixl 7  *, %, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  ++

infixl 4  <$, <*>, <*, *>, <**>
infixl 1  >>=, >>
infixr 1  =<<

infixr 0  $  lazy function application
infixr 0  $! strict function application
infixl -1 ->


## Operators with unspecified associativity



## Right-associative operators

- Right-associativity has the `r` suffix: *infixr*

infixr -1 ->

a -> b -> c ≡ a -> (b -> c)

Either a b -> Reader r b -> Parser (p -> q)



a $  b    c = a $  (b    c)
a .  b .  c = a .  (b .  c)
a ^  b ^  c = a ^  (b ^  c)
a ++ b ++ c = a ++ (b ++ c)


## Left-associative operators

- Right-associativity has the `r` suffix: *infixl*

a + b + c = (a + b) + c
a   b   c = (a   b)   c
