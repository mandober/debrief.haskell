# Composition

## Function composition

In Haskell, composition of functions follows the same principles as composition of functions in mathematics and category theory, in particular, which extends and generalizes the notion of composition.

Regular composition of functions is denoted by the symbol `.`, which is telling of the fact that we're in a functional language - the simplest, and most convenient symbols to type, are reserved for things that occur the most often (compare this to the use of dot in OOP). Along this line, function application uses no symbol at all, i.e. it is denoted with the space character (spaced-out juxtaposition), e.g. `f a b c `, or it may be explicitly denoted with the symbol `$` that has a slighly different semantics, e.g. `f a (b c) = f a $ b c`.

The composition itself follows math in its *right-to-left* semantics, so `g ∘ f` in math is `g . f` in Haskell, and both are read as "g after f". This notation is sometimes cumbersome since it is reversed - we normally read from left to right (L2R), but the composed functions are applied from right to left (R2L).

For example, `h ∘ g ∘ f` applied to an arg `x` is `h(g(f(x)))` in math and it's `h (g (f x))` in Haskell. That is, the rightmost function (here `f`) is applied first to the arg `x`, and then the result of `f(x)` is fed as an arg `y` (where `y = f(x)`) into the next function on the left (here `g`); then the result of `g(y)` is fed as an arg `z` (where `z = g(y) = g(f(x)))`) to the final, leftmost function `h`, which produces the overall result, `h(z)`.

`g ∘ f`

Function `g` is said to be *pre-composed* with function `f`, and function `f` is said to be *post-composed* with function `g`, and the application to an argument `x` is the same (up to syntax): `g(f(x))` in math vs `g (f x)` in Haskell.

## Composition of pure functions




## misc

⟳ ⟲ -- ↺ ↻ -- ⤸ ⤹ -- ⥀ ⥁


```hs
-- | Right-to-left composition, .⃗ , b⃗ , c⃗
(⥁) :: (b -> c) -> (a -> b) -> a -> c
(⥁) = (.)

b⃗ :: (b -> c) -> (a -> b) -> a -> c
b⃗ = (.)

-- ----------------------------------------------------------------------------
-- | Left-to-right composition, .⃖ , b⃖, c⃖, ○⃗
(⥀) :: (a -> b) -> (b -> c) -> a -> c
(⥀) = flip (.)

b⃖ :: (a -> b) -> (b -> c) -> a -> c
b⃖ = flip (.)

-- ----------------------------------------------------------------------------
f,g :: a -> Integer
f = const 0
g = const 1

h1,h0 :: Integer
h0 = (f ⥁ g) () -- 0    f (g x) = f . g
h1 = (f ⥀ g) () -- 1    g (f x) = g . f
```
