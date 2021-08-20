# Arrows

https://en.wikibooks.org/wiki/Haskell/Understanding_arrows
https://en.wikibooks.org/wiki/Haskell/Solutions/Understanding_arrows

Functions are similar to arrows. Like the function type ctor, `->`, the type ctor of an `Arrow` instance has kind `* -> * -> *`; that is, it takes two type arguments − unlike, a Monad, which takes only one.

The `Arrow` class has the `Category` as the superclass. 
The `Category` is the class for things that can be composed like functions.

```hs
class Category (y :: * -> * -> *) where
  id  :: y a a                     -- identity for composition
  (.) :: y b c -> y a b -> y a c   -- composition is associative
```

The function ctor (->) is an instance of `Category` and `Arrow` class as well.

```hs
instance Category (->) where
  -- id  :: (->) a a
  id  :: a -> a
  id = Data.Function.id

  -- (.) :: (->) b c -> (->) a b -> (->) a c
  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  (.) = (Data.Function..)
```

A practical consequence of this similarity is that you have to think in point-free terms when looking at expressions full of Arrow operators, otherwise you will quickly get lost looking for the values to apply things on.

```hs
-- point-less-ness
(total &&& (const 1 ^>> total)) >>> arr (uncurry (/))
```

In any case, you are already lost; even if you look at arrow expressions in the right way, you're lost either way, little girl.

To ameliaorate this problem, the `proc` notation was invented. It adds extra variable names and whitespace, while making some operators implicit to ease the reasoning about the Arrow code.

`Control.Category` also defines `(<<<) = (.)` and `(>>>) = flip (.)` used for arrow composition in both directions (ltr and rtl).

## Arrow is between Applicative and Monad
