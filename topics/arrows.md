# Arrows

https://www.haskell.org/arrows/


## Arrow tutorial

https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial




## Understanding arrows

https://en.wikibooks.org/wiki/Haskell/Understanding_arrows

Arrows, like monads, express computations that happen within a context. However, they are a more general abstraction than monads, and thus allow for contexts beyond what the Monad class makes possible. The essential difference between the abstractions can be summed up thus:

> Just as we think of a monadic type `m a` as representing a "computation delivering an `a`", so we think of an arrow type `a b c` (that is, the application of the parameterised type `a` to the two parameters `b` and `c`) as representing "a computation with input of type `b` delivering a `c`". Arrows make the dependence on input explicit. 
-- John Hughes in "Generalising Monads to Arrows"

Arrows are similar to functions. Like the function type ctor, `->`, the type ctor of an `Arrow` instance has kind `* -> * -> *`, i.e. it takes two type args. Crucially, the `Arrow` class has the `Category` as the superclass. The `Category` is, roughly, the class for things that can be composed like functions.

```hs
class Category (y :: * -> * -> *) where
  id  :: y a a                     -- identity for composition
  (.) :: y b c -> y a b -> y a c   -- associative composition
```
