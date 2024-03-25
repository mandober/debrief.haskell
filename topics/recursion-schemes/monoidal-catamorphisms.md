# Monoidal catamorphisms

Monoidal catamorphisms - Recursion Schemes with Monoids

Monoidal Catamorphisms - Bartosz Milewski, at λC 20 Global Edition, LambdaConf
https://www.youtube.com/watch?v=-bHU7fPUP6E&list=TLPQMDQwMTIwMjMKngo8iH4tLQ

You can fold a list by converting list elements to monoidal values and then appending them together. The folding itself is agnostic of the monoid, so you can hide the monoid by wrapping it in an existential type. However, *folding* is just a special case of a *catamorphism*. When you combine the two you get a *monoidal catamorphism*.


```hs
data Fold a b where
  Fold :: forall a b m. Monoid m => (a -> m) -> (m -> b) -> Fold a b

fold :: forall a b. Fold a b -> [a] -> b
fold (Fold f g) = g . mconcat . fmap f

instance Functor (Fold r) where
  fmap :: (a -> b) -> Fold r a -> Fold r b
  fmap ab (Fold rm ma) = Fold rm (ab . ma)


class Monoidal f where
  init    :: f ()
  combine :: f a -> f b -> f (a, b)
```

## Monoidal functor

Being a monoidal factor means that you can take two folds and multiply them in some sense. So if you have a fold for one monoid and another fold for another monoid, you can combine them into pairs.

A monoidal factor is a data type that defines the method called `init`, that is a container with a unit type; and the method `combine` that allows you to combine two containers, one containing `a`'s and one containing `b`'s, to a single container that contains `(a,b)` pairs.
