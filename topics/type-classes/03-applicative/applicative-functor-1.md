# Applicative functor

Introducuction of applicative functors, which further generalise the idea of mapping to functions with more than one argument.

The *applicative style of programming* is one in which pure functions are applied to *effectful arguments*.


## Generalizing fmap

The `map` function on list maps a list, given a unary function and a list, by applying the function to all elements in the list.

Generalizing mappable structures (container types), we get functors, in that a functor is a container or context that can be mapped over. In fact, the term functor is overloaded: it may mean the structure itself, especially a type that is an instance of the Functor class, or it may mean a whole class of types that are functors (and hence have instances of the Functor class).




and based on in the generalized mapping function, `fmap`, that can map any functor with a unary function.

```hs
fmap0 :: a -> f a
fmap  :: (a -> b) -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

This is generalizing the idea of mapping to functions with two, three and more arguments.

To derive the Applicative class and its two methods, i.e. 

To have an Applicative that will generalize function application and allow us to apply functions with any number of args to a functorial structure, we need just two functions, called `pure` and `<*>`.

```hs
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
```

`pure` is the same as fmap0 above, i.e. it represents the situation when we don't even have a function, but just a constant value (it would be `b` from the function type `a -> b`, but it is renamed to `a`).

`pure` allows us to replace all values in the structure with the same value. No! But `(<$) :: Functor f => a -> f b -> f a` does; it is by default defined as `fmap . const`.

`pure` just lifts a value to a structure, e.g. a value 4 into [4] in terms of the list applicative, or 4 into `Just 4` in terms of the Maybe applicative.

Applicative style

```hs
-- applicative style
pure f <*> x <*> y <*> z
-- compared to normal fn application
f x y z
```


Using `pure` and `<*>`, we can define any mapping operations, no matter how many args the function to apply has.

```hs
fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 f fa = pure f <*> fa

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = pure f <*> fa <*> fb

fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

```

- `pure x` lifts a value    `x :: a`           into type `f a`
- `pure g` lifts a function `g :: a -> b`      into type `f (a -> b)`
- `pure g` lifts a function `g :: a -> b -> c` into type `f (a -> b -> c)`
- and so on; the type param `f` is a type ctor








## Ref

* Applicative Functors by Graham Hutton
https://www.youtube.com/watch?v=8oVHISjS3wI

* Haskell by Graham Hutton - complete playlist
https://www.youtube.com/playlist?list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc
