# Monad laws

The monad laws say nothing much more than that expressions involving `return` and `>>=` simplify, in just the way one would expect. There are 3 laws that can be stated in three different ways.

1. The first law states that `return` is a right identity element of `>>=`
    p >>= return = p

In do-notation the law reads:
    do {x <- p; return x} = do {p}

2. The second law says that return is also a kind of left identity element:
    return e >>= f = f e

In do-notation the law reads:
    do {x <- return e; f x} = do {f e}

3. The third law says that (>>=) is kind of associative:
  ((p >>= f) >>= g) = p >>= (\x -> (f x >>= g))

In do-notation the law reads:
      do {y <- do {x <- p; f x}; g y}
    = do {x <- p; do {y <- f x; g y}}
    = do {x <- p; y <- f x; g y}

The last line makes use of the un-nesting property of do-notation.


For the third way of stating the monad laws, consider *the fish operator*:

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
amb >=> bmc = \a -> amb a >>= bmc

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
bmc <=< amb = amb >>= bmc
```

This operator is just like function composition except that the component functions each have type `a -> m b` for appropriate `a` and `b`, and the order of composition is from left to right rather than from RTL as with `(.)`.

This operator, which is called *left to right* **Kleisli composition**, is defined in the Haskell library `Control.Monad`. Its  dual version is *right to left* Kleisli composition.

This is the shortest way of stating the monad laws.

The point is that we can define `>>=` in terms of `>=>`:

    p >>= f = (id >=> f) p

More briefly

    (>>=) = flip (id >=>)


We also have the **leapfrog rule**:

    (f >=> g) . h = (f . h) >=> g

In terms of `>=>`, the 3 monad laws say simply that `>=>` is associative with identity `return`. Any type with an associative binary operation and an identity element is called a *monoid*.
