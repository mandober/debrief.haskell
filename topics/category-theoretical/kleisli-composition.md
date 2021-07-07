# Kleisli composition

For the third way of stating the monad laws, consider *the fish operator*:

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
amb >=> bmc = \a -> amb a >>= bmc

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
bmc <=< amb = amb >>= bmc
```

This operator is just like function composition except that the component functions each have type `a -> m b` for appropriate `a` and `b`, and the order of composition is from left to right rather than from RTL as with `(.)`.

This operator, which is called *left to right* **Kleisli composition**, is defined in the Haskell library `Control.Monad`. Its  dual version is *right to left* Kleisli composition.


The point is that we can define `>>=` in terms of `>=>`:

    p >>= f = (id >=> f) p

More briefly

    (>>=) = flip (id >=>)


We also have the **leapfrog rule**:

    (f >=> g) . h = (f . h) >=> g

In terms of `>=>`, the 3 monad laws say simply that `>=>` is associative with identity `return`. Any type with an associative binary operation and an identity element is called a *monoid*.
