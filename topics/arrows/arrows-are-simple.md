# Arrows are simpler than they appear

http://www.newartisans.com/2012/10/arrows-are-simpler-than-they-appear/

Arrows are simply abstractions of functions. To see how they are useful, consider the composition of a set of functions, among which some are pure and some are monadic.

```hs
f :: a -> b
g :: b -> m c
h :: c -> 𝓂 d
```

Knowing each of the types involved, we could build a composition by hand, but the output type of the composition would have to reflect the intermediate monad types; in the case above, `m (𝓂 d)`.

Could we just treat functions as if they were `a -> b`, `b -> c`, and `c -> d`.

> We use arrows to abstract the presence of monads and reason only about the underlying types.

Here's an arrow which abstracts the presence of the `IO` for functions in the IO monad, such that we can compose them with pure functions without the user needing to know that IO is involved. We start by defining an `IOArrow` to wrap the IO functions.

```hs
data IOArrow a b = IOArrow { runIOArrow :: a -> IO b }

instance Category IOArrow where
  id = IOArrow return
  IOArrow f . IOArrow g = IOArrow $ f <=< g

instance Arrow IOArrow where
  arr f = IOArrow $ return . f
  first (IOArrow f) = IOArrow $ \(a, c) -> do
    x <- f a
    return (x, c)
```
