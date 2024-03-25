# Streams

A stream is an infinite data structures, a type of codata, as opposite to the finite data. Data is finitary and introduced using recursion and eliminated via induction. *Codata* is infinite; it is introduced using *corecursion* and eliminated using *coinduction*.

In Haskell, a stream is an infinite list. It may have the same type declaration as a list but without the `Nil` data ctor. That way the construction of an empty stream (list) is impossible, but so piece-by-piece construction. Like an infinite list (which cannot be detected in Haskell in order to e.g. convert it into a stream), a stream is usually construted using recursion without the base case, or using unbounded range expressions (the support for corecursion and coinduction, and codata, in general, is a demanding language feature, not currently present in Haskell).

```hs
-- infinite lists

-- unbounded range expression
nats     = [0 ..]             -- [0,1,2,…]
evens    = [0,2 ..]           -- [0,2,4,…]
odds     = [1,3 ..]           -- [1,3,5,…]

-- baseless recursion
inflist1 = 0 : inflist1       -- [0,0,0,…]
inflist2 = 0 : 5 : inflist2   -- [0,5,0,5,…]

-- functions
inflist3 = cycle [3,5,7]      -- [3,5,7,3,5,7,…]
inflist3 = iterate (* 3) 1    -- [1,3,9,27,81,243,729,2187,6561,…]
inflist4 = iterate' (* 5) 1   -- [0,5,10,15,20,25,30,35,40,45,…]
```



A `Stream` is an examplary instance of the `Comonad` class. 


```hs
data Stream a where
  (:>) :: a -> Stream a -> Stream a
  deriving (Eq, Show, Functor, Foldable)

instance Comonad Stream where
  extract :: Stream a -> a
  extract (x :> xs) = x

  duplicate :: Stream a -> Stream (Stream a)
  duplicate = streamS
```
