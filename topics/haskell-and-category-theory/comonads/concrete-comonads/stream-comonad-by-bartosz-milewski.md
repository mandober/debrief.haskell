# The Stream Comonad
https://bartoszmilewski.com/2017/01/02/comonads/



This process of shifting the focus from one element of the container to another is best illustrated with the example of an infinite stream. A stream is like an infinite list, i.e. like the regular list without the empty constructor. It is trivially a Functor.

```hs
data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Cons a as) = Cons (f a) (fmap f as)
```

*The focus of a stream is its first element*, so definition of `extract` follows; `duplicate` produces a stream of streams, each focused on a different element.

```hs
extract :: Stream a -> a
extract (Cons a _) = a

duplicate :: Stream a -> Stream (Stream a)
duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
```

In the resulting stream of streams produced by `duplicate`, the first element (stream) is the original stream, the second element (stream) is the tail of the original stream, the third element (stream) is its tail, and so on, ad infinitum.

The complete `Comonad` instance for `Stream`s:

```hs
instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
```


This is a very functional way of looking at streams. In an imperative language, we would probably start with a method `advance` that shifts the stream by one position, but here, `duplicate` produces all shifted streams in one fell swoop. Haskell's laziness makes this possible and even desirable.

To make it practical, we also implement the analog of the imperative advancement thorugh a stream; but it's never part of the comonadic interface.

```hs
tail :: Stream a -> Stream a
tail (Cons a as) = as
```


If you had any experience with digital signal processing, you'll see immediately that a co-Kleisli arrow for a stream is just a digital filter, and `extend` produces a filtered stream.


As a simple example, let's implement *the moving average filter*. The `sumS` sums `n` elements of a stream, `average` calculates the average of the first `n` elements of the stream. Partially applied `average n` is a co-Kleisli arrow, so we can extend it over the whole stream with `movingAvg`. The result is the stream of running averages.

```hs
sumS :: Num a => Int -> Stream a -> a
sumS n (Cons a as) = if n <= 0 then 0 else a + sumS (n - 1) as

average :: Fractional a => Int -> Stream a -> a
average n stm = (sumS n stm) / (fromIntegral n)

movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n = extend (average n)
```

A stream is an example of a unidirectional, one-dimensional comonad. It can be easily made bidirectional or extended to two or more dimensions.
