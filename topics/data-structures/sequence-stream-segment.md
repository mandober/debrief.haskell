# Sequence, stream and segment
http://conal.net/blog/posts/sequences-streams-and-segments

Some discrete sequences are representations of something more essential, namely a flow of continuously time-varying values. Continuous models, whether in time or space, are often more compact, precise, adaptive and composable than their discrete counterparts.

FP offers great support for sequences of variable length. Lazy FP adds infinite sequences, often called streams, which allows for more elegant and modular programming. FP also has functions as first class values, and when the function's domain is (conceptually) continuous, we get a continuous counterpart to infinite streams.

Streams, sequences, and functions are 3 corners of a square:
- streams are               discrete and infinite
- sequences are             discrete and finite
- functions-on-reals are  continuous and infinite
- the missing corner is   continuous and finite, the topic of this post

## Streams

In the Wouter Swierstra's `Stream` library, a stream is an infinite sequence of values, defined as

```hs
data Stream a = Cons a (Stream a)

-- Stream is a functor and an applicative functor
instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
    pure  = repeat
    (<*>) = zipWith ($)

repeat :: a -> Stream a
repeat x = Cons x (repeat x)
```


## Comonads

Comonads are dual to monads. Comonads are like monads but wearing their category arrows backwards. In the `category-extras` package, the Comonad is defined: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/category-extras

An intuitive description is that monads describe values in context. `return` just (uneffectful) injects a pure value into a monadic context.

```hs
return  :: Monad m     => a -> m a
```

The dual to monadic return is `extract`, also known as *counit* or *coreturn*, which extracts a value out of a comonadic context, discarding the value's context; e.g. `extract` to streams is what `head` is to lists.

The `category-extras` library relocates this method from the `Comonad` into the `Copointed` class:

```hs
extract :: Copointed w => w a -> a
```

Monadic values are typically produced in effectful computations: 
`a -> m b`.    
Comonadic values are typically consumed in context-sensitive computations: 
`w a -> b`.    

>  Kleisli arrows wrap the producer pattern     
> CoKleisli arrows wrap the consumer pattern


Monads have a way to extend a monadic producer into one that consumes the entire monadic value. (this operation in most often in the flipped form, which obscures the conceptual distinction between Haskell arrows and arbitrary category arrows).

```hs
(=<<) :: (Monad m) => (a -> m b) -> (m a -> m b)

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

Dually, comonads have a way to extend a comonadic consumer into one that produces an entire comonadic value (which also has a flipped version):

```hs
extend :: (Comonad w) => (w a -> b) -> (w a -> w b)
-- = > >
(=>>)  :: (Comonad w) => w a -> (w a -> b) -> w b
```


Another view on monads is as having a way to join two monadic levels into one. 
Dually, comonads have a way to duplicate one level into two:

```hs
join      :: (Monad   m) => m (m a) -> m a

duplicate :: (Comonad w) => w a -> w (w a)
```

For a monad, any of `join`, `=<<`, and `>>=` can be used to define the others.

For a comonad, any of `duplicate`, `extend`, and `=>>` can be used to define the others.


### The Stream comonad

The Stream library already has functions of the necessary types for `extract` and `duplicate`, corresponding to familiar list functions:

```hs
head :: Stream a -> a
head (Cons x _ ) = x

tails :: Stream a -> Stream (Stream a)
tails xs = Cons xs (tails (tail xs))
where

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs
```

Indeed, `head` and `tails` are just what we're looking for.

```hs
instance Copointed Stream where extract   = head
instance Comonad   Stream where duplicate = tails
```

There is also a `Monad` instance for `Stream`, in which `return` is `repeat` (matching `pure` as expected) and `join` is diagonalization, producing a stream whose nth element is the nth element of the nth element of a given stream of streams.

Exercise: The indexing function (!!) is a sort of semantic function for Stream. Show that (!!) is a morphism for Functor, Applicative, Monad, and Comonad. In other words, the meaning of the functor is the functor of the meanings, and similarly for the other type classes. The Comonad case has a little wrinkle. See the posts on *type class morphisms*:    
http://conal.net/blog/tag/type-class-morphism/
