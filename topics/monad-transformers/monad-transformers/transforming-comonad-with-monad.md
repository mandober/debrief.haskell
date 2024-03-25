---
downloaded:       2022-01-03
page-url:         http://blog.sigfpe.com/2008/03/transforming-comonad-with-monad.html
page-title:       A Neighborhood of Infinity: Transforming a comonad with a monad
article-title:    Transforming a comonad with a monad
---
# A Neighborhood of Infinity: Transforming a comonad with a monad

I can't yet read most of this paper on Combining a Monad and a Comonad because I've no experience with 2-categories. Nonetheless, there's something useful I can extract from the bits that do make sense - the idea of combining a monad with a comonad in the way that we already combine monads using monad transformers. In my previous post I hinted at the idea that the cobind operation for a comonad was a bit like an operation on a SIMD computer that allows every processor to get access to information local to every other processor. The idea now will be to allow those processors to also exploit monads to perform operations like  IO.
I can't yet read most of this paper on [Combining a Monad and a Comonad][1] because I've no experience with 2-categories. Nonetheless, there's something useful I can extract from the bits that do make sense - the idea of combining a monad with a comonad in the way that we already combine monads using monad transformers. In my [previous post][2] I hinted at the idea that the cobind operation for a comonad was a bit like an operation on a SIMD computer that allows every processor to get access to information local to every other processor. The idea now will be to allow those processors to also exploit monads to perform operations like IO.

First let me first reintroduce the code for the array comonads I considered before so that this post again becomes a self-contained literate Haskell post:

\> {-# OPTIONS -fno-monomorphism-restriction #-}  
\> {-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,Arrows #-}

\> import Data.Array  
\> import Data.Foldable as F  
\> import Control.Monad  
\> import Control.Arrow

\> data Pointer i e = P { index::i,  array::Array i e } deriving Show

\> bind f x = x >>= f

\> class Functor w => Comonad w where  
\>    (=>>) :: w a -> (w a -> b) -> w b  
\>    x =>> f = fmap f (cojoin x)  
\>    coreturn :: w a -> a  
\>    cojoin :: w a -> w (w a)  
\>    cojoin x = x =>> id  
\>    cobind :: (w a -> b) -> w a -> w b  
\>    cobind f x = x =>> f

\> instance Ix i => Functor (Pointer i) where  
\>  fmap f (P i a) = P i (fmap f a)

\> instance Ix i => Comonad (Pointer i) where  
\>  coreturn (P i a) = a!i  
\>  P i a =>> f = P i $ listArray bds (fmap (f . flip P a) (range bds))  
\>      where bds = bounds a

Remember how things work with monads. If

m

is a monad, then given a function

a -> m b

we can lift it to a function

m a ->m b

. Similarly, with a comonad

w

we can lift a function

w a -> b

to a function

w a -> w b

. In fact, I wrote about all this a [while back][3].

Just like before, we can write a simple 'blur' function, except this time it also performs some I/O.

\> wrap i = if i<0 then i+5 else if i>4 then i-5 else i

\> blur (P i a) = do  
\>       let k = wrap (i-1)  
\>       let j = wrap (i+1)  
\>       let s = 1.0\*a!k + 2.0\*a!i + 1.0\*a!j  
\>       print $ "sum = " ++ show s  
\>       return s

Notice that blur is of the form

w a -> m b

. So how can we compose these things?

Consider

g :: w a -> m b

and

f :: w b -> m c

. Using

bind

and

cobind

we can 'lift' the head and tail of these functions:

bind g :: w a -> w (m b)

and

cobind f :: m (w b) -> m c

.

We can almost compose these functions. But the catch is that we need a function from

w (m b)

to

m (w b)

. Conveniently, there's a prototypical example of such a thing in the ghc libraries, the function

sequence

which distributes any monad over

\[\]

. As we can easily move back and forth between arrays and lists we can write:

\> class Distributes m w where  
\>    distribute :: w (m a) -> m (w a)

\> instance (Monad m,Ix i) => Distributes m (Pointer i) where  
\>    distribute (P i ma) = do  
\>        let bds = bounds ma  
\>        a <- sequence (elems ma)  
\>        return $ P i (listArray bds a)

And now we can compose these funny dual-sided Kleisli/coKleisli arrows. But before writing a composition function, note what we have. We're looking at functions that can make

a

\-ish things to

b

\-ish things, and we can compose them like functions directly from

a

to

b

. This is precisely the design pattern captured by arrows. So we can write:

\> data A m w a b = A { runA :: w a -> m b }

\> instance (Distributes m w,Monad m,Comonad w) => Arrow (A m w) where  
\>    A g >>> A f = A $ bind f . distribute . cobind g  
\>    first (A f) = A $ \\x -> do  
\>        u <- f (fmap fst x)  
\>        return $ (u,coreturn (fmap snd x))  
\>    pure f = A $ return . f . coreturn

Now check out page 1323 of [Signals and Comonads][4] by Uustalu and Vene. Honestly, I found that paper *after* I wrote the above code. It's almost line for line identical!

Anyway, now we can try an example. First some data to work on:

\> x = P 0 $ listArray (0,4) (map return \[0.0..4.0\])

This is like last post's except that as I've now used a non-normalised blur operation I can show how arrow notation makes it easy to fix that:

\> g = proc a -> do   
\>    b <- A blur -< a  
\>    n <- A blur -< 1  
\>    returnA -< a-b/n

That's a very simple (and inefficient) high pass filter by the way. At the end of the day, however, we still need to be able to lift our arrows to act on

x

:

\> liftCM :: (Distributes m w, Monad m, Comonad w) => A m w a b -> w (m a) -> w (m b)  
\> liftCM (A f) = cobind (\\x -> distribute x >>= f)

\> y = liftCM g x

And convert the result to something we can look at:

\> result = sequence (toList (Main.array y))

It might not be quite the result you expected because we see a lot of I/O. Each computation that is [tainted][5] with I/O will carry that I/O with it. So every bit of I/O that was computed by any value that that went into the final result remains hanging round its neck until the bitter end, with duplications if the value was duplicated.

Of course there are lots of other monad/comonad pairs to try, so maybe there are some other interesting combinations lurking around.

Exercise:

liftCM

lifts an arrow

A m w a b

to a function

w (m a) -> w (m b)

. You can also implement a lift to

m (w a) -> m (w b)

. Implement it and figure out what it does.

[1]: http://citeseer.ist.psu.edu/314131.html
[2]: http://sigfpe.blogspot.com/2008/03/comonadic-arrays.html
[3]: http://sigfpe.blogspot.com/2006/06/monads-kleisli-arrows-comonads-and.html
[4]: http://www.jucs.org/jucs_11_7/signals_and_comonads/jucs_11_7_1311_1327_vene.pdf
[5]: http://sigfpe.blogspot.com/2007/04/trivial-monad.html
