# Free monad

http://blog.omega-prime.co.uk/2008/05/08/free-monads-in-haskell/

Given any endofunctor, it is possible to automatically generate a monad, called *the free monad of the functor*.

Everything flows from this data type:

```hs
-- author's type
data FreeM f a = Return a | Bind (f (FreeM f a))

-- Issue #1
-- Should we use GADT to define this type since then we can constrain the `f`
-- type param to be a Functor? 
data FreeM f a where
  Return :: Functor f => a -> FreeM f a
  Bind   :: Functor f => f (FreeM f a) -> FreeM f a
```

The `f` type parameter is the functor we're building the free monad for, and `a` is the type of contents of a `FreeM` value. The type declaration states that the free monad can either contain such a value directly (`Return` ctor), or it can contain a value of it's own type, but wrapped in the functor `f`. 

Intuitively, the declaration states that a value can be an actual `a` wrapped within the `Return` ctor, or a value can be nested within a finite number of functor applications.

We can make `FreeM` an instance of `Functor`:

```hs
instance (Functor f) => Functor (FreeM f) where
  fmap f (Return a) = Return (f a)
  fmap f (Bind fm)  = Bind (fmap (fmap f) fm)
```

On the RHS of a `f (Bind fm)` pattern (ignoring the `Bind` tag), we have `fmap (fmap f) fm`; the first `fmap` call is the one from the functor `f` that we're making a free monad over; the next `fmap` is actually a recursion back into the fmap we are currently trying to compute, but one functor "lower".

Essentially, we're fmap-ing our tower of functors on our way down, until we finally match a `Return a` that is easily handled (acts as the base case).

Instead of defining a monad using the usual Haskell (»=) operator, we'll use the more categorical `join :: Monad m => m (m a) -> m a` construction:

```hs
joinFreeM :: Functor f => FreeM f (FreeM f a) -> FreeM f a
joinFreeM (Return a) = a
joinFreeM (Bind fm) = Bind (fmap joinFreeM fm)
```

If you think about the type of this operation, what we want to do is stick the pile of functors associated with the outermost `FreeM` onto the pile of functors associated with the innermost one, to produce one cohesive functor pile. How we go about this is fairly straightforward: in the `Return` case there are no functor applications on the outermost `FreeM` so we just give back the inner one. The `Bind` case recurses its way down the whole functor pile, mushing them together in some sense.

The code to actually declare a `Monad` instance is entirely boilerplate given that we have a join operation:

```hs
instance (Functor f) => Monad (FreeM f) where
    return = Return
    m >>= f = joinFreeM ((fmap f) m)
```

## But, but what is it good for?

Wouter Swierstra, in his paper "Data types à la carte", had already demonstrated this technic (in passing) with his `Term` data structure with some nice examples. For instance, consider this functor:

```hs
data Zero a = Zero deriving (Show)

instance Functor Zero where
  fmap _ Zero = Zero

zeroTest :: FreeM Zero Int -> FreeM Zero Int
zeroTest my = do
  x <- return 5
  y <- my
  return $ x + y

-- > zeroTest (return 1)  -- 6
-- > zeroTest (Bind Zero) -- Zero
```

It's...it's...it's the *Maybe monad*!



> What about this?

```hs
data One a = One a
  deriving (Show)

instance Functor One where
    fmap f (One a) = One (f a)

oneTest :: FreeM One Int -> FreeM One Int
oneTest my = do
    x <- return 5
    y <- my
    return $ x + y

-- > oneTest (Return 2)
-- 7
-- > oneTest (Bind $ One $ Return 2)
-- One 7
-- > oneTest (Bind $ One $ Bind $ One $ Return 2)
-- One One 7
```

...it's something like an *identity monad*! What if we try something a bit more exotic (mentioned in Wouter's paper) like *list functor*?

```hs
listTest :: FreeM [] Int
listTest = do
    x <- Bind [Return 1, Return 2]
    y <- Bind [Return 3, Return 4]
    return $ x + y

-- > listTest
-- [[4,5],[5,6]]
```

But for want to a call to concat, this looks strikingly similar to the list monad - and all without ever having to define it.

I've found that the fun of free monads is that you can play around with any functor you like and get something interesting generated from it. Since they're free, this happens without you typing a single line of non-trivial code.
