# Continuation Monad

http://web.archive.org/web/20030620133510/http://www.haskell.org/hawiki/MonadCont

module: `Control.Monad.Cont`

The `Cont` Monad, short for Continuation Monad, adds continuation passing features. The only method of `MonadCont` is `callCC` i.e. the Scheme-inspired operator `call/cc` (call-with-current-continuation). There is also a transformer version, `ContT`.

The type of continuations `callCC` provides are called *escape (or abortive) continuations* because when called they immediately abort the rest of the computation and return whatever is passed to the continuation. Escape continuations also provide a simple form of exception handling.

## Details

```hs
runCont :: Cont r a -> (a -> r) -> r

callCC :: ((a -> m b) -> m a) -> m a
```

`runCont` runs a CPS computation. It takes a final continuation (e.g. `id`) that transforms the result of the monadic computation into the final result.

`callCC` is a method of `MonadCont` that takes a function that takes the current continuation, represented as a function that takes the output (would be returned) value.


## Possible Implementation

```hs
-- r is the final result type of the whole computation
newtype Cont  r   a = Cont  { runCont  :: ((a ->   r) ->   r) }
newtype ContT r m a = ContT { runContT :: ((a -> m r) -> m r) }

class (Monad m) => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance Monad (Cont r) where
    return a = Cont $ \k -> k a

    --    c  >>= f =        \k -> c (\a ->          f a  k)
    (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

instance MonadCont (Cont r) where
    callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
```


## shift/reset

Most of the more interesting things to do with call/cc in Scheme require some mutable state and aren't immediately applicable with this definition of `callCC` (perhaps `ContT` over `State` or vice versa?); further, it's not immediately obvious how to make some control structures (e.g. jump/goto) with just `callCC`.

Another possibility is to use partial (functional, composable) continuations, the most common primitives being `shift` and `reset`. Their `ContT` versions could be defined as:

```hs
shift e = ContT $ \k -> runContT (e $ \v -> ContT $ \c -> k v >>= c) return
reset e = ContT $ \k -> runContT e return >>= k
```

These control structures are more powerful than `callCC` (see `MonadIdentity` for how to use these in non-monadic code, or inline the definitions from the `Identity` monad to get a `Cont` version).
