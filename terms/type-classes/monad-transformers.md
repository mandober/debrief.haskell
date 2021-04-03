# Monad transformers

https://en.wikibooks.org/wiki/Haskell/Monad_transformers

Particular monads help in handling IO, Maybe, lists and state by providing a common way to use these useful general-purpose tools, but it would be useful if we could use the capabilities of different monads at the same time. For instance, a function might want to use both IO and Maybe exception handling. While a type like `IO (Maybe a)` would work ok, it would force us to do pattern matching within IO do-blocks to extract values, something that Maybe monad was meant to spare us from.

**Monad transformers** are special types that allow us to roll two monads into one that has the behavior of both.

The `State` type and monad transformer `StateT`:

type State  s   a = State  { runState  :: a ->   (a, s) }
type StateT s m a = StateT { runStateT :: a -> m (a, s) }

this shows that the other monad, `m`, will plug itself in the most awkward of places.


## Case study: password validation

Imposing requirements on the strenght of user-created passwords, e.g. at least one of: capital letter, number, non-alphanumeric character, etc.

```hs
-- acquiring a passphrase from a user
getPassphrase :: IO (Maybe String)
getPassphrase = do
    s <- getLine
    if isValid s then return $ Just s else return Nothing

-- validation test could be anything
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

any :: Foldable t => (a -> Bool) -> t a -> Bool
```

The do block is in the IO monad, and we just happen to return a Maybe value inside it.

Monad transformers not only make it easier to write getPassphrase but also simplify all the code instances.

Our passphrase acquisition program could continue like this:

```hs
askPassphrase :: IO ()
askPassphrase = do
    putStrLn "Insert your new passphrase:"
    mx <- getPassphrase
    case mx of
        Just v  -> do putStrLn $ "Storing in database: " ++ v
        Nothing -> putStrLn "Passphrase invalid."
```

With monad transformers, we will be able to extract the passphrase in one go, without any pattern matching or equivalent bureaucracy like `isJust`.


## MaybeT

To simplify getPassphrase and the code that uses it, we will define a *monad transformer MaybeT that gives the IO monad some characteristics of the Maybe monad*.

MaybeT is a simple monad transformer. We call it *MaybeT* following the naming convention for monad transformers where a *T* is appended to the name of the monad whose characteristics they provide.

MaybeT is a wrapper type around `m (Maybe a)`, where `m` can be any monad (IO here).

Monad transformers transform monads into monads, so we make `(MaybeT m)` an instance of the Monad class:

```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return = MaybeT . return . Just
 -- return = MaybeT . return . return

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f = MaybeT $ do
        mx <- runMaybeT x
        case mx of
            Nothing -> return Nothing
            Just v  -> runMaybeT $ f v
```

`x >>= f = ...`    
We'll define bind as infix operator with:
x     :: MaybeT m a
f     :: (a -> MaybeT m b)
(ret) :: MaybeT m b

x :: MaybeT m a

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
type    MaybeT m a = MaybeT (              m (Maybe a) )
type MaybeT m a = MaybeT (m (Maybe a))

x :: MaybeT m a
x :: MaybeT (m (Maybe a))


MaybeT { runMaybeT :: m (Maybe a) }



`x  >>= f = MaybeT $ do ...`    
Firstly, the entire `do block ` is wrapped with the `MaybeT` constructor.

The do block has the type `do-block :: m (Maybe b)`.

We use the `MaybeT` constructor before the do block, even though we have the accessor `runMaybeT` within do becasue, we want the do block to be in the `m` monad, not in the `MaybeT m` (which lacks a defined `bind`  at this point).


`mx <- runMaybeT x`    
`runMaybeT` accessor unwraps `x` into an `m (Maybe a)` computation. That shows us that the entire *do block is in `m`*. The slurp extracts a `Maybe a` value from the unwrapped computation into `mx`.

 `case mx of ...`    
The case statement tests a maybe value `mx`
- if Nothing, we return Nothing into `m`, using `return` to lift the Nothing
  into monadic context.
- if Just, we apply `f` to the inner value, as `f v`.   
  Since `f :: a -> MaybeT m b` and `v :: a` then applyling `f` to `a` we get a type `MaybeT m b` which is the type of the bind returns.

Since `f`'s type is `MaybeT m b`, we need an extra `runMaybeT` wrapper around the results of `f v` before we put it back into the `m` monad. (??)



The definition may seem complicated, but aside from the copious un/wrapping, the impl of MaybeT's `bind` works the same way as the `bind` for Maybe.

```hs
mx >>= f = mcase mx of
    Nothing -> Nothing
    Just v  -> f v
```
