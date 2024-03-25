# MTs from the first principles

https://williamyaoh.com/posts/2023-06-10-monad-transformers-101.html

## Before monad transformers

Consider, for example, working with both the `IO` and `Maybe` monad, and having to deal with the types like `IO a`, `Maybe a`, `IO (Maybe a)`.

Interleaving these two monads manually will not yield a very pleasent code:

```hs
validateForm :: Int -> String -> IO (Maybe (Int, String))
validateForm n s = do
  -- `validate n` returns IO (Maybe Int)
  -- so we first extract the Maybe Int
  d0 <- validate n
  -- then we scrutinize it
  case d0 of
    Nothing -> return Nothing -- early exit
    Just d1 -> do             -- otherwise
      -- `check s` returns IO (Maybe String)
      -- so again, we first extract it
      d2 <- check s
      -- then we scrutinize it
      case d2 of
        Nothing -> return Nothing
        Just d3 -> return $ Just (d1, d3)


validate :: Int -> IO (Maybe Int)
validate x = return (Just x)

check :: String -> IO (Maybe String)
check x = return (Just x)
```

This function only has two parameters but already it is getting unwieldy. What we want instead is a way where each monadic binding both deals with the IO portion and handles the pattern matching or short-circuiting of the inner monad.

This suggests creating a new monad - that combines the capabilities (effects) of the IO and Maybe monads - that will have a different bind definition.

```hs
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }
```

If we had the FAM instances for this type, we could write much less noisy code

```hs
validateForm :: Int -> String -> MaybeIO (Int, String)
validateForm x s = do
  mx <- validate x
  ms <- check s
  pure (mx, ms)

-- Couldn't match expected type:
--   Maybe (Int, String)
-- with actual type:
--   (Maybe Int, Maybe String)
```

...which fails terribly (aside from 'MaybeIO' re/wrapping): 
could not match expected type: `Maybe (Int, String)` 
with actual type: `(Maybe Int, Maybe String)`.

But this works:

```hs
validateForm :: Int -> String -> MaybeIO (Int, String)
validateForm x s = MaybeIO $
  do
    mx <- validate x
    ms <- check s
    case mx of
      Nothing -> return Nothing
      Just a  -> case ms of
        Nothing -> return Nothing
        Just b  -> return (Just (a, b))
```


We've run into a different problem:
>What if we want to add another monad into this stack?

Say we want to also have a Reader to hold some configuration, or some State to hold a cache. Are we going to create new wrapping types for every single possible combination of monads? Are we going to write monad instances and helper functions for each of those possible combinations?

It seems like what we want is a way to "isolate" the functionality of each individual monad we want to use, while then having the ability to cobble them back together into a working whole. That is, we'd have a "Maybe" component that gives you the ability to short-circuit, we'd have a "Reader" component that lets you store some read-only data, and some way to plug those together.

We need to parameterize the `Maybe` type by another monad, `m`:

```hs
newtype MaybeT m a = MaybeT (m (Maybe a))
```

If we now substitute `IO` for `m`, we get the `MaybeIO` data type from above:

```hs
MaybeT IO a = IO (Maybe a) = MaybeIO a
```

>We took a plain `Maybe` monad and parameterized it so as to have a "slot" we can plug another monad into.

If we write a Monad instance for the `MaybeT` type, we could then drop it on top of any existing monad and keep the underlying functionality while augmenting it with the capability to handle failing computations - the curtosy of the `Maybe` moand.

```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT mf <*> MaybeT ma = MaybeT $ liftA2 (<*>) mf ma

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mxx >>= f = MaybeT $ do
    mx <- mxx
    case mx of
      Nothing -> pure Nothing
      Just x  -> runMaybeT (f x)
```

Notice `fmap . fmap` (or `pure . pure`, or `liftA2`) - we need two `fmap`s because we are lifting into two (monadic) layers (once for `Maybe` and once for the other monad `m`).


We could write similar instances for a hypothetical `StateT`, `ReaderT`, and so on, and then we'd have the ability to mix and match monadic capabilities, while still retaining the syntactical convenience of the initial `MaybeIO` monad.

>The key thing is that monad transformers can take in any other monad as their inner monad.

And since monad transformers are themselves monads, we can stack 'em up indefinitely.

```hs
-- notice how each "layer" takes in exactly one (other) monad
type MonadStack a = StateT Int (ReaderT String (MaybeT IO)) a
```

A bunch of transformers chained together like this, where each transformer is the inner monad of another transformer, is referred to as a *monad stack*. The innermost monad in the stack, like IO above, is usually referred to as the *base or bottom monad*.

>The base monad is also where we eventually run our code in; typical choices for the base are `IO` (for side effects) or `Identity` (for pure code).

We write the bulk of our code wrapped inside a monad stack to get a convenient monad instance, then at the toplevel of our program we use functions like `runMaybeT`, `runReaderT`, `runStateT`, etc. to unwrap all those transformer types and get a value like `IO (Maybe a)` or `state -> cfg -> IO (a, state)` that we can actually run.

There is still one problem we need to handle re `MaybeT` and that is how do we access the inner Maybe value. When we want to signal that the current function should short-circuit, how do we do it? Returning `Nothing` won't work, since Maybe and MaybeT are distinct types. We need a helper function that specifically returns a `MaybeT`.

```hs
nothing :: Applicative m => MaybeT m a
nothing = MaybeT (pure Nothing)
```

For any other monad transformers we create, we'd need to do the same thing and write helper functions to enable monad-specific functionality. But compared to the amount of boilerplate that we'd need to implement the "every possible combination of monads" choice from before, this level of repetition is something we'd take anyday.

```hs
-- for StateT
get :: Applicative m => StateT s m s
put :: Applicative m => s -> StateT s m ()

-- for ReaderT
ask :: Applicative m => ReaderT cfg m cfg
local :: (cfg -> cfg') -> ReaderT cfg' m a -> ReaderT cfg m a
```

You can think of monad transformers as functions that take in some other monadic type and "transform" it by adding in extra capabilities (hence the name). And it really can be any monad; the definitions above are completely agnostic of what the inner monad is, only that it implements certain typeclasses.

Unfortunately, there are exceptions. Some transformers don't form law-abiding monads, or have to be used in very specific ways. The most notorious of these is `ListT`.

```hs
>>> concatMap (take 3) [[1..], [10..], [100..], [1000..]]
-- [1,2,3,10,11,12,100,101,102,1000,1001,1002]

>>> concatMap (take 3) (Just [1..]) -- [1,2,3]

concatMap :: (a -> [b]) -> [a] -> [b]
```

## mtl style and transformers style

There are two common ways to use type classes to make working with MTs more convenient, referred to as the transformers style and the mtl style. Both are about adding convenience to make the code more digestable.

Consider again the `MaybeT` example. It works well when every function returns `MaybeT IO a`. Calling `IO a` actions directly inside a `MaybeT` function does not work - the types are mismatched, IO /= MaybeT IO.

```hs
validateInput :: MaybeT IO String
validateInput = do
  line <- getLine -- does not compile since IO /= MaybeT IO
  -- ...
```

Instead, we have to lift (fmap) the `Just` ctor over `getLine`, and then wrap that in `MaybeT` before we can extract the string entered by user:

```hs
validateInput :: MaybeT IO String
validateInput = do
  line <- MaybeT $ fmap Just getLine
  -- ...
```

We'd do something similar for each IO action we want to run inside `MaybeT IO`.

>One could stop here, and apart from writing some extra convertor functions, everything would work fine.

To cut down this boilerplate, we could write a function `IO a -> MaybeT IO a`, but it would only work in this particular monad stack. If we use a different inner monad or the stack is multiple layers deep, we'd have to write a similar function to manually handle the wrapping for every layer above the one we are trying to use.

The `transformers` was the original library to implement MTs and it introduced the style now called transformers style. The `mtl` library, based on it, later introduced a different solution to tackle this problem.

## transformers style

>The problem is we need a way to take the inner (wrapped) monad and convert it into the wrapping MT.

The most direct way to solve this is an actual function from one to the other, like `IO a -> MaybeT IO a`, but as we concluded, we'd need similar functions for variations of the inner and outer monads. Sounds like polymorphism and a call to make a class to group different MTs.

```hs
class MonadTrans mt where
  lift :: Monad m => m a -> mt m a
```

The `MonadTrans` class targets the MT type ctors, `mt`, of kind `(* -> *) -> * -> *`. However, `mt` is a specific monad transformer, like MaybeT (i.e. there is no generic solution where we combine any two monads to make a MT out of them; it is always a specific MT with another monad plugged in).

```hs
class MonadTrans mt

>>> :kind a  ::             *
>>> :kind m  ::  * -> *
>>> :kind mt :: (* -> *) -> * -> *
>>> :kind mt m :: * -> *
>>> :kind mt m a :: *

>>> :kind MonadTrans :: ((* -> *) -> * -> *) -> Constraint
>>> :kind MonadTrans MaybeT :: Constraint

type MaybeIO :: * -> *
type MaybeT  :: (* -> *) -> * -> *
```

If we substitute `mt = MaybeT` and `m = IO`, then `lift` will have exactly the type `IO a -> MaybeT IO a`. But since `lift` is polymorphic, we can now wrap any inner monad we want with just one function.

This also gives us a concise definition of a monad transformer: it is any type where an inner monad can be converted to the type itself. The operation is called "lift" because we are moving an inner monad "upwards" through the stack, towards the topmost transformer.

```hs
instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (StateT s) where
  lift m = StateT (\s -> fmap ((,) s) m)

validateInput :: MaybeT IO String
validateInput = do
  line <- lift getLine  -- much shorter now!
  -- ...
```


https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Class.html

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.Class.html

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Select.html#t:SelectT

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-RWS-CPS.html#t:RWST

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Writer-CPS.html#t:WriterT


Though this is a major improvement, there are still cases when the monad stack can get tall due to all the `lift`ing.

```hs
foo :: StateT Int (ReaderT String (MaybeT IO)) ()
foo = do
  -- IO requires lots of lifting here
  input <- lift $ lift $ lift getLine
  -- ...
```

The problem of requiring the end developer to implement or remember lots of different lifting functions for each monad transformer is gone.

However, we can only lift one layer at a time. It would be *noice* if, for every component in the monad stack, we could use the functions from that component in any stack containing that component, with no wrapping needed.

So, just to avoid writing a few `lift`s in a row, we are going polymorphic all the way, despite the expected inefficiency of such code (eat a bag of nested dicts).

## mtl style

To cut down the boilerplate even more, all the functions involved need to be polymorphic. For example, for `StateT` we have functions whose return type is too concrete:

```hs
get :: Applicative m => StateT s m s
put :: Applicative m => s -> StateT s m ()
```

Instead, we need these functions to have a signature like so:

```hs
get :: MonadState mt s => mt s
put :: MonadState mt s => s -> mt ()
```

Rather then tying ourselves to a concrete state type, we use a type class that represents "statefulness". The idea is that as long as the type of the monad stack implements this class, we can call `get` and `put` without lifting, no matter how deep the `StateT` goes and regardless of what order the transformers have been stacked in.

```hs
class Monad m => MonadState m s where
  get :: m s
  put :: s -> m ()

-- StateT can implement MonadState directly...
instance Monad m => MonadState (StateT s m) s where
  get :: StateT s m s
  get = StateT \s -> return (s, s)

  put :: s -> StateT s m ()
  put s = StateT \_ -> return ((), s)

-- ...and if the inner monad supports statefulness, other monad
-- transformers can simply delegate the state operations downward
instance (MonadState m s) => MonadState (MaybeT m) s where
  get :: MaybeT m s
  get = lift get

  put :: s -> MaybeT m ()
  put s = lift (put s)
```

and similar instances of `MonadState` for `ReaderT`, `WriterT`, etc.

Another way to look at this, for any MT that is not `StateT` itself, the `MonadState` instance handles calling the appropriate number of `lift`s.

And with that, we now have a way to use `get` and `put` in whatever monad stack as long as we specify that said monad stack has something to handle the statefulness (somewhere in the stack).

```hs
foo = do
  s :: Int <- get
  put (s + 1)

-- `foo` above will now work in all these stacks:
foo ::         StateT Int IO    ()
foo :: MaybeT (StateT Int IO)   ()
foo :: (MonadState m  Int) => m () -- fully polymorphic
```

This solution is what's known as **mtl style**. More generally, this approach of using class constraints/instances and making the type of a value completely polymorphic is known as **tagless final**. Tagless final can be seen as an *inversion of control*, where the behavior of the code is determined by use sites, rather then by the code definition itself. It is a more general technique then what we saw here; it just happens to be useful for working with monad transformers as well.

To fully make use of this style, we'd have to write similar classes for all MTs as well, which can be a lot of work. But once we do, the syntactic noise of calling `lift` disappears; we can directly use any function from any MT in the stack. Whether doing all this justifies saving a few calls to `lift` is another question.


Beyond the syntactic convenience of not having to call `lift` anymore, there are some other benefits to using mtl style vs transformers style.

With transformers style, if we have two functions, one that returns a `MaybeT IO a` and one `MaybeT (StateT Int IO) b`: intuitively, it seems like we should be able to use these two together, since the former is a subtype of the latter. But if we try, we see that some painful finicking is required in order to convert from one type to the other. Lift won't help us here since the offending `StateT` is in the middle of the stack rather than on top.

This problem disappears in *mtl style*, since we never actually specify concrete types for our monad stack anywhere - just classes describing the required functionality. So if we have a function that uses Reader and another that uses both State and Reader, there is nothing stopping us from combining them.

```hs
justReader :: MonadReader m String => m ()
justReader = -- ...

readerAndState :: (MonadReader m String, MonadState m Int) => m ()
readerAndState = -- ...

-- we can use both in the same function!
combined :: (MonadReader m String, MonadState m Int) => m ()
combined = do
  justReader
  readerAndState
```

In fact, this elegantly deals with a number of other potential problems with using *transformers style*, like how `ReaderT cfg (StateT s IO) a` and `StateT s (ReaderT cfg IO) a` are not the same type and cannot be used together, despite being equivalent by inspection.

In the *mtl style*, the order in which transformers are specified does not matter during the design phase, only during the execution.

## Differences

Between mtl and transformers, which one is better? While we can see that mtl is definitely more syntactically convenient, it's actually a rather subtle question. Both libraries can be better in different situations. mtl allows you to avoid having to specify an order when writing code, and only choose it once you go to run it, which can matter when working with monads that allow for early exits. It's also just nicer to use. On the other hand, transformers can allow you to have more than one of the same transformer in the same stack; with mtl, if you want two different StateT's, both with an Int state parameter, you can't; there's no way to differentiate the constraints needed. This ambiguity doesn't exist in transformers style. Implementing mtl style (say, if you create your own monad transformers) is significantly more work as well, since every new transformer requires a typeclass, plus instances for every existing transformer, something known as the *n² instances problem*.

One important difference is that mtl is often slower than transformers, despite providing the exact same functionality and using the exact same underlying transformer types. The reasons why are outside the scope of this post, but it's something to keep in mind if you're using monad transformers for performance-critical code.

---

In the next post, we'll go deeper into the why of monad transformers. We'll look at practical examples of combining transformers to solve real problems. We'll see how the resulting code is more than the sum of its parts, while still retaining the modularity typical of Haskell abstractions.

https://williamyaoh.com/posts/2023-07-01-why-monad-transformers-matter.html
