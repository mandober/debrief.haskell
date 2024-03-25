# The continuation monad

from: The continuation monad - Haskell for all, 2012
https://www.haskellforall.com/2012/12/the-continuation-monad.html

A Haskell continuation has the following type:

```hs
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
```

A continuation takes a function of type `(a -> r)` and generates an `r`, where `r` can sometimes be a fixed value like `Int` or `IO ()`.

For example, a long-running process can spawn an action every time the user enters a line of input (e.g. when writing a REPL)

```hs
        -- i.e. Cont (IO ()) String
onInput :: (String -> IO ()) -> IO ()
onInput k = forever $ do
  line <- getLine
  k line
```

You will recognize this idiom if you've used frameworks with callbacks. We supply the framework with a function (i.e. a continuation) and the framework uses that function to call us back when it finishes the task.

Cont are used for many advanced things, but also for some more to the ground ones For example coding everything around an action (behavior) that will be specified later. In such situations we can just parameterize that action, so it's filled in later, as a continuation.

```hs
-- skeleton code around an, as of yet unknown, action (???)
unitAttack :: Target -> IO ()
unitAttack target = do
  swingAxeBack 60
  valid <- isTargetValid target
  if valid
  then ??? target
  else sayUhOh

-- parameteriziong the action
unitAttack :: Target -> (Target -> IO ()) -> IO ()
unitAttack target todo = do
  swingAxeBack 60
  valid <- isTargetValid target
  if valid
  then todo target
  else sayUhOh

-- See how type signature has the shape of the Cont datatype.
-- We can also wrap it in the Cont data type:
unitAttack :: Target -> Cont (IO ()) Target
unitAttack target = Cont $ \todo -> do
    swingAxeBack 60
    valid <- isTargetValid target
    if valid
    then todo target
    else sayUhOh

-- Or, we can use ContT data type instead.
-- The benefit of ContT is that it is also a monad transformer.
-- ContT has the same Monad instance as Cont, so they are interchangeable.
unitAttack :: Target -> ContT (IO ()) Target
unitAttack target = Cont $ \todo -> do
    swingAxeBack 60
    valid <- isTargetValid target
    if valid
    then todo target
    else sayUhOh
```

In the data type `Cont (IO ()) Target`, the first type, `Target`, is the *nominal return type*, and the second one is *the answer type*, `IO ()`. A continuation takes a nominal result and returns the overall return type, called the answer type.
