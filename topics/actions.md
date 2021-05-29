# Actions

* Actions are repr by IO type ctor whose type var describes an effect.
* General type is (IO a), so (IO String) is an action that returns a string.
* Actions are inert until executed by the Haskell run-time system (RTS)
* The slurp operator (<-) pulls in values from IO ctors for further manipulation, but an IO value can never rid itself of the IO data ctor.
* Actions are first-class values, can be combined into larger actions
* Actions may return a value
* Although actions don't take parameters, this can be emulated using a function that returns an action (sure, there's a huge difference between an action and an action-returning function)

* The rule for combining actions: the effects of actions happen in succession; the result of the first action is observable by the second. IO actions are modelled sequentially, akin to statements in an imperative language.


The do-notation is similar to let-expressions, and it's used to combine actions. Adjacent actions get combined. Nesting is similar to nesting of let expressions.

The do-blocks allow the plain let-expressions and bindings (but the `in` keyword should be elided.) and if-then-else expressions, as they evaluate to an action; each branch (the "then" or the "else" branch) has to amount to an action, and multiple lines can be combined into one using a (sub) do-block.


## Action! Computation! Blurb!

Putting actions in a tuple doesn't run them, it just creates a pair holding two IO computations. Compare the differences between these:

```hs
act :: (IO (), IO ())
act = (putStr "Action!", putStr "Computation!")

run1 :: IO ()
run1 = do
   let (x, y) = act     -- pattern match in `do` sequences using `let`
   x                    -- run the first action
   y                    -- then run the second

run2 :: IO ()
run2 = do
   let (x, y) = act     -- pattern match
   y                    -- run the second action
   x                    -- then run the first

run3 :: IO ()
run3 = do
   let (x, y) = act     -- pattern match
   x                    -- run the first action
   x                    -- then run it again!
```


## Lift

- lifting in IO
- what does liftM mean in IO
- *liftM* as the shorthand for `do { x <- act1; return (f x) }`
- what does liftM2 mean in IO
- generalizations: `liftM3`, `liftM4`, etc.

```hs
-- liftM is a shorthand for
do { x <- act1; return (f x) }

-- liftM
(...) = do
  x <- act1
  return (f x)

Control.Monad liftM :: Monad m => (a1 -> r) -> m a1 -> m r



-- liftM2 is a shorthand for
do { x <- act1; y <- act2; return (x `op` y) }

(...) = do
  x <- act1
  y <- act2
  return (x `op` y)
```


# Actions

http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html

Actions
- Action functions are functions that return actions
- functions involving IO type
- running actions
- actions are first class values


## Impurity

Doing I/O is by its nature impure. A function cannot execute an action and stay pure. Pure functions cannot exert changes on the outside environment. To get around this problem and stay pure, thereby keeping referentail transparancy, functions do not really execute actions - a function that is to display a text on the stdout, doesn't do it directly, but it returns the action that, when eventually executed, prints the message to the console. That function returns the same action every time - an action that prints some text to the console - and thus stays pure.

The `putStrLn` function accepts a string arg, and *returns an action* to print that string arg to the console. That's an important distinction - `putStrLn` doesn't actually print anything itself, it just plays naive and returns an action (something like executable instructions describing what needs to be peformed), which when executed prints the text. These instructions are executed by the RT system when the program runs.

Actions are those instructions, but usually functions that involve actions are called *actions* themselves.

The Haskell code stays pure that way, pushing all the filth into the runtime system which eventually flushes the toilet/pulls the trigger, unleashing dirt.

```hs
putStrLn :: String -> IO ()
```

**Actions** are first-class values; you can mix them together and combine them into larger actions.

Actions are identified by the presence of the `IO` ctor. They are often seen inside the `do` notation; however, the occurance of the do notation does not imply `IO` context because the do notation may be used by any monad, not just `IO`.

```hs
main :: IO ()
main = do
    putStrLn "Go shopping"
    putStrLn "Take a nap"
    putStrLn "Learn Haskell"
```

This example is indeed in the context of IO monad, but had it been one action instead of 3, the do block would be redundant. The do block is introduced when two or more actions need to be combined. The do blocks may be nested.
