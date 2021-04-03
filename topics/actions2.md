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
