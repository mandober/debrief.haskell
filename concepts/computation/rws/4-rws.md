# RWS

The `RWS` type is an amalgam of the `Reader`, `Writer` and `State` types.

```hs
newtype RWS r w s a = RWS { unRWS :: r -> s -> w -> (a, s, w) }
```

## RWS type

- The `Reader r a` type is actually a function `r -> a`. 
- The `Writer w a` type is actually a pair `(w, a)`. 
- The `State` type is function that returns a pair, `s -> (a, s)`, so it is already like a combination of the `Reader` and `Writer`.
- The `RWS` is the combination of these 3 types, `Reader`, `Writer` and `State`.

```hs
newtype Reader  r a = Reader { runReader :: r ->     a  } -- function
newtype Writer  w a = Writer { runWriter ::      (w, a) } -- pair
newtype State   s a = State  { runState  :: s -> (s, a) } -- fn ret pair combo
newtype RWS r w s a = RWS  { runRWS :: r -> s -> (a, s, w) }
```

The `RWS` is a binary function - it takes an environment `r`, like the `Reader`, and a state `s`, like the `State` - and returns a triple consisting of the "function's original return value", `a`, a new state `s`, and the log `w` that the `Writer` accumulates.

Since the `State` offers the global access to a stateful data, it is in a way a supertype of the `Reader`, which only offers a read-only access to the global stateful data - thus it is unclear why the `RWS` "implements" both (i.e. the `Reader` and the `State`, when State alone should be sufficient).





## RWST

## Intro

Pure functions cannot change extrnal resources. Actually, we can reference module-level variables from within the functions in the same module, but we cannot modify them. Manipulation of external variables is disallowed.

For example, an application's configuration is often modeled through a global `config` variable, except it is usually read-only, and because of that called an *environment* - in fact, an environment is just a read-only state.

Another situtation arises when we need to *trace* the execution of functions by having each function *log* something. This may also be solved with an extra parameter that represents the log, and then each function can append its own output to the log.

Modeling a global state, that is not just read-only but needs also to be modified, using nothing but pure functions, can be accomplished by adding an extra, state, parameter to all functions that need to manipulate that state.


The `State` monad deals with scenarios that involve, for example, maintaining a counter that all levels of code can access and mutate.

The `Reader` monad is similar, but the global state is assumed to be immutable and thus called an *environment*. The reader monad is therefore also known as the environment monad. 

read-only state like a configuration. Such global state is also called an *environment* and so 

The `Writer` monad deals with tracing, i.e. threading an auxillary value from a function (containing e.g. degugging or logging info) along with the function's normal output, into another function. This is solved by having functions return a pair of values instead of just their usual single output value. The next function in the sequence needs to split the recived input (a pair) into its components (the log part and the payload part), using the payload in some way, but adding its own logging info to the log value; then reassmbing a new pair and passing it on down the pipeline.
