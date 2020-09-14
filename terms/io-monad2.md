# IO Monad

*What is IO monad*, 2018
- https://www.youtube.com/watch?v=fCoQb-zqYDI
- https://github.com/rexim/io

## How does a pure FPL model side-effects and remains pure?

A function that takes some state of the world, along with some arguments, and after performing some I/O computations returns the results along with the next state of the world, is pure.

It is like extending the domain and codomain of a function to encompass more then the function originally bargained for. It's similar to a function that must log its execution but remain pure; which can be achieved by hauling around an additional type, i.e. the type of the log (e.g. String).

To model pure functions that deal with I/O, the type that needs to be hauled around could be named `World`, and it would represents the entire state of the world at that moment.

Functions dealing with I/O would take the `World` along with other arguments, and they would perform the computations that might include executing some side-effects in the provided `World`; they would return the results along with the new state of the `World`.

That way, each function would remain pure, since given the same state of the World, they would always return the same succeeding state of the World.

For example, a action to print some text to console would receive the string to print and the current state of the world (which includes the state of the screen), and it would return the result (the result would be uninteresting so it'd be a unit, `()`) along with the new state of the world - a world in which the provided string is printed in the console!


```hs
data World = World deriving Show

f :: World -> World

printStr :: String -> World -> World

readStr :: World -> (String, World)

instance Semigroup World where
  w1 <> w2 = w1

instance Monoid World where
  mempty = World
```

Thus, it becomes pure because given      
the same String and the same World state,     
it always has hte same outcome, result-wise and World-wise:     
    `printStr :: String -> World -> World`




The fn `whatIsYourPureName` is our impl of a pure function that reads the user- provided input string from the stdin. In fact, it takes the current world (where it didn't ask for input) and produces a pair consisting of a String (a user's input) and the new World (i.e. the world where it does ask a user for his name, then prints it).

We're going to modify the world several times, so we need to keep track of it.
- Initially, we pass the string "What is your name?", along with a world `w1` to the `printStr` function.
- `printStr` prints the string to the console and returns a new state of the world, which we assing to `w2` (which is a world where the console contains the printed string).
- then we pass `w2` to `readStr :: World -> (String, World)`, and it prompts the user for his name. When the user enters his name, `readStr` returns a pair: the string it read and the new state of the world. We use destructuring assignment to bind both, read string to `name` and the world to `w3`.
- we call `printStr` again, passing it the string `"Hello " ++ name` and the previous world state `w3`. The `printStr` puts that text on the screen and returns the final world, which we bind to `w4`


```hs
data World = World deriving Show

printStr :: String -> World -> World
printStr s !w = unsafePerformIO (putStrLn s >> return w)

readStr :: World -> (String, World)
readStr !w = unsafePerformIO (getLine >>= \s -> return (s, w))

whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4 where
    w2         = printStr "What is your name?"  w1
    (name, w3) = readStr                        w2
    w4         = printStr ("Hello " ++ name)    w3
```

However, now that the `World` is a value that can be passed around we have a problem: everyone can write a fn that branches the world into two. It takes a single world but return a pair consisting of that world forked in two, causing a terrible incoherence.

```hs
branch :: World -> (World, World)
branch w = (printStr "True world" w, printStr "False world" w)
```

The world must not fork - we mustn't be able to split the World and have Worlds with different states. Some FPL, like *Clean*, solve this with *uniqueness types* where a value can only be used once.

Haskell doesn't have support for unique type so it goes with another solution - it completely hides the World type from the users. Users don't have to know about the World types anyway, but they need a way to compose the types that hide the World.

## World Transformers

Haskell makes the World type inaccessible by introducing World Transformer.

> **World Transformer** is a function that transforms the `World` type and another type, `a`, by hiding the `World`.

This can be done easily with a type alias:      
`type WorldT a = World -> (a, World)`

We have functions `readStr` and `printStr` that work with the `World` type directly, but now what we need to make type wrappers, so they work with a World Transformer type, `WorldT`, instead.


The `readStr` already has the matching signature:     
`readStr:: World -> (String, World)` 

because the signature of `WorldT a` type alias is     
`type WorldT a = World -> (a, World)`

so when the `a` is specialized to a String it becomes:     
`type WorldT a = World -> (String, World)`
which is the exact signature of `readStr`.

So `readStrT` is already a WT that returns a String:      
`readStrT :: WorldT String`.



The `printStr` has the signature     
`printStr :: String -> World -> World`

And, again, the type alias `WorldT` is:     
`type WorldT a = World -> (a, World)`

when `a` is specialized to the unit type it becomes:     
`printStr :: String -> World -> ((), World)`

now we can express `printStrT` also as a WT:     
`printStrT :: String -> WorldT ()`

It takes a String and returns a `WorldT ()`,      
and `WorldT ()` is `World -> ((), World)`     
so it also takes a World (although hidden)     
and returns a pair of unit and new World state.

```hs
--          World -> (String, World)
readStrT :: WorldT String
readStrT = readStr

--           String -> World -> ((), World)
printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)
```

We can now use `readStrT` but we still need to supplying it with a `World`:    
`readStrT World`

it executes the effect (action) of reading a string from stdin, then returns a pair made out of the read string and a new world state, `("344",World)`.

We can also use the `printStrT` by supplying it with a string to print and a world state: `printStrT "Hi" World`.

It will execute the action of printing the string, then returns a pair of unit (which we ignore) and a new world state:

```
((),Hi  -- "Hi" is the printed string that got mixed
World)  -- with the return value of `((), World)`
```

## Composing World Transformers

What we need now is a way to compose the World Transformers together.
















---

- https://bit.ly/haskellrank
- https://stackoverflow.com/questions/3850368/how-do-functional-languages-model-side-effects
- https://clean.cs.ru.nl/Clean
- http://www.mbsd.cs.ru.nl/publications/papers/cleanbook/CleanBookI.pdf
- https://en.wikipedia.org/wiki/Uniqueness_type
