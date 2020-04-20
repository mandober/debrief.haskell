# IO Monad

*What is IO monad*, 2018
- https://www.youtube.com/watch?v=fCoQb-zqYDI
- https://github.com/rexim/io

## How does a pure FPL model side-effects and remains pure?

**Side-effect** is an something (action, effect) that modifies the outside environment, especially the environment outside a pure function.

However, side-effect are relative depending on how big is the system. For one system (program, language) something may be a side-effect, while for a larger system the same thing is not a side-effect because the bigger system includes the environment that underwent the change.

Theoretically, the lager system might have information about everything, knowing the state of the entire world down to the configuration of every single particle. Such computer would certainly be called *Laplace's demon*. Fortunatelly, such colossus is not necessary to model the side-effects, but it gives us a clue how to do it.

We create a function that takes a state of the world, along with other arguments, and after performing the IO computation it returns the next state of the world, possibly together with some "normal" type of output.

It is like extending the domain and codomain of the function to encompass more then the function originally has bargained for. It's similar to functions that must log their execution (but remain pure), which they achieve by hauling around an additional type, usually a string, that represents the log.

To model pure functions that deal with IO, the type that such functions need to haul around could be named `World` and it would represents the entire state of the world at that moment. The functions dealing with IO would then take the World (possibly with more input types) and they would execute the side-effect in the supplied World, returning the new state of the World (possibly along with another output type). In that way they would remain pure, since given the same state of the world and the same string, the printing function would always return the same next state of world, with that string printed to the console.

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

To remain pure, functions would aditionally require a World. For example, a function that prints a string to the console would take a String and the World (the World where the console screen is blank) and return the new modified World (the World where the console screen contains the string).


Thus, it becomes pure because given      
the same String and the same World state,     
it always produces the same resulting World:     
    `printStr :: String -> World -> World`

`whatIsYourPureName` is our impl of a pure function that reads the user- provided input string from the stdin. In fact, it takes the current world (where it didn't ask for input) and produces a pair consisting of a String (a user's input) and the new World (i.e. the world where it does ask a user for his name, then prints it).

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
