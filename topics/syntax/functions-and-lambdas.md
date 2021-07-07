# Named functions and lambdas

In a function's signature, if the return type is a function itself, it may be written with or without parenthesis (it is equivalent); when written without the parenthesis the function is broken into parts, e.g. `...-> (a -> b)` would become `... -> a -> b`.

This is best observed within the signature of composition. But first let's start with two plain functions:

```hs
inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1
```

The signature of the composition operator that takes these two plain functions and composes them, is then:

```hs
(.:) :: (Int -> Int)   -- inc
     -> (Int -> Int)   -- dec
     -> (Int -> Int)   -- composed function, say, incdec

indec :: Int -> Int
indec = inc .: dec
```

Note that in the signature of compose operator (.:), each paramter is a function and the result is also a function of the same type like any of the params. The first param is a function (Int -> Int) and the second is also a function (Int -> Int), and the return type as well, (Int -> Int). The return type represents the composed function at the value level which takes one param of type Int, and returns a result of type Int .

Anyway, because a named function is equivalent to a lambda function, the return type may be stated with or without parenthesis:

```hs
-- defining the compose operator that takes 2 fns from Int to Int
-- and returns a new function from Int to Int
(.:) :: (Int -> Int)            -- inc
     -> (Int -> Int)            -- dec
     -> (Int -> Int)            -- returns composed function
(.:) f g = \x -> f (g x)

-- defining the compose operator that takes 3 args:
-- 2 fns from Int to Int and an Int, returning an Int
(.:) :: (Int -> Int)            -- function from Int to Int
     -> (Int -> Int)            -- function from Int to Int
     -> Int                     -- param of type Int
     -> Int                     -- returns an Int
(.:) f g x = f (g x)
```

There you have it. Whether the sig had a return type `(Int -> Int)` or split into `Int -> Int` the result was the same, although the handling was somewhat different: it the first case we had to use a lambda and in the second we used a normal function.
