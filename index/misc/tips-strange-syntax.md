# Strange syntax

## Pattern binding signature

```hs
doer :: Monad m => StackTrans m ()
doer = do
    x <- pop
    y <- pop
    let extract :: Stack -> Integ Stack = fromJust . match -- (1)
    case (extract x, extract y) of
        (Val n _, Val m _) -> pushs (n + m)

-- (1) inside a do-block, the let-expr binds a function to an identifier
let extract :: Stack -> Integ Stack = fromJust . match
-- equivalent
let (extract :: Stack -> Integ Stack) = fromJust . match
-- equivalent
let extract :: Stack -> Integ Stack
    extract = fromJust . match
```


## Patterns, as-patterns and pattern-binding


```hs
-- top-level function named `fib` (nothing unusual here)
fib n = 0 : 1 : [ a + b | (a,b) <- zip fib (tail fib) ] !! n

-- top-level function `fib` is also as-pattern `fib`
fib@(x:xs) = 0 : 1 : [ a + b | (a,b) <- zip fib (tail fib) ]
```
