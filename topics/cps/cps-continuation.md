# Continuations

fu :: a -> a ->  `a`
ck :: a -> a -> `(a -> b) -> b`


```hs
-- 1
-- unremarkable function directly returns the retval to the caller
add :: (Num a) => a -> a -> a
add x y = x + y
-- the caller calls the unremarkable function with a couple of
-- numbers, immediately getting back the result from the callee.
x1 :: (Num a) => a
x1 = add 5 4

-- 2
-- passing in the (mapping) function f along with the two args.
-- the function `fadd` now has to apply `f` to the retval before returning it.
-- (fadd's signature is adjusted accordingly)
--      (Num a) => a -> a ->  a
fadd :: (Num a) => a -> a -> (a -> b) -> b
fadd x y f = f $ x + y
-- the call now has an extra arg, the mapping function
x2 :: (Num a) => a
x2 = fadd 5 4 (+6)

-- 3
-- instead of passing it a mapping function (a -> b), how about we pass it
-- a continuation k instead (a -> r). Shikses! That's the same thing!
kadd :: (Num a) => a -> a -> (forall r. (a -> r) -> r)
kadd x y k = k $ x + y
-- the extra arg was the cont all along (!) Now, isn't that special?
x3 :: IO ()
x3 = kadd 4 5 print
```

## Continuation spotting

Continuations have the signature `(a -> r) -> r`, where `a` is/was the type of the output value that a function was about to return before all this continuation nonsense. But now, `a` is not returned but it gets applied to the continuation `(a -> b)`, which is just a plain old unary mapping function that is to be applied to the retval, so the result of that is `b` which is actually returned.

That is, the end of the signature : `... -> a`    
with a passed continuation becomes: `... -> (a -> b) -> b`

However, the `b` (or `r` which is often used instead to instill the emerging significance) is a type the callee knows nothing about. 



from the continuation and `r` is the final result after the callback is applied.

`forall r. (a -> r) -> r`

-- continuation signature: forall r. (a -> r) -> r

type Cont a = forall r. (a -> r) -> r

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }
