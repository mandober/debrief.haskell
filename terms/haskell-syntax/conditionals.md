# Conditionals

```hs
-- if-then-else
if cond then true_part else false_part

exp1 = if a == 12
       then 14
       else 22

-- case
case exp of
    pat1  -> action1
    pat1  -> action2
    _     -> else_action

case x of
    [ ] -> 0
    [x] -> 1
    _   -> 2

-- Function conditionals (guards)
f x | x == []        = 1
    | length x == 12 = 15
    | otherwise      = -1
```



## if-then-else

https://wiki.haskell.org/If-then-else

- if-then-else is a language construct, syntactic sugar left to maintain some kind of familiarity between Haskell and other PLs

- it may be expressed with a custom fn that takes a predicate and 2 braches of the same type, which is also the overall type.

```hs
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
```


The advantages of the function `if'` over the syntax *if-then-else* are the same like for all such alternatives. Two important non-syntactic Haskell's strengths:
* types: for classification and documentation
* HOFs (higher order functions): as combinators

If `if'` were a regular function, each language tool would have been able to process it without hassle. Haddock could've generated documentation, a text editor could've make suggestions, Hoogle could've retrieve fns given sigs.


Ternary operator:

```hs
zipIf :: [Bool] -> [a] -> [a] -> [a]
zipIf = zipWith3 if'


zip3     :: [a] -> [b] -> [c] -> [(a, b, c)]
zipWith3 :: (a  ->  b  ->  c  ->  d)
         -> [a] -> [b] -> [c] -> [d]

zip     :: [a] -> [b] -> [(a, b)]
zipWith :: (a  ->  b  ->  c)
        -> [a] -> [b] -> [c]
```

Its infix version is the ternary operator (?:) as in C-like languages! It can be used as `(cond ? exp1 $ exp2)`.

Additionally, Church booleans can be represented compactly by sectioning on the (?), i.e. `(True?) = const`, `(False?) = flip const`.

```hs
(?) :: Bool -> a -> a -> a
(?) = if'

infixr 1 ?
```

From a list of expressions choose the one, whose condition is true. The first parameter is the default value. It is returned if no condition applies.

```hs
select :: a -> [(Bool, a)] -> a
select = foldr (uncurry if')
```

Easy lifting into monads (MonadReader in this case)

```hs
ifF :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
ifF = liftM3 if'
```







```hs
main :: IO ()
main = do
    inp <- getLine
    if length inp > 5
    then do
        putStrLn "too"
        putStrLn "short"
        main
    else do
        putStrLn "not short"
        putStrLn "enough"
        main
```


## Case

https://wiki.haskell.org/Case
