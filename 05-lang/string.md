# Strings

A String is a sequence of characters.

In Haskell, `String` is represented by a *linked-list* of `Char` values, i.e. 
`[Char]`.


> :t putStrLn
putStrLn :: String -> IO ()
> :t print
print :: Show a => a -> IO ()

> :t 4
4 :: Num t => t
> :t (+)
(+) :: Num a => a -> a -> a
> :t (+2)
(+2) :: Num a => a -> a
> :t (++)
(++) :: [a] -> [a] -> [a]

Types are stated on a separate line, apart from definition:
```hs
hello :: String
hello = "hello"
```

*Concatenation* is the joining together of sequences of values. Often in Haskell this is meant with respect to the [] or “List” datatype, which also applies to String which is [Char]. The concatenation function in Haskell is (++) which has type: [a] -> [a] -> [a]
For example:     
> "tacos" ++ " " ++ "rock"
"tacos rock"

*Data structures* are a way of organizing data so that the data can be accessed conveniently or efficiently.
