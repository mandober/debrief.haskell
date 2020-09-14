# Sectioning

**Sectioning** is a way to quickly define a partially applied operator or a function. Most commonly, a (binary) operator gets partially applied to either the left or the right argument, which is immaterial in case of associative operators.


```hs
-- partially applications of the LEFT arg
[ (7:), (42*), (2^), ([1..5]++), (4 `elem`), (2<=) ]

-- partially applications of the RIGHT arg
[ (++[]), (/2), (`elem` [1..6]), (>4), (==5) ]


-- sectioning as anonymous function definition
(3 `elem`) [1..5]
-- equivalent to an anonymous lambda
(\xs -> 3 `elem` xs) [1..5]

-- or to a named function definition
f = (3 `elem`)
g = (`elem` [2..7])
-- equivalent to
f = \xs -> 3 `elem` xs
g = \x -> x `elem` [2..7]
-- or
f xs = 3 `elem` xs
g x = x `elem` [2..7]
-- application
f [1..5]
g 3

-- infixed functions can also be sectioned:
f = (`elem` [1..5])

-- infixed data ctors can also be sectioned:
f = (33 `Cons`)
lst1 = f (22 `Cons` (11 `Cons` Nil))
g = (`Cons` lst1)
g 44
```

Operators are commonly used in sectioning, but apart from them, regular functions in infix form can also be used.
