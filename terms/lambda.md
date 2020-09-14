# Lambda forms

Anonymous function syntax in Haskell uses a backslash to represent a lambda. Wrap it in parentheses and apply it to values:


```hs
-- first, definition with let and lambda:
let id = \x -> x
-- then application:
id 10
-- 10

-- lambda application:
(\x -> x) 13
-- 13

-- application as let expr
let x = 5 in (\x -> x) x
-- 5


-- lambdas:
id    = (\x -> x)
true  = (\x y -> x)
false = (\x y -> y)


-- from let expr to lambdas:
--
let x = 10 in x + 9001
-- equivalent to
(\x -> x + 9001) 10
--
let a = b in c
-- equivalent to
(\a -> c) b


-- from where expr to lambdas:
--
x + 9001 where x = 10
-- equivalent to
(\x -> x + 9001) 10
--
c where a = b
-- equivalent to
(\a -> c) b
```
