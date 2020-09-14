# Let expression

- `let..in` expression is interchangable with a lambda expression:
- `let pat = body in expr` ~ `(\pat -> body) expr`
- let expressions support recursion, so `let x = x in x` loops forver
- pure functions use the `let..in` construct, IO uses just `let` (no `in`)
- in IO, let binds pure values, values within a context use `<-` (slurp)

In pure functions, the `let-in` expression is interchangable with a lambda expression:

(\ PARAM -> BODY) ARG  ≡  let PARAM = ARG in BODY

```hs
(\par -> par * 2) arg  ≡  let par = arg in par * 2

let (a, s) = runState f s
    
```





```hs

(\x -> x + 5) 3
let x = 3 in x + 5
-- 8

-- pattern matching can be used in both let and lambda expr
(\(x:xs) -> (x, xs)) "abc"
let (x:xs) = "abc" in (x, xs)
-- ('a', "bc") :: (Char, [Char])

-- btw, to convert a Char into a String, just wrap it in brackets
(\(x:xs) -> ([x], xs)) "abc"
let (x:xs) = "abc" in ([x], xs)
-- ("a", "bc") :: ([Char], [Char])


-- let expr are recursive so pay attention
let x = x in x
```
