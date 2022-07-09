# Declaration vs expression style of programming in Haskell

https://wiki.haskell.org/Declaration_vs._expression_style

Haskell has two styles of writing functional programs (due to different language designers preferring different approaches, i.e. "let's agree to disagree" approach to language design).

* *Declaration style* 
  an algorithm is specifed in terms of equations that need to be satisfied.
* *Expression style* 
  larger expressions are built by composing smaller ones.

Declaration vs expression style - summary of distinctions:

Declaration style | Expression style
------------------|-----------
where             | let
named functions   | lambdas
pattern matching  | case
guards            | if


## Illustration

To illustrate the two styles, SPJ offers examples in terms of the `filter` function.

```hs
filter :: (a -> Bool) -> [a] -> [a]

-- Declaration style
filter p [] = []
filter p (x:xs)
  | p x   = x : rest
  | otherwise = rest
  where
  rest = filter p xs

-- Expression style
filter =
  \p -> \ xs ->
    case xs of
      []     -> []
      (x:xs) ->
        let rest = filter p xs
        in  if p x
              then x : rest
              else     rest
```

## Characteristics

Characteristic syntactic elements of the styles:

* Declaration style
  - `where` clause
  - function args on the lhs, `f x = x * x`
  - pattern matching in function definitions, `f [] = 0`
  - guards, `f [x] | x > 0 = 'a'`

* Expression style
  - `let` expression
  - function args on the rhs in a lambda, `f = \x -> x * x`
  - case expression, `f xs = case xs of [] -> 0`
  - if expression, `f [x] = if x > 0 then 'a' else ...`
