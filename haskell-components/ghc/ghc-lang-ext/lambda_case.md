# Lambda-case

- `LambdaCase` enables lambda-case expressions, the use of lambda-case syntax
- since: 7.6.1
- combinable with: `Arrows` (since 9.0.1)


The `LambdaCase` extension enables a shorthand for a somewhat frequent form where a lambda introduces a parameter, which is then immediately eliminated (matched) by a `case-of` expression. The point is to relieve the user of having to write a name that is gonna be used once and thrown away.

With `LambdaCase` the  parameter **binding is implicit**; you don't declare the formal parameter in your (unary) function - it is already implicitly bound - so you just `\case` it. If you accidentally declare a param anyway, your function will expect an extra arg (you can't make an accident if you've adorned the function with a signature - the type system's got you).

You don't declare formal params anywhere (not on the LHS as a proper formal param, nor on the RHS as a lambda bound param): the param will be implicitly bound, you just need to `\case` it. The `case-of` clause is replaced by a similar (yet with a hard to discern syntactical form) of the lambda-case, i.e. `\case`.

There is no: lambda-bound parameter (`\ppat`), no `->` indicator, it's `\case` instead of `case`, and the `of` part is absent.


## General form

LambdaCase extension enables expressions of the form:

```hs
        \case
\ppat -> case ppat of
  pattern (Match 1) -> exp 1
  -- ...
  pattern (Match N) -> exp N
  pattern _         -> Nothing
```

Additionally, since GHC 9.0.1, combining `LambdaCase` with `Arrows` allows `\case` syntax to be used as a command in `proc` notation:

```hs
proc x -> (f -< x) `catchA` \case
  p1 -> cmd1
  -- ...
  pN -> cmdN
```
