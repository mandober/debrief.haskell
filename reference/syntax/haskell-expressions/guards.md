# Guards

## Guards in function definitions



## Guards in case expression


## MultiWayIf

https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multiway_if.html#extension-MultiWayIf

- `MultiWayIf`
- Since: 7.6.1
- Allow the use of multi-way-if syntax

The extension -XMultiWayIf essentially introduces *standalone guards*, simplifying the use of guards that aren't at the outermost level of a function declaration or case-expression. Among other things, this made it easier to use guards inside of lambda expressions.



With MultiWayIf extension GHC accepts conditional expressions with multiple branches:

```hs
if | guard1 -> expr1
   | ...
   | guardN -> exprN

-- which is roughly equivalent to

case () of
  _ | guard1 -> expr1
  ...
  _ | guardN -> exprN
```

Multi-way if expressions introduce a new layout context. 
So the example above is equivalent to:

```hs
if { | guard1 -> expr1
   ; | ...
   ; | guardN -> exprN
   }

-- The following behaves as expected:

if | guard1 -> if | guard2 -> expr2
                  | guard3 -> expr3
   | guard4 -> expr4

-- because layout translates it as

if { | guard1 -> if { | guard2 -> expr2
                    ; | guard3 -> expr3
                    }
   ; | guard4 -> expr4
   }
```

Layout with multi-way if works in the same way as other layout contexts, except that the semi-colons between guards in a multi-way if are optional.

So it is not necessary to line up all the guards at the same column, which is consistent with the way guards work in *function definitions* and *case expressions*.


## Lambda-case

https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/lambda_case.html#extension-LambdaCase

- `LambdaCase`
- Since: 7.6.1
- Allow the use of lambda-case syntax.

The LambdaCase extension enables expressions of the form

```hs
\case { p1 -> e1; ...; pN -> eN }
```

which is equivalent to

```hs
\freshName -> case freshName of { p1 -> e1; ...; pN -> eN }
```

Since GHC 9.4.1, it also allow expressions with **multiple scrutinees** 
(see GHC proposal [#302][#302]) of the form:

```hs
\cases { p11 ... pM1 -> e1; ...; p1N ... pMN -> eN }
```

which is equivalent to a function defined as

```hs
f p11 ... pM1 = e1
...
f p1N ... pMN = eN
```

Note that both `\case` and `\cases` start a layout, so you can write

```hs
\case
  p1 -> e1
  ...
  pN -> eN
```


Additionally, since GHC 9.0.1, combining `LambdaCase` with `Arrows` allows `\case`, and since GHC 9.4.1 `\cases`, syntax to be used as a command in **proc notation**:

```hs
proc x -> (f -< x) `catchA` \case
  p1 -> cmd1
  ...
  pN -> cmdN
```


[#302]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0302-cases.rst
