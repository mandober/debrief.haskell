# 3.1. Introduction to GHCi

301-introduction.md

GHCi is GHC's interactive environment that includes an interactive debugger. GHCi can interactively evaluate Haskell expressions, interpret Haskell programs, load GHC-compiled modules, At the moment GHCi supports most of GHC's language extensions.


Let's start with an example GHCi session. You can fire up GHCi with the command `ghci`:

```sh
$ ghci
GHCi, version 8.y.z: https://www.haskell.org/ghc/  :? for help
ghci>
```

There may be a short pause while GHCi loads the prelude and standard libraries, after which the prompt is shown.

Haskell expressions can be typed at the prompt:

```sh
ghci> 1+2
3
ghci> let x = 42 in x / 9
4.666666666666667
ghci>
```

GHCi interprets the whole line as an expression to evaluate. The expression may not span several lines - as soon as you press enter, GHCi will attempt to evaluate it.

In Haskell, a `let` expression is followed by `in`. However, in GHCi, since the expression can also be interpreted in the `IO` monad, a `let` binding with no accompanying `in` statement can be signalled by an empty line, as in the above example.

Since GHC 8.0.1, you can bind values and functions to names without `let` statement:

```sh
ghci> x = 42
ghci> x
42
ghci>
```
