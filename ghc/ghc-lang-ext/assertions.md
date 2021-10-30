# Assertions

If you want to make use of assertions in your standard Haskell code, you could define a function like the following:

```hs
assert :: Bool -> a -> a
assert False x = error ("assertion failed: " <> show x)
assert _     x = x
```

which works, but gives you back a less than useful error message - an assertion failed, but which and where?

One way out is to define an extended `assert` function which also takes a descriptive string to include in the error message and perhaps combine this with the use of a pre-processor which inserts the source location where `assert` was used.

GHC offers a helping hand here, doing all of this for you. For every use of `assert` in the user's source:

```hs
kelvinToC :: Double -> Double
kelvinToC k = assert (k >= 0.0) (k-273.15)
```

GHC will *rewrite this to also include the source location* where the
assertion was made,

`assert pred val ==> assertError "Main.hs|15" pred val`

The rewrite is only performed by the compiler when it spots applications of `Control.Exception.assert`, so you can still define and use your own versions of `assert`, should you so wish. If not, import `Control.Exception` to make use `assert` in your code.


GHC ignores assertions when optimisation is turned on with the `-O` flag. That is, expressions of the form `assert pred e` will be rewritten to `e`. You can also disable assertions using the `-fignore-asserts` option.

The option `-fno-ignore-asserts` allows enabling assertions even when optimisation is turned on.

Assertion failures can be caught, see the docs for the `Control.Exception` library for the details.
