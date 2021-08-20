# Newtypes

## Function coercion

Function coercion is a concern when a newtype is wrapping a function.

The internal module Data.Functor.Utils contains the code:

```hs
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
```

Several functions use `#.` instead of the regular `.` to avoid potential efficiency problems relating to #7542. The problem, in a nutshell:

If `N` is a newtype constructor, then `N x` will always have the same representation as `x` (something similar applies for a newtype deconstructor). However, if `f` is a function, then

```hs
N . f = \x -> N (f x)
```

which looks almost the same as `f`, except that *the eta expansion* will lift it, so that the LHS could be `⟘`, but the RHS never is. This can lead to very inefficient code.

Thus, we steal a technique from Shachaf and Edward Kmett and adapt it to the current (rather clean) setting. Instead of using `N . f`, we use `N #. f`, which is just

```hs
coerce f `asTypeOf` (N . f)
```

That is, we "pretend" that `f` has the right type, and thanks to the safety of `coerce`, the type checker guarantees that nothing really goes wrong. We still have to be a bit careful, remembering that `#.` completely ignores the value of its left operand due to `asTypeOf`:

```hs
asTypeOf :: a -> a -> a
asTypeOf _ x = x
```
