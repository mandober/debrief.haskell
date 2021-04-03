# Strictness in Haskell


Bang patterns and Strict Haskell
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#bang-patterns-and-strict-haskell

- eliminating thunks
- making sure an expr is evaluated (coz of, e.g., the ordering of effects)


## Strictness flag

Whenever a data constructor is applied, each argument to the constructor is evaluated iff the corresponding type in the algebraic datatype declaration has a **strictness flag**, denoted by an exclamation point.


```hs
-- 2nd arg to Cons will be evaluated before Cons is applied
data List a | Nil = StCons a !(List a)
```


To illustrate the difference between strict versus lazy constructor application, consider the following:

```hs
stList = StCons 1 undefined
lzList = (:)    1 undefined

stHead (StCons h _) = h -- evaluates to undefined when applied to stList
lzHead (h : _)      = h -- evaluates to 1 when applied to lzList
```

## Bang patterns

https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/strict.html

`!` is also used in the "bang patterns" (GHC extension), to indicate strictness of the patterns in matching.

```hs
f !x !y = x + y
```



## Strictness

Haskell is lazy but sometimes that works against it. A classic example is the `foldl` which lazily builds a sequence of thunks as it traverses a list. Now, with large enough lists all those unevaluated thunks, quickly deplating memory resources, become a huge problem.

```hs
foldl (+) (((0 + 1) + 2) + 3) []
```

To combat this problem, Haskell has a strict form of folding functions, identified by the ticks: `foldr'` and `foldr'`. They both use the services of the `seq` function. With `foldl'` instead of the regular `foldl`, the equation above is evaluated eagrly: the accumulator is evaluated each time a new list element is received, thus keeping the running cost low.

```hs
-- Data.Foldable
foldr' :: (a -> b -> b) -> b -> t a -> b
foldl' :: (b -> a -> b) -> b -> t a -> b
```

The `seq` function is special: it's not like any other function, but it is implemented by the compiler itself! This is because only the compiler can implement it and ensure it important property: the `seq` function is strict in its first argument; the type `a` is forcibly evaluated before the type `b` is returned:

```hs
seq :: a -> b -> b
seq x y = y
```

`seq` is the only function with this property. It is used by `foldl'`to force evaluation of accumulator as a list is traversed.

```hs
foldl'  :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x:xs) =
    let z' = f z x
    in  z' `seq` foldl' f z' xs
```

`seq` is also used in the definition of the `$!`:

```hs
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
```

Returning back from an IO monad should produce the fully evaluated expression; we don't expect a thunk there because we're probably counting that we'll have an in normal form.

When compiling a program with optimizations tuned on (it kicks from the first optimizations level on), GHC will do a strictness analysis to figure out where a strict variant of a function might be better suited. In case of thunk pile-up due to laziness of `foldl`, GHC would conculde that it shopuld replace it with the strict version, i.e. with `foldl'`.

However, sprinkling strictness where it's not meant to go can be dangerous and performance-inhibiting. It seems it's best to stick with the well-known cases, although if GHC is able to do it automatically, why bother.
