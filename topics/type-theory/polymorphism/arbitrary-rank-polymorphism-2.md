# Arbitrary-rank polymorphism

* https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html
* https://wiki.haskell.org/Rank-N_types
* https://blog.poisson.chat/posts/2019-03-25-higher-rank-types.html

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/impredicative_types.html#impredicative-polymorphism



## Higher-rank types and higher-rank polymorphism

In the standard Haskell 2010 we can easily define simple polymorphic functions, but a GHC extension `RankNTypes` is required to define a function whose arg is a polymorphic function.

For example, the following function takes a "shuffle" function, `f`, as an argument, and applies it to two lists, each with a different base type:

```hs
-- shuffleBoth :: (???) -> ([Int], [String])
shuffleBoth f = (f [0 :: Int, 1, 2], f ["Higher", "Rank", "Poly"])
```

The type signature of this function cannot be inferred automatically - without a user-supplied signature, this won't compile.

The GHC complains about not being able to match the type `String` (the base type of the second list) with `Int`, which is the base type of the first.

The `f` arg function must work on both types of list, i.e. on `[Int] -> [Int]` and `[String] -> [String]`, and these two types cannot be unified, suggesting that a more general type, `[a] -> [a]`, should be the type of `f` function.

```hs
shuffleBoth :: ([a] -> [a]) -> ([Int], [String])
shuffleBoth f = (f [0, 1, 2], f ["Higher", "Rank", "Poly"])

-- with explicit forall, the signature is:
shuffleBoth :: forall a. ([a] -> [a]) -> ([Int], [String])
shuffleBoth f = (f [0, 1, 2], f ["Higher", "Rank", "Poly"])
```

But now the GHC complains:

```
Couldn't match type 'a' with 'Int'
  Expected: [Int]
    Actual: [a]
  'a' is a rigid type variable bound by
    the type signature for:
      shuffleBoth :: forall a. ([a] -> [a]) -> ([Int], [String])
```

The solution is to make sure the `RankNTypes` is on and then to tuck the universal quantifier under the parens, so it becomes the existential quantifier:

```hs
shuffleBoth :: (forall a. [a] -> [a]) -> ([Int], [String])
shuffleBoth f = (f [0, 1, 2], f ["Higher", "Rank", "Poly"])
```
