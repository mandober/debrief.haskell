# Evaluation order

Haskell is a lazy language, and its evaluation strategy enables us to write code such as the following:

```hs
ghci> take 10 (iterate (+5) 0)
-- [0,5,10,15,20,25,30,35,40,45]
```

Let us now attempt a similar feat at the type level. First, we define type families that correspond to `take` and `iterate (+5)`

```hs
type IteratePlus5 :: Nat -> [Nat]
type family IteratePlus5 k where
  IteratePlus5 k = k : IteratePlus5 (k+5)

type Take :: Nat -> [a] -> [a]
type family Take n a where
  Take 0 xs = '[]
  Take n (x : xs) = x : Take (n-1) xs
```

`Take` works as expected

```hs
ghci> :kind! Take 3 [0, 1, 2, 3, 4, 5]
Take 3 [0, 1, 2, 3, 4, 5] :: [Nat]
= '[0, 1, 2]
```

but `IteratePlus5` sends the type checker into an infinite loop

```hs
ghci> :kind! Take 10 (IteratePlus5 0)
^CInterrupted.
```

> Clearly, the evaluation of type families is not lazy. In fact, it is not eager either - the rules are not defined at all.

Even when working with finite data, reasoning about time or space complexity of algorithms implemented as type families is impossible.

GHC issue *#18965* offers a solution to this problem. In the meantime, it is a pitfall one must be aware of.

https://gitlab.haskell.org/ghc/ghc/-/issues/18965
