# The synergy with GADTs

The need for closed type families arises most often when working with GADTs. Here is a definition of heterogeneous lists:

```hs
type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x : xs)

infixr 5 :&
```

It can be used to represent sequences of values of different types:

```hs
h1 :: HList [Integer, String, Bool]
h1 = 42 :& "Hello" :& True :& HNil

h2 :: HList [Char, Bool]
h2 = 'x' :& False :& HNil
```

Just as with normal lists, we can define ops such as computing the length:

```hs
hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ :& xs) = 1 + hlength xs

ghci> hlength h1 -- 3
ghci> hlength h2 -- 2
```

However, even for something as trivial as concatenation we need type-level computation:

```hs
happend :: HList xs -> HList ys -> HList ??
```

What shall be the type of `happened h1 h2`? Well, it must include the elements of the first list and then the elements of the second. That is precisely what the `Append` type family implements:

```hs
happend :: HList xs -> HList ys -> HList (Append xs ys)
```

And that is the typical reason one would reach for *closed type families: to implement operations on GADTs*.
