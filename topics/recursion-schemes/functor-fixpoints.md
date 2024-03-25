# Functor fixed points

## Fixed points



## Data.Fix module

The module `Data.Fix` provides the definitions for fixed points of a functor.

Type `f` should be a `Functor` if you want to use simple recursion schemes or `Traversable` if you want to use monadic recursion schemes.

This style allows you to express recursive functions in non-recursive manner. You can imagine that a non-recursive function holds values of the previous iteration.

### Example

First we define a *base functor*. The type parameter `r` signifies the recursion points.

```hs
-- normal list
data List  a   = Nil | Cons a (List a)
```

Considering the regular `List` data type, the type parameter `a` signifies the data type of list elements. The `Cons` ctor is actually a pair, taking an element of type `a` and a list of type `List a`. However the `a` stands for any type at all, so it could as well be instantiated with the type `List a`, producing something like:

```hs
data List a               = Nil | Cons a               (List a)
data List (List a)        = Nil | Cons (List a)        (List (List a))
data List (List (List a)) = Nil | Cons (List (List a)) (List (List (List a)))
-- ...
```

We cannot write such type, but it suggest it is going to infinity. Going back from infinity to the beginning we find that the regular `List` is the least fixpoint of the `ListF` functor.



```hs
-- normal list
data List  a   = Nil | Cons a (List a)
-- ListF functor
data ListF a b = Nil | Cons a b deriving (Show, Functor)
```

The `List` is then a fixed point of the `ListF`:

```hs
type List a = Fix (ListF a)
```


We can write the `length` function. Note that the function we give to `foldFix` is not recursive. Instead the results of recursive calls are in `r` positions, and we need to deal only with one layer of the structure.

```hs
length :: List a -> Int
length = foldFix $ \x -> case x of
  Nil      -> 0
  Cons _ n -> n + 1
```

If you already have recursive type, like [Int], you can first convert it to `Fix (ListF a)` and then use `foldFix`. Alternatively, you can use *recursion-schemes* combinators which work directly on recursive types.




## Ref

Data.Fix module
https://hackage.haskell.org/package/data-fix/docs/Data-Fix.html
