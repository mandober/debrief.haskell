# Free monads

Free monads in 7 easy steps - Part I of the series Free monads, 2015
https://joa.sh/posts/2015-09-13-free-monad-steps.html
https://joa.sh/posts/2016-03-23-free-monads.html

What does Free buy us? 22 Sep 2017
https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

Control.Monad.Free
https://hackage.haskell.org/package/free


## Free monads

https://ekmett.github.io/reader/2008/monads-for-free/
https://ekmett.github.io/reader/2011/free-monads-for-less/
https://ekmett.github.io/reader/2011/free-monads-for-less-2/

```hs
data Free f a = Roll (f (Free f a)) | Return a

newtype Free f a = Free { unfree :: Either a (f (Free f a))) }
```

Usually `Free` as ADT is written using `Free` as newtype, around a sum type (i.e. Either) so you can write it using point-free style, but the ADT makes for clearer introduction.

The idea is that you take the functor and recursively fold it in upon a choice of either itself or a bare variable.


---

**Free monads** allow you to separate the semantics of your application from the details of implementation in a very flexible way. They are basically an easy way to create a DSL that specifies only the bare minimum to get monadic behaviour and do-notation.


```hs
data Free s a
```

`Free` can be thought of as a program, `s` as a language, and `a` is the program's result. Then we need a way to interpret that language (and program). The language is also called an algebra.

```hs
data Console a where
  PutStr :: String -> Console ()
  GetLine :: Console String
```
