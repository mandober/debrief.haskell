---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Monad_Transformers_Explained
page-title:       Monad Transformers Explained - HaskellWiki
---
# Monad Transformers Explained - HaskellWiki

Below is a short explanation of and motivating example for monad transformers.

Basically it's like making a double, triple, quadruple, ... monad by wrapping around existing monads that provide wanted functionality.

You have an innermost monad (usually `Identity` or `IO` but you can use any monad). You then wrap monad transformers around this monad to make bigger, better monads.

Concrete example: Suppose I was writing a server. Each client handling thread must be of type `IO ()` (that's because `forkIO :: IO () -> IO ThreadID`)).

Suppose also that this server has some configuration that (in an imperative program) would be global because client handling threads all need to query it.

```hs
data Config = Config Foo Bar Baz
```

One way of doing this is to use currying and making all the client threads of type `Config -> IO ()`. Not too nice because any functions they call have to be passed the `Config` parameter manually. The `Reader` monad solves this problem but we've already got one monad. We need to wrap `IO` in a `ReaderT`. The type constructor for `ReaderT` is `ReaderT r m a`, with `r` the shared environment to read from, `m` the inner monad and `a` the return type. Our `client_func` becomes:

```hs
client_func :: ReaderT Config IO ()
```

We can then use the `ask`, `asks` and `local` functions as if Reader was the only Monad:

(these examples are inside `do` blocks)

(Assuming some function `port :: Config -> Int` or similar.)

To do stuff in an inner monad, there is the function `lift`. Because having big chains of `lift $ lift $ lift $ foo` can get annoying, the libraries in mtl (the Monad Transformer Library) play a trick using the type system. Each monad in the mtl is defined in terms of a type class. For example, `Reader` is actually an instance of `MonadReader`, as is `ReaderT`. Further, anything in the mtl that wraps a `MonadReader` is also set up to be a `MonadReader`, so the `ask`, `asks` and `local` functions will work without any (manual) lifting. Other mtl monads behave in a similar way.

To do stuff in `IO` the `liftIO` function is used:

(given `h :: Handle`, the client's handle)

liftIO $ hPutStrLn h "You win"
liftIO $ hFlush h

A type class called `MonadIO` is used to implement a similar trick as above. `IO` is an instance of `MonadIO`, and any mtl transformer that wraps a `MonadIO` instance also is an instance of `MonadIO`. This means that `IO` functions need only use `liftIO` and not a big chain of `lift`s. Note also that `IO` has no transformer and must therefore always be the innermost monad.

This is all well and good, but the `client_func` now has type `ReaderT Config IO ()` and `forkIO` needs a function of type `IO ()`. The escape function for `Reader` is `runReader :: Reader r a -> r -> a` and similarly for `ReaderT` the escape function is `runReaderT :: ReaderT r m a -> r -> m a`:

(Given some `c :: Config` that's been assembled from config files or the like)

forkIO (runReaderT client\_func c)

Will do the trick.

Monad transformers are like onions. At first, they make you cry but then you learn to appreciate them. Like onions, they're also made of layers. Each layer is the functionality of a new monad, you lift monadic functions to get into the inner monads and you have transformerised functions to unwrap each layer. They're also like a present in that regard: in this example we unwrapped the outer wrapping paper to get to the present: an object of type `IO ()`, which lets us make Haskell do something.
