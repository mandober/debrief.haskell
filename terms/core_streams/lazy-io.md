# Lazy IO


## Lazy IO breaks equational reasoning
http://okmij.org/ftp/Haskell/index.html#lazyIO-not-True


* Lazy IO is like mmap
* memory-mapped IO reads the whole file in a memory string
* constant memory use is a great advantage of Lazy IO
* In lazy IO reading happens on demand
* see `pipes`, `conduit` packages


## Streaming: Lazy IO

(What I Wish I Knew When Learning Haskell)

The problem with using the usual monadic approach to processing data accumulated through I/O is that the Prelude tools require us to manifest large amounts of data in memory all at once before we can even begin computation.

```hs
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
mapM :: forall (t :: * -> *) (m :: * -> *) a b
     . (Traversable t, Monad m)
     => (a -> m b)
     -> t a
     -> m (t b)

sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence :: forall (t :: * -> * (m :: * -> *) a
         . (Traversable t, Monad m)
         => t (m a)
         -> m (t a)
```

Reading from a text file creates a thunk for the string/text (file contents) which when forced will actually read the file.

The problem is that this method (lazy IO) ties the *ordering of IO effects* to *evaluation order* which is difficult to reason about.

Normally, the monad laws (sans `seq`) guarantee that these computations are identical; however, using lazy IO we can encounter a degenerate case.

```hs
import System.IO

main :: IO ()
main = do
  withFile "foo.txt" ReadMode $ \fd -> do
    contents <- hGetContents fd
    print contents
  -- "foo\n"

  contents <- withFile "foo.txt" ReadMode hGetContents
  print contents
  -- ""
```

> What we need is a system to guarantee deterministic resource handling with constant memory usage.

To that end both the `conduit` and `pipes` libraries solved this problem using different (though largely equivalent) approaches.
