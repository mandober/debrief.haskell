# Pipes

- the `pipes-4.1.0` stream processing library is a solution for lazy IO


## Pipes Ref

- Streaming: Pipes (What I Wish I Knew When Learning Haskell)
  http://dev.stephendiehl.com/hask/#pipes

- Pipes tutorial
  https://hackage.haskell.org/package/pipes-4.1.0/docs/Pipes-Tutorial.html




## Streaming: Pipes
(What I Wish I Knew When Learning Haskell)
http://dev.stephendiehl.com/hask/#pipes

`Pipes` is a stream processing library with a strong emphasis on the static semantics of composition.

```hs
await :: Monad m => Pipe a y m a
yield :: Monad m => a -> Pipe x a m ()

(>->) :: Monad m
      => Pipe a b m r
      -> Pipe b c m r
      -> Pipe a c m r

runEffect :: Monad m => Effect m r -> m r
toListM :: Monad m => Producer a m () -> m [a]
```

The simplest usage is to connect "pipe" functions with a *pipe composition operator*, `>->` , where each component can `await` and `yield` to *push and pull* values along the stream.


```hs
import Pipes
import Pipes.Prelude as P
import Control.Monad
import Control.Monad.Identity

a :: Producer Int Identity ()
a = forM_ [1..10] yield

b :: Pipe Int Int Identity ()
b =  forever $ do
  x <- await
  yield (x*2)
  yield (x*3)
  yield (x*4)

c :: Pipe Int Int Identity ()
c = forever $ do
  x <- await
  if (x `mod` 2) == 0
    then yield x
    else return ()

result :: [Int]
result = P.toList $ a >-> b >-> c
```


For example we could construct a "FizzBuzz" pipe.

```hs
{-# LANGUAGE MultiWayIf #-}

import Pipes
import qualified Pipes.Prelude as P

count :: Producer Integer IO ()
count = each [1..100]

fizzbuzz :: Pipe Integer String IO ()
fizzbuzz = do
  n <- await
  if | n `mod` 15 == 0 -> yield "FizzBuzz"
     | n `mod` 5  == 0 -> yield "Fizz"
     | n `mod` 3  == 0 -> yield "Buzz"
     | otherwise       -> return ()
  fizzbuzz

main :: IO ()
main = runEffect $ count >-> fizzbuzz >-> P.stdoutLn
```

To continue with the degenerate case we constructed with Lazy IO, consider than we can now compose and sequence deterministic actions over files without having to worry about effect order.

```hs
import Pipes
import Pipes.Prelude as P
import System.IO

readF :: FilePath -> Producer String IO ()
readF file = do
    lift $ putStrLn $ "Opened" ++ file
    h <- lift $ openFile file ReadMode
    fromHandle h
    lift $ putStrLn $ "Closed" ++ file
    lift $ hClose h

main :: IO ()
main = runEffect $ readF "foo.txt" >-> P.take 3 >-> stdoutLn
```

This is a simple sampling of the functionality of pipes. The documentation for pipes is extensive and great deal of care has been taken make the library extremely thorough. pipes is a shining example of an accessible yet category theoretic driven design.


See: [Pipes Tutorial](https://hackage.haskell.org/package/pipes-4.1.0/docs/Pipes-Tutorial.html)
