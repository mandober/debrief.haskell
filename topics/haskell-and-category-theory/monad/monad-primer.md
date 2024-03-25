---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/MonadPrimer
page-title:       MonadPrimer - HaskellWiki
article-title:    MonadPrimer - HaskellWiki
---
# MonadPrimer - HaskellWiki


#!/usr/bin/env runhaskell
module Main where
import Control.Applicative
import Control.Monad

\-- A crib of common monads, their behavior, and how helper functions
\-- behave when operating on them.
\-- Includes functions from Monad, Applicative and Functor.
main \= do
    \-- Maybe a
    \-- optional a value.
    putStrLn "----- Maybe a -----"
    print (return 3 :: Maybe Int)                 \-- Just 3
    print (pure 3 :: Maybe Int)                   \-- Just 3
    print (Just 3 \>>= \\n \-> Just (n+1))           \-- Just 4
    print (Just 3 \>>= return.(+1))                \-- Just 4
    print (Nothing \>>= return.(+1))               \-- Nothing
    print (return.(+1) \=<< Just 3)                \-- Just 4
    print (join (Just (Just 3)))                  \-- Just 3
    print (join (Just Nothing) :: Maybe Int)      \-- Nothing
    print (liftM (+1) (Just 3))                   \-- Just 4
    print (liftA (+1) (Just 3))                   \-- Just 4
    print ((+1) \`fmap\` Just 3)                    \-- Just 4
    print ((+1) \`fmap\` Nothing)                   \-- Nothing
    print (return (+ 1) \`ap\` Just 3)              \-- Just 4
    print (Just (+ 1) \`ap\` Just 3)                \-- Just 4
    print (Nothing \`ap\` Just 3 :: Maybe Int)      \-- Nothing
    print ((+ 1) <$> Just 3)                      \-- Just 4
    print ((+ 1) <$> Nothing :: Maybe Int)        \-- Nothing
    print (liftM2 (+) (Just 3) (Just 4))          \-- Just 7
    print (liftA2 (+) (Just 3) (Just 4))          \-- Just 7
    print (return (+) \`ap\` Just 3 \`ap\` Just 4)    \-- Just 7
    print ((+) \`fmap\` Just 3 \`ap\` Just 4)         \-- Just 7
    print ((+) <$> Just 3 <\*> Just 4)             \-- Just 7
    print ((+) <$> Nothing <\*> Just 4)            \-- Nothing
    print ((+) <$> Just 3 <\*> Nothing)            \-- Nothing
    print (guard True \>> Just 3)                  \-- Just 3
    print (guard False \>> Just 3)                 \-- Nothing
    print (mapM (\\n \-> guard (n<10) \>> Just (n+2)) \[2..4\])  \-- Just \[4,5,6\]
    print (mapM (\\n \-> guard (n<10) \>> Just (n+2)) \[8..14\]) \-- Nothing
    print (forM \[2..4\] (\\n \-> guard (n<10) \>> Just (n+2)) ) \-- Just \[4,5,6\]
    print (foldM (\\n m \-> guard (n<10) \>> Just (n+m)) 1 \[2..4\]) \-- Just 10
    print (foldM (\\n m \-> guard (n<10) \>> Just (n+m)) 5 \[2..4\]) \-- Nothing
    print (sequence \[Just 5, Just 6, Just 7\])     \-- Just \[5,6,7\]
    print (sequence \[Just 5, Nothing, Just 7\])    \-- Nothing

    \-- \[a\]
    \-- lists of a.
    putStrLn "----- \[a\] -----"
    print (return 3 :: \[Int\])                     \-- \[3\]
    print (pure 3 :: \[Int\])                       \-- \[3\]
    print (\[3\] \>>= \\n \-> \[n+1\])                   \-- \[4\]
    print (\[3\] \>>= return.(+1))                   \-- \[4\]
    print (\[\] \>>= return.(+1))                    \-- \[\]
    print (\[3\] \>>= \\n \-> \[n+1,n+10\])              \-- \[4,13\]
    print (\[3,5\] \>>= \\n \-> \[n+1,n+10\])            \-- \[4,13,6,15\]
    print (return.(+1) \=<< \[3\])                   \-- \[4\]
    print (join \[\[3\]\])                            \-- \[3\]
    print (join \[\[3\],\[4,5\],\[6,7,8\]\])              \-- \[3,4,5,6,7,8\]
    print (join \[\[\]\] :: \[Int\])                    \-- \[\]
    print (join \[\] :: \[Int\])                      \-- \[\]
    print (liftM (+1) \[3\])                        \-- \[4\]
    print (liftA (+1) \[3\])                        \-- \[4\]
    print ((+1) \`fmap\` \[3\])                       \-- \[4\]
    print ((+1) \`fmap\` \[\])                        \-- \[\]
    print ((+1) \`fmap\` \[3,30,300\])                \-- \[4,31,301\]
    print (return (+1) \`ap\` \[3\])                  \-- \[4\]
    print (\[(+1)\] \`ap\` \[3\])                       \-- \[4\]
    print (\[\] \`ap\` \[3\] :: \[Int\])                  \-- \[\]
    print (\[(+1),(+10)\] \`ap\` \[3\])                 \-- \[4,13\]
    print ((+ 1) <$> \[3\])                         \-- \[4\]
    print ((+ 1) <$> \[3,10,20\])                   \-- \[4,11,21\]
    print ((+ 1) <$> \[\] :: \[Int\])                 \-- \[\]
    print (liftM2 (+) \[3\] \[4\])                    \-- \[7\]
    print (liftA2 (+) \[3\] \[4\])                    \-- \[7\]
    print (return (+) \`ap\` \[3\] \`ap\` \[4\])          \-- \[7\]
    print ((+) \`fmap\` \[3\] \`ap\` \[4\])               \-- \[7\]
    print ((+) <$> \[3\] <\*> \[4\])                   \-- \[7\]
    print ((+) <$> \[3,10\] <\*> \[4,20\])             \-- \[7,23,14,30\]
    print ((+) <$> \[\] <\*> \[4,20\])                 \-- \[\]
    print ((+) <$> \[3,10\] <\*> \[\])                 \-- \[\]
    print (guard True \>> \[1,2,3\])                 \-- \[1,2,3\]
    print (guard False \>> \[1,2,3\])                \-- \[\]
    print (mapM (\\n \-> \[n+1,n+2\]) \[10,20\])        \-- \[\[11,21\],\[11,22\],\[12,21\],\[12,22\]\]
    print (forM \[10,20\] (\\n \-> \[n+1,n+2\]))        \-- \[\[11,21\],\[11,22\],\[12,21\],\[12,22\]\]
    print (foldM (\\n m \-> \[n+m,m+1\]) 5 \[10\])      \-- \[15,11\]
    print (foldM (\\n m \-> \[n+m,m+1\]) 5 \[10,100\])  \-- \[115,101,111,101\]
    print (sequence \[\[5\], \[6,7,8\], \[9\]\])          \-- \[\[5,6,9\],\[5,7,9\],\[5,8,9\]\]
    print (sequence \[\[5\], \[\], \[9\]\])               \-- \[\]
    \-- XXX foldM

    \-- XXX Error/(Either String) a?

    \-- ((->) a)
    \-- functions which take an argument of type a.
    putStrLn "----- ((->) a) -----"
    let testEnv f \= print (f 100)
    testEnv (const 3)                         \-- 3
    testEnv (return 3)                        \-- const 3 -> 3
    testEnv (pure 3)                          \-- const 3 -> 3
    testEnv (const 3 \>>= \\n \-> (+n))          \-- (+3) -> 103
    testEnv (\*2)                              \-- 200
    testEnv ((\*2) \>>= \\n \-> (+n))             \-- (\\n -> n\*2 + n) -> 300
    testEnv ((\\n \-> (+n)) \=<< const 3)        \-- (+3) -> 103
    \-- join :: (a->a-> ...) -> (a -> ...)
    testEnv (join (+))                        \-- (\\n -> n+n) -> 200
    testEnv (join (\*))                        \-- (\\n -> n\*n) -> 10000
    testEnv (join (const (const 3)))          \-- const 3 -> 3
    testEnv (liftM (+1) (const 3))            \-- (+1).(const3) -> 4
    testEnv (liftA (+1) (const 3))            \-- (+1).(const3) -> 4
    testEnv ((+1) \`fmap\` const 3)             \-- (+1).(const 3) -> 4
    testEnv ((+1) \`fmap\` (\*2))                \-- (+1).(\*2) -> 201
    testEnv (return (+1) \`ap\` const 3)        \-- (+1).(const3) -> 4
    testEnv ((+1) <$> const 3)                \-- (+1).(const3) -> 4
    testEnv (liftM2 (+) (\*2) (\*3))            \-- (\\n -> n\*2 + n\*3) -> 500
    testEnv (liftA2 (+) (\*2) (\*3))            \-- (\\n -> n\*2 + n\*3) -> 500
    testEnv (return (+) \`ap\` (\*2) \`ap\` (\*3))  \-- (\\n -> n\*2 + n\*3) -> 500
    testEnv ((+) \`fmap\` (\*2) \`ap\` (\*3))       \-- (\\n -> n\*2 + n\*3) -> 500
    testEnv ((+) <$> (\*2) <\*> (\*3))           \-- (\\n -> n\*2 + n\*3) -> 500
    \-- no guard
    testEnv (sequence \[(\*2),(+1),(\`div\`2)\])   \-- (\\n -> \[n\*2, n+1, n \`div\` 2\] -> \[200,101,50\]
    testEnv (mapM (\\n \-> (+n)) \[3,4,5\])       \-- (\\n -> \[n+3, n+4, n+5) -> \[103,104,105\]
    testEnv (forM \[3,4,5\] (\\n \-> (+n)))       \-- (\\n -> \[n+3, n+4, n+5) -> \[103,104,105\]
    \-- XXX foldM

    \-- XXX (State s) a
    \-- Stateful computations.

    \-- IO a
    \-- IO operations with results of type a.
    putStrLn "----- IO a -----"
    \-- Two example IO actions returning integers.
    \-- Assumes /tmp/val1 contains "5" and /tmp/val2 contains "10"
    let ex1 :: IO Int
        ex1 \= liftM read (readFile "/tmp/val1")
        ex2 :: Int \-> IO Int
        ex2 n \= liftM ((+n).read) (readFile "/tmp/val2")
        testIO :: Show a \=> IO a \-> IO ()
        testIO \= (\>>= print)
    testIO (return 3)                          \-- 3
    testIO (pure 3)                            \-- 3
    testIO (ex1)                               \-- 5
    testIO (ex1 \>>= ex2)                       \-- 15
    testIO (ex2 \=<< ex1)                       \-- 15
    \-- somewhat contrived
    \--  join :: IO (IO a) -> IO a
    testIO (join (return ex1))                 \-- 5
    testIO (liftM (+1) ex1)                    \-- 6
    testIO (liftA (+1) ex1)                    \-- 6
    testIO ((+1) \`fmap\` ex1)                   \-- 6
    testIO ((+1) \`fmap\` ex2 1)                 \-- 12
    testIO (return (+1) \`ap\` ex1)              \-- 6
    testIO ((+1) <$> ex1)                      \-- 6
    testIO (liftM2 (+) ex1 (ex2 1))            \-- 16
    testIO (liftA2 (+) ex1 (ex2 1))            \-- 16
    testIO (return (+) \`ap\` ex1 \`ap\` ex2 1)    \-- 16
    testIO ((+) \`fmap\` ex1 \`ap\` ex2 1)         \-- 16
    testIO ((+) <$> ex1 <\*> ex2 1)             \-- 16
    \-- no guard
    testIO (mapM ex2 \[3,4,5\])                  \-- \[13,14,15\]
    testIO (forM \[3,4,5\] ex2)                  \-- \[13,14,15\]
    testIO (sequence \[ex1, ex2 0\])             \-- \[5,10\]
    \-- XXX foldM
