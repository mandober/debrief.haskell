# Input and output in a purely functional language

- purity
- referential transparency
- side-effects
- input/output
- equations
- memoization

Purely functional programming language
Mathematical functions and equations
Modelling side-effects in a pFPL while remaining pure


## Modelling side-effects in a purely functional language

It seems impossible for a pure FPL to integrate operations that exert side-effects and remain pure. After all, to be pure, a function must not enduldge in side-effecting. A lot of side-effects have to do with the basic input/output operations, such as:
- print something to the console
- displaying a graphic on the screen
- interacting with a user, like reading in user input from a keyboard
- accessing file descriptors
- reading and writing files and other FS items
- accessing network, DBs, sockets, etc.

Without side-effects a programming language is useless, it might perform some calculations but it wouldn't be able to display the results. And a lazy FPL would never even attempt to start any work because if there's no way to show the results then there might as well be nobody to witness them so why produce them in the first place.

~

A procedure that reads user input cannot be a function, particularly in the pure mathematical sense, so such "functions" are referred to as actions and have the type , `IO`, that also serves as a marker for their easy recognition.

~

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

The bracket function is like a RAII technic, used to perform actions safely wrt to the resources. When a function needs to open a file and write to it, it will eventurally need to close that file, even if an error occured during. The bracket makes that error-safe.

`bracket` takes 3 args:
- IO a      :: action to perform at the beginning
- a -> IO b :: action to perform at the end, regardless of errors
- a -> IO c :: action to perform in the middle, which may error

```hs
bracket :: IO a             -- initial action
        -> (a -> IO b)      -- finally action
        -> (a -> IO c)      -- middle unsafe (try) action (error-prone)
        -> IO c
```

For instance, the following function writes some chars in a file. It first needs to open the file, write the characters and then close the file. However, if writing fails, `hClose` will still be executed, and the exception will be re-raised afterwards. That way, you don't need to worry about catching the exceptions and about closing all of the handles yourself.

```hs
writeChar :: FilePath -> Char -> IO ()
writeChar fp c =
    bracket
    (openFile fp ReadMode)      -- initial action
    hClose                      -- finally action
    (\h -> hPutChar h c)        -- unsafe middle action
```
