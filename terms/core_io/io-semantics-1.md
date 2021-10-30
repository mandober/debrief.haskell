# IO Semantics

https://wiki.haskell.org/IO_Semantics

For simplicity, the example only presents tty IO semantics.

The idea is to define IO as:

```hs
data IO a
  = Done a
  | PutChar Char (IO a)
  | GetChar (Char -> IO a)
```

Think of the `IO a` data type as a tree. `Done` is the type of its leaves;leaves hold the result of the program, `a`. `PutChar` is a node that holds one character of data (in its `Char` field), and has one child tree (the `IO a` field). `GetChar` is a node that has many children, but holds no data itself. It has one child for every `Char`, which is what the function `Char -> IO a` means; BTW, the cardinality of `a -> b` is `bᵃ`, thus `(IO a)ᶜʰᵃʳ`.

The tree contains all the info needed to execute tty interactions. One interprets (or executes) an `IO a` by tracing a route from root of the tree to a leaf.
* If a `PutChar` node is encountered, the character data contained at that node is output to the terminal and then its subtree is executed. It is at this point that the Haskell's code is evaluated in order to determine what character should be displayed before continuing.
* If a `GetChar` node is encountered, a character is read from the terminal (blocking if necessary) and the subtree corresponding to the character received is executed.
* If `Done` is encountered the program ends. Done holds the result of the computation, but in the case of `main :: IO ()` the data is of type (), thus it contains no info and is ignored.

This execution is not done in pure code ("in Haskell"), instead, it is performed by the (dirty, C-based) RT system.

The monadic operations are then defined as follows:

```hs
return :: a -> IO a
return x = Done x

(>>=) :: IO a -> (a -> IO b) -> IO b
Done x >>= f = f x
PutChar c x >>= f = PutChar c (x >>= f)
GetChar g >>= f = GetChar (\c -> g c >>= f)
```

So, the `return` fn is just another name for `Done`.

The bind operation takes a tree `x` and a function `f` and replaces the `Done` nodes (the leaves) of `x` by a new tree produced by applying `f` to the data held in the `Done` nodes.

The primitive IO commands are defined using these constructors:

```hs
putChar :: Char -> IO ()
putChar x = PutChar x (Done ())

getChar :: IO Char
getChar = GetChar (\c -> Done c)
```

The function `putChar` builds a small `IO ()` tree that contains one `PutChar` node holding the character data followed by `Done`.

The function `getChar` builds a short `IO Char` tree that begins with a `GetChar` that holds one `Done` node holding every character.

Other tty commands can be defined in terms of these primitives:

```hs
putStr :: String -> IO ()
putStr = mapM_ putChar
```

In general, `IO a` represents the desired interaction with the OS. For every system call, there will be a corresponding ctor in `IO` tree of the form:

```hs
    | ...
    | SysCallName p₁ p₂ … pₙ (r -> IO a)
    | ...
```

where `pᵢ` are the parameters for the system call, and `r` is its result. Thus, `PutChar` and `GetChar` won't be the data ctors of IO tree if they don't correspond to appropriate system calls.
