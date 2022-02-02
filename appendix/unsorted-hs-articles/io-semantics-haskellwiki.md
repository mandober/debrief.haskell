# IO Semantics - HaskellWiki

> The following is inspired by Luke Palmer's post. This only describes one possible semantics of IO a; your actually implementation may vary.

Semantics of IO: A Free Approach
--------------------------------

The following is inspired by [Luke Palmer's post](http://luqui.org/blog/archives/2008/03/29/io-monad-the-continuation-presentation/). This only describes one possible semantics of `IO a`; your actually implementation may vary.

The idea is to define `IO` as

data IO a \= Done a
          | PutChar Char (IO a)
          | GetChar (Char \-> IO a)

For simplicity this an example of `IO` that only gives semantics for teletype IO.

Think of `IO a` as a tree whose leaves are `Done a` that holds the result of the program. `PutChar` is a node that has one child tree and the node holds one character of data. `GetChar` is a node that has many children; it has one child for every `Char`, but `GetChar` holds no data itself.

This tree contains all the information needed to execute teletype interactions. One interprets (or executes) an `IO a` by tracing a route from root of the tree to a leaf.

If a `PutChar` node is encountered, the character data contained at that node is output to the terminal and then its subtree is executed. It is at this point that Haskell code evaluated in order to determine what character should be displayed before continuing. If a `GetChar` node is encountered, a character is read from the terminal (blocking if necessary) and the subtree corresponding to the character received is executed. If `Done` is encountered the program ends. `Done` holds the result of the computation, but in the case of `main :: IO ()` the data is of type `()` and thus contains no information and is ignored.

This execution is not done anywhere in a haskell program, rather it is done by the run-time system.

The monadic operations are defined as follows:

return :: a \-> IO a
return x \= Done x

(\>>=) :: IO a \-> (a \-> IO b) \-> IO b
Done x \>>= f \= f x
PutChar c x \>>= f \= PutChar c (x \>>= f)
GetChar g \>>= f \= GetChar (\\c \-> g c \>>= f)

As you can see `return` is just another name for `Done`. The bind operation takes a tree `x` and a function `f` and replaces the `Done` nodes (the leaves) of `x` by a new tree produce by applying `f` to the data held in the `Done` nodes.

The primitive IO commands are defined using these constructors.

putChar :: Char \-> IO ()
putChar x \= PutChar x (Done ())

getChar :: IO Char
getChar \= GetChar (\\c \-> Done c)

The function `putChar` builds a small `IO ()` tree that contains one `PutChar` node holding the character data followed by `Done`.

The function `getChar` builds a short `IO Char` tree that begins with a `GetChar` that holds one `Done` node holding every character.

Other teletype commands can be defined in terms of these primitives

putStr :: String \-> IO ()
putStr \= mapM\_ putChar

More generally speaking, `IO a` will represent the desired interaction with the operating system. For every system call there will be a corresponding constructor in `IOTree` of the form

	| SysCallName p1 p2 ... pn (r \-> IO a)

where `p1` ... `pn` are the parameters for the system call, and `r` is the result of the system call. (Thus `PutChar` and `GetChar` will not occur as constructors of `IOTree` if they don't correspond to system calls)


[Source](https://wiki.haskell.org/IO_Semantics)