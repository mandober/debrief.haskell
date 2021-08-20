# Tackling the Awkward Squad in Haskell
Simon Peyton Jones, 2001

The awkward squad:
- MIO (monadic IO)
- CCR (Concurrency)
- EER (Errors, Exceptions)
- FFI (foreign libs call)


<!-- TOC -->

- [Input and output](#input-and-output)
    - [The stream I/O model](#the-stream-io-model)
- [Monadic I/O](#monadic-io)
    - [do notation](#do-notation)
- [Control structures](#control-structures)
- [References](#references)

<!-- /TOC -->

To write programs that are useful, the programmer must eventually confront *the Awkward Squad*, a range of un-beautiful but crucial issues, generally concerning interaction with the external world:
- Input and output
- Error detection and recovery; for example, perhaps the program should time out if something does not happen in time
- Concurrency, when the program must react in a timely way to independent input sources
- Interfacing to libraries or components written in other languages

The call-by-value (or strict) family of FPL have taken a pragmatic approach, adopting the strategy of IPL (imperative programming languages). To print something, there is a printChar function that has the side effect of printing a character. However, printChar isn't really a function anymore (because it has a side effect), but in practice this approach works ok, provided you specify the order of evaluation as part of the language design. This is what almost all PLs do, from FORTRAN and Java to iFPs (impure FPLs) like Lisp and SML.

Call-by-need (or lazy) languages (such as Haskell) have the evaluation order deliberately unspecified.

But why is it so? Well, suppose that we were to add side-effecting "functions" such as `printChar`. Consider this list: `xs = [printChar 'a', printChar 'b']`. What does it mean?! Because, unlike SML, where the evaluation would print 'a' followed by 'b', in Haskell, the calls to `printChar` are only executed if the elements of the list are evaluated. For example, if we use `xs` as in `length xs` then nothing is printed, since `length` does not touch the elements of the list (it calculates the lenght without needing to look at the elements).

> Practically, laziness and side effects are incompatible. If you want laziness, you must use a purely functional language; if you want side effects, you must use a strict language.

For a long time this situation was rather embarrassing for the lazy PL community: even the I/O for pFPL was weak and unconvincing, let alone error recovery, concurrency, and similar side-effect-y concepts. However, the situation has changed thanx to monads. Using monads, we have found a way how to structure programs so we can, in effect, do imperative programming where that is what we want, and only where we want. The `IO` ctors in function signatures can be considered as markers for side-effects.


## Input and output

A purely functional program implements a function; it has no side effect. Yet the ultimate purpose of running a program is invariably to cause some side effect: a changed file, some new pixels on the screen, a message sent, etc.

If the side effect can't be in the functional program, it will have to be outside it. Perhaps the functional program could be a function mapping an input string to an output string:

```hs
main :: String -> String
```

A "wrapper" program, written in C, can get an input string from somewhere (stdin, file), apply the function to it, and store the result string somewhere (stdout, another file). The functional programs must remain pure, so we locate all impurity in the wrapper program. The problem is that one sin leads to another: what if we need to read or write more than one file, delete files, open sockets, sleep for a while, etc.?

### The stream I/O model

The next approach, that was actually adopted by the first version of Haskell, is to enrich the argument and result type of the main function:

```hs
main :: [Response] -> [Request]
```

Now the program takes as its argument a (lazy) list of Response values and produces a (lazy) list of Request values. Informally a Request says something like "please get the contents of file /etc/motd", while a Response might say "the contents you wanted is: No email today". More concretely, Request and Response are both ordinary algebraic data types, something like this:

```hs
type FilePath = String

data Request
    = ReadFile FilePath
    | WriteFile FilePath String
--  | etc.

data Response
    = RequestFailed
    | ReadSucceeded String
    | WriteSucceeded
--  | etc.
```

However, there is still a wrapper program. It repeatedly takes a request off the result list, acts on the request, and attaches an appropriate response to the argument list. There has to be some clever way to deal with the fact that the function is to be applied to a list of responses before there are any responses in the list, but that isn't a problem in a lazy setting.

This request/response story is expressive enough that it was adopted as the main input/output model in the first version of Haskell, but it has several defects:
* It is hard to extend. New input or output facilities can be added only by extending the Request and Response types, and by changing the "wrapper" program. Ordinary users are unlikely to be able to do this.
* There is no very close connection between a request and its corresponding response. It is extremely easy to write a program that gets one or more "out of step".
* Even if the program remains in step, it is easy to accidentally evaluate the response stream too eagerly, and thereby block emitting a request until the response to that request has arrived - which it won't.

Rather than elaborate on these shortcomings, we move swiftly on to a better solution, namely monadic I/O. Hudak and Sundaresh give a useful survey of approaches to purely-functional input/output, which describes the pre-monadic state of play.

Hudak and Sundaresh: *On the Expressiveness of Purely Functional I/O Systems*


## Monadic I/O

The big breakthrough in input/output for purely-functional languages came with the monads as a general structuring mechanism for functional programs. Here is the key idea:

> A value of type `IO a` is an "action" that, when performed, may do some I/O, before delivering a value of type `a`.

A more concrete way of looking at these actions:

```hs
type IO a = World -> (a, World)
```

This type definition says that a value of type `IO a` is a function that, when applied to an argument of type `World`, delivers a new `World` together with a result of type `a`.

The idea is rather program-centric: the program takes the state of the entire world as its input, and delivers a modified world as a result, modified by the effects of running the program.

We may visualise a value of type IO a like this:

```
            ┌──────┐ ~> result :: a
            │ IO a │
World in ~> └──────┘ ~> World out
```

In general, we will call a value of type IO a an *I/O action*, or sometimes *computation*.

We can give IO types to some familiar operations, which are supplied as primitive: `getChar :: IO Char`. getChar is an I/O action that, when performed, reads a character from the stdin (thereby having an effect on the world outside the program), and returns it to the program as the result of the action.

```
         ┌─────────┐ ~> Char
         │ getChar │
World ~> └─────────┘ ~> World
```

`putChar :: Char -> IO ()` is a function that takes a character and returns an action that, when performed, prints the character on the stdout (its effect on the external world), and returns the trivial value i.e. unit, `()`.

```
Char  ~> ┌─────────┐ ~> ()
         │ putChar │
World ~> └─────────┘ ~> World
```

Suppose we want to read a character, and print the character we have read. Then we need to glue together putChar and getChar into a compound action, like this:

```
    getChar >>= putChar

         ┌─────────┐ ~> Char  ~> ┌─────────┐ ~> ()
         │ getChar │             │ putChar │
World ~> └─────────┘ ~> World ~> └─────────┘ ~> World
```


For this we use a glue combinator, provided as primitive:

```hs
(>>=) :: IO a -> (a -> IO b) -> IO b

-- With it, we can combine similar actions:
getChar >>= putChar     :: IO ()
getLine >>= putStrLn    :: IO ()

echoc :: IO ()
echoc = getChar >>= putChar

echo :: IO ()
echo = getLine >>= putStrLn
```

Suppose that we wanted to perform 'echo' twice in succession. We can't say: `echo >>= echo` because bind expects a function as the second arg, not an action. To adjust, we must throw away the result, `()`, of the first echo action. For that we define a glue combinator "then" (`>>`) in terms of the `bind`:

```hs
(>>) :: IO a -> IO b -> IO b
(>>) a1 a2 = a1 >>= (\_ -> a2)

echo2 :: IO ()
echo2 = echo >> echo
```

In practice, it is common for the second arg of (>>=) to be a lambda abstraction. For example, here is how we could read a char, and print it twice:

```hs
echoDup = getChar >>= (\c -> (putChar c >> putChar c))
```

All the parentheses in this example are optional, because a lambda abstraction extends as far to the right as possible, so you'll often see this layout:

```hs
echoDup :: IO ()
echoDup = getChar >>= \c ->
          putChar c >>
          putChar c
```

How could we write an I/O action that reads two characters, and returns both of them? We can start well enough:

```hs
getTwoChars :: IO (Char,Char)
getTwoChars = getChar >>= \c1 ->
              getChar >>= \c2 ->
              ???
```

But what are we to put for the "???"? It must be of type `IO (Char,Char)`, but we have done all the input/output required. What we need is one more combinator:

```hs
return :: a -> IO a
```

The action `return v` is an action that does no IO and immediately returns `v` without having any side effects.

```hs
getTwoChars :: IO (Char,Char)
getTwoChars = getChar >>= \c1 ->
              getChar >>= \c2 ->
              return (c1,c2)

-- Here is a more realistic action that reads a whole line of input:
getInput :: IO [Char]
getInput = getChar >>= \c ->
           if c == '\n' then
              return []
           else
              getInput >>= \cs ->
              return (c:cs)

-- A complete Haskell program defines a single big I/O action, called main, of type IO (). The program is executed by performing the action. Here, for example, is a program that reads a complete line from the input, reverses it, and prints it on the output:
main :: IO ()
main = getLine >>= \ cs -> putLine (reverse cs)

putLine :: [Char] -> IO ()
putLine [] = putChar '\n'
putLine (c:cs) = putChar c >> putLine cs
```

The only operation that combines/composes IO actions is (>>=), and it treats the world in a single-threaded way. That is, it takes the world produced from the first action and passes it on to the second action. The world is never duplicated or thrown away, no matter what code the programmer writes. It is this property that allows us to implement getChar (and other IO primitives) by performing the operation right away - a sort of "update in place".

There is a huge number of possible IO "primitives", such as putChar and getChar; some ops can be defined in terms of the existing ones (e.g. getLine) but many cannot. We need a way to call arbitrary IO libs supplied by the OS.


### do notation

Haskell provides a special syntax, "the do notation", for monadic computations. Using the do notation we can write getTwoChars as follows:

```hs
getTwoChars :: IO (Char,Char)
getTwoChars = do
    c1 <- getChar
    c2 <- getChar
    return (c1,c2)
```

You can leave out the `c <-` part when you want to throw away the result of the action:

```hs
putTwoChars :: (Char,Char) -> IO ()
putTwoChars (c1,c2) = do
    putChar c1
    putChar c2
```

The syntax is much more convenient than using (>>=) and lambdas, so in practice everyone uses do notation for I/O-intensive programs in Haskell.

The compiler desugars the do notation into calls to (>>=):

```hs
do { x <- e; s } = e >>= \x-> do { s }
do { e; s } = e >> do { s }
do { e } = e
```

It follows from this translation that the do statement `x <- e` binds the variable `x`. It does not assign to the location `x`, as would be the case in an imperative program. If we use the same variable name twice on the left hand side, we bind two distinct variables. For example:

```hs
do
    c <- getChar -- c :: Char
    c <- putChar -- c :: ()
    return c
```

The first line binds `c` to the character returned by `getChar`. The second line feeds that `c` to `putChar` and binds a distinct `c` to the value returned by `putChar`, namely `()`. This example also demonstrates that the scope of `x` bound by `x <- e` does not include `e`.

A do expression can appear anywhere that an expression can. Here, for example, is getLine in do notation; it uses a nested do expression:

```hs
getLine :: IO [Char]
getLine = do
    c <- getChar
    if c == '\n'
    then return []
    else do
        cs <- getLine
        return (c:cs)
```



## Control structures

We can build control structures (for, while, etc.) out of functions.

The `forever` combinator can express an infinite loop. So, `forever a` is an action that repeats `a` forever. If we want to repeat a given action a specified number of times, we can define it: `repeatN n a` is an action that, when performed, will repeat `a` n times.

Notice that forever and repeatN, like (>>) and (>>=), take an action as one of their arguments. *It is this ability to treat an action as a first class value that allows us to define our own control structures.*

The for loop - the idea is that `for ns fa` will apply the function `fa` to each element of `ns` in turn, in each case giving an action; these actions are then combined in sequence.

```hs
forever :: IO () -> IO ()
forever a = a >> forever a

repeatN :: Int -> IO a -> IO ()
repeatN 0 a = return ()
repeatN n a = a >> repeatN (n - 1) a

for :: [a] -> (a -> IO ()) -> IO ()
for []     _ = return ()
for (x:xs) f = f x >> for xs f

-- Another way to define for:
for xs f = sequence_ (map f xs)
```


Here, map applies `fa` to each element of `xs`, giving a list of actions
then `sequence_` combines these actions together in sequence. The `_` in `sequence_` reminds us that it throws away the results of the subactions, returning only `()`.

```hs
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ :: [IO a] -> IO ()
sequence_ xs = foldr (>>) (return ()) xs
```

Another fn, `sequence`, takes a list of actions, each returning a result of type `a`, and glues them together into a single compound action returning a result of type `[a]`. It is defined as:

```hs
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence :: [IO a] -> IO [a]
sequence []     = return []
sequence (x:xs) = do
    r  <- x
    rs <- sequence xs
    return (r:rs)
```

Notice what is happening: instead of having a fixed collection of control structures provided by the lang, we can invent new ones, as the need arises.


## References

The IO operations so far allow us to write programs that do IO in strictly sequentialised, imperative fashion. But we can also model **mutable variables**:

```hs
import GHC.IORef

data IORef a -- An abstract type

newIORef   :: a -> IO (IORef a)
readIORef  :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
```

A value of type `IORef a` is a reference to a *mutable cell* holding a value of type `a`. A new cell can be allocated using newIORef, supplying an initial value. Cells can be read and written using readIORef and writeIORef.

Here is a small loop to compute the sum of the values between 1 and n in an imperative style:

```hs
count :: Int -> IO Int
count n = do
    r <- newIORef 0
    loop r 1
    where
        loop :: IORef Int -> Int -> IO Int
        loop r i | i > n = readIORef r
                 | otherwise = do
                    v <- readIORef r
                    writeIORef r (v+i)
                    loop r (i+1)
```










unsafePerformIO

We have seen the IO monad as an abstract data type: that is, a type together with a collection of operations over that type. A key feature of an abstract data type is what it prevents as well as what it permits. In particular, notice the following:
* All the operations except one, (>>=), have an IO action
  as their result, but do not take one as an arg.
* The only operation that combines IO actions is (>>=)
* The IO monad is "sticky": no operation takes args with
  an IO type and returns a result with a non-IO type.


Sometimes, however, such restrictions are too limiting. For example, suppose you wanted to read a config file, using code something like this:

    configFileContents :: [String]
    configFileContents = lines (readFile "config")  -- WRONG!

    useOptimisation :: Bool
    useOptimisation = "optimise" `elem` configFileContents

lines :: String -> [String]
elem :: Eq a => a -> [a] -> Bool

The code is not type correct, because readFile has type:
readFile :: FilePath -> IO String

So readFile produces an IO String, while lines consumes a String.
We can "fix" this by giving
- configFileContents the type IO String, and
- useOptimisation the type IO Bool, plus some changes to the code.

But that means we can only test useOptimisation when we are in the IO monad (also being careful not to read the file every time we tested the boolean!), which would be very inconvenient. What we want is a way to get from IO String to String, but that is the very thing we cannot do in the IO monad.

There is a good reason for this:
reading a file is an IO action, so in principle it matters when we read the file, relative to all the other IO ops in program. But in this case, we are confident that the file config will not change during the program run, so it doesn't matter when we read it.

This sort of thing happens often enough that all Haskell implementations offer one more, unsafe, IO primitive:

    unsafePerformIO :: IO a -> a

then we can write:

    configFileContents :: [String]
    configFileContents = lines (unsafePerformIO (readFile "config"))

This combinator has a deliberately long name - whenever you use it, YOU ARE PROMISING THE COMPILER THAT THE TIMING OF YOUR IO OPERATION, RELATIVE TO ALL THE OTHER IO OPS OF THE PROGRAM, DOES NOT MATTER. You must undertake this proof obligation, because the compiler cannot do it for you, hence "unsafe" prefix.

We have to invent a World out of thin air, then discard it afterwards.

unsafePerformIO is best regarded as a tool for systems programmers and library writers, rather than for casual programmers. The IO it encapsulates can happen at unpredictable moments, or even not at all. What is less obvious is that you can also use it to defeat the Haskell type system, by writing a function:

    cast :: a -> b

unsafePerformIO is often mis-used to force an imperative program into a purely-functional setting, though it's the wrong tool for the job. Such apps can invariably be restructured into a cleaner, functional form. Nevertheless, when the proof obligations are satisfied, unsafePerformIO can be extremely useful.

In practice, unsafePerformIO has 3 very common patterns of usage:

- Performing once-per-run input/output, like configFileContents

- Allocating a global mutable variable, e.g.
        noOfOpenFiles :: IORef Int
        noOfOpenFiles = unsafePerformIO (newIORef 0)

- Emitting trace messages for debugging purposes
        trace :: String -> a -> a
        trace s x = unsafePerformIO (putStrLn s >> return x)


Summary
=======
Let us summarise what we have learned so far:
- A complete Haskell program is a single IO action called 'main'
- Big IO actions are built by gluing together smaller ones with bind and return
- An IO action is a first-class value. It can be passed to a function as an arg, or returned as the result of a function call, like (>>). It can be stored in a data structure (e.g. the arg to 'sequence')
- The fact that IO actions can be passed around makes it easy to define new "glue" combinators in terms of existing ones.


Monads
======
In general, a monad is a triple of a type constructor M, and two functions, `return` and (>>=), with types:

    return :: ∀α. α → M α
    (>>=)  :: ∀αβ. M α → (α → M β) → M β

These three must satisfy the following algebraic laws:

    return x >>= f = f x                                        (LUNIT)

    m >>= return = m                                            (RUNIT)

                  x does not appear free in m3
    ---------------------------------------------------------   (BIND)
    m1 >>= (λx.m2 >>= (λy.m3)) = (m1 >>= (λx.m2)) >>= (λy.m3)

-}
