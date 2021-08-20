# Approaches to IO

I/O is a first-class citizen in Haskell. We explore 3 styles of I/O programming, starting with the most naïve style: *imperative style*. From there, we move on to the elegant and concise *lazy I/O*, only to run into its severe limitations. The way out is the third and last style we explore, *iteratee I/O*. 

Topics:
- I/O as a first-class citizen
- Imperative I/O
- Lazy I/O
- The problem with Lazy I/O
- Resource management with bracket
- Iteratee I/O

## I/O as a first-class citizen

The IO Monad provides the context in which side effects may occur, and it also allows us to decouple pure code from I/O code. In this way, side effects are isolated and made explicit.

```hs
import System.IO
import Control.Monad
import Control.Applicative

main = do
  h <- openFile "jabberwocky.txt" ReadMode
  l <- hGetLine h
  putStrLn . show . words $ l
  hClose h
```

This code looks imperative in style: it seems as if we are assigning values to `h` and `l`, reading from a file, and then leaving side effects with the `putStrLn` function. The `openFile` and `hGetLine` functions are I/O actions that return a file handle and string, respectively. The `hClose` and `putStrLn` functions are I/O actions that return nothing in particular. In the `putStrLn . show` function, we compose a function that returns an I/O action with a pure function.

From this, we can see that functions can return I/O actions; functions can take I/O actions as arguments. We can compose regular functions with functions that return I/O actions. This is why it is said that I/O is a first-class citizen of Haskell.

### I/O common class instances

I/O is a Functor, Applicative and Monad. The I/O Monad is also an Applicative Functor, which in turn is a Functor. I/O as Functor enables us to map the pure `show . words` fn to the result of the `hGetLineh` action, ie `IO String` type.

```hs
main = do
  h <- openFile "jabberwocky.txt" ReadMode
  l <- fmap (show . words) (hGetLine h)
  putStrLn l
  hClose h
```

When we treat I/O as an Applicative Functor we can use `<*>`

```hs
main = do
  h <- openFile "jabberwocky.txt" ReadMode
  l <- (show . words) <$> (hGetLine h)
  putStrLn l
  hClose h
```

For the monadic version of the preceding code, we use the `liftM` function:

```hs
main = do
  h <- openFile "jabberwocky.txt" ReadMode
  l <- liftM (show . words) (hGetLine h)
  putStrLn l
  hClose h
```

All 3 styles of the `fmap` function are equivalent in this case. However, in general, Monad is more powerful than Applicative, and Applicative is more powerful than Functor.


## Imperative I/O

*Imperative I/O* or *handle-based I/O* 

It has some advantages:
- Processing is incremental (for example, the processing of a file).
- We have precise control over resources (for example, when files are opened or closed and when long-running processes are started).

But also disadvantages:
- I/O is expressed at a relatively low level of abstraction.
- This style of code is not composable
- The traversal state is exposed: we need to pass the file handle around and check for EOF at each iteration. We need to explicitly clean up the resource.

## Lazy I/O

Of the 3 main glues of Haskell (HOFs, the type system, and laziness), laziness is different in that it is not a concrete thing in the language, but is instead related to the way the code will be evaluated in the runtime. Laziness is something that we have to know about rather than something we can always see in the code.

When performing lazy I/O, we need to make the distinction between an I/O action and performing an I/O action. Also, we need to know the lazy/strict characteristics of the functions we are working with.

In the preceding code, we composed pure functional streams with I/O streams using monadic operators and functions. This is the lazy I/O, the pure functional way for composable I/O. In the same way that we can often express functions as pipelines of simpler functions, the same is true for I/O. Many practical I/O can be modeled as processing pipelines of streams.

The advantages of Lazy I/O:
- I/O is expressed at a relatively high level of abstraction
- It is very composable, enabling the decoupling of producers from consumers

The disadvantages:
- Poor control over when something is evaluated
- Poor control of resources

Lazy evaluation is elegant and has far reaching implications. It is an integral part of functional programming that unfortunately does not translate to performing I/O.

### The problems with lazy I/O

The order of the side effects is tied to the order of the lazy evaluation. Because the order of lazy evaluation is not explicit, the order of effects also isn't. The sequence of side-effects can become hard to predict. It can be difficult to reason about the space requirements of a lazy program.

Poor resource management and lack of explicit order of effects can make it difficult to know when to clean up resources. Since *the demand drives lazy evaluation, which drives effects*, we inherently have little opportunity to "intercept" evaluation for resource management purposes. Also, resource management is made more difficult by the possibility of errors.

Despite this, lazy I/O remains an attractive option in simple situations, where space requirements and order of execution are sufficiently predictable and where resource management is easy enough. However, when there is strong demand for precise resource management or predictable space usage, lazy I/O is not an option; for example, writing networking code, handling many files, or handling many HTTP requests in a web server, and so on.

"Extensive experience in Haskell has, however, exposed severe drawbacks of lazy evaluation, which are especially grievous for stream processing of large amounts of data. *Lazy evaluation is fundamentally incompatible with computational effects*, can cause fatal memory leaks, and greatly inhibits modular reasoning, especially about termination and space consumption. Seemingly innocuous and justified changes to the code or code compositions may lead to divergence, or explosion in memory consumption."
-- `Lazy v. Yield: Incremental, Linear Pretty-printing` by Kiselyov et al

### Resource management with bracket

The bracket function relies on higher order functions to express a specific kind of wrapper pattern, viz. *"acquire and release"*.

https://wiki.haskell.org/Bracket_pattern

The finally function is a special form of bracket:

```hs
finally :: IO a     -- some action
        -> IO b  -- final action: runs afterwards
        -> IO b  -- result
```

The bracket family of functions helps us clean up resources more reliably, but by no means definitively solves the problem of closing resources in a timely manner. If we need more precise resource management than this (and more predictable space requirements and ordering of effects), then we must use a more sophisticated pattern for stream programming called Iteratee I/O.


## Iteratee I/O

In the late 2000s, Kiselyov championed a new way of doing I/O that combines the best of Handle-based I/O (precise control over resources and predictable space requirements) with the best of lazy I/O (decoupling of producers and consumers, high level of abstraction).

This example was inspired by
- http://www.scs.stanford.edu/11au-cs240h/notes/iteratee.html
- https://themonadreader.files.wordpress.com/2010/05/issue16.pdf
  `Iteratee: teaching an old fold new tricks`.
