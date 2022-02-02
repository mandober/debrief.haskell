---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Monads_as_computation
page-title:       Monads as computation - HaskellWiki
article-title:    Monads as computation - HaskellWiki
---
# Monads as computation - HaskellWiki

Programmers in general, and functional programmers in particular, are usually not so content to solve a problem in a fragile way by coding a solution directly. Quite often the best way to solve a problem is to design a domain-specific language in which the solution to one's problem is easily expressed. Doing this generally ensures that a wide class of similar problems can be attacked using the same code. That way, you get code which is resistant to damage in the form of changes to design requirements.
### Motivation

Programmers in general, and functional programmers in particular, are usually not so content to solve a problem in a fragile way by coding a solution directly. Quite often the best way to solve a problem is to design a domain-specific language in which the solution to one's problem is easily expressed. Doing this generally ensures that a wide class of similar problems can be attacked using the same code. That way, you get code which is resistant to damage in the form of changes to design requirements.

Better still, we'd like to embed those domain specific languages into the language which we wrote them in, so that they can be used together, and so we get benefits from the language we're working in without so much extra work. So we write combinator libraries which are essentially libraries of code whose API's are sufficiently powerful that using the library is like programming in a small language embedded within the existing one.

Such a library will have some representation of primitive computations, and some ways to glue those computations together into more complex ones. A parsing library might define primitive parsers for parsing single characters, and then combining functions for concatenating parsers or selecting between them. A drawing library might define some basic drawing operations, and then various means of combining drawings into larger ones (on top, beside, above, etc.).

In this manner, the user of the combinator library builds up the computation they want, piecing together smaller parts into larger ones.

As far as programming is concerned, a [monad][1] is just a particular style of combinator library. That is, one which supports a few basic means of combination.

The reason for making this abstraction is so that all the libraries which make use of those means of combination can then share a library of combining functions built up from the primitive ones they are required to support.

Specifically, by defining an instance of Monad for your library when appropriate, you automatically get the benefit of the functions in the [Control.Monad library][2] (as well as a few others, like [Data.Traversable][3]). This includes things like for-each loops (forM/mapM), ways to turn pure functions into combiners (liftM2, etc.), as well as other control structures which you get for free just for making your library an instance of Monad.

### The parts of a monad

There are of course, other kinds of combinator library, but monads arise fairly naturally from a few basic premises.

-   Monadic computations have results. This is reflected in the types. Given a monad M, a value of type `M t` is a computation resulting in a value of type `t`. It's important to realise that this is typically just some data structure. It will be interpreted as a computation and "run" in a way which is dependent on the given library.
-   For any value, there is a computation which "does nothing", and produces that result. This is given by defining the function `return` for the given monad.
    
    return :: (Monad m) \=> a \-> m a
    
-   Given a pair of computations `x` and `y`, one can form the computation `x >> y`, which intuitively "runs" the computation `x`, throws away its result, then runs `y` returning its result.
    
    (\>>) :: (Monad m) \=> m a \-> m b \-> m b
    
-   Further, we're allowed to use the result of the first computation to decide "what to do next", rather than just throwing it away. This idea is embodied by the operation `(>>=)`, called 'bind'. If `x` is a computation, and `f` is a function from potential results of that computation to further computations to be performed, then `x >>= f` is a computation which runs `x`, then applies `f` to its result, getting a computation which it then runs. The result of this latter computation is the result of the combined one.
    
    (\>>=) :: (Monad m) \=> m a \-> (a \-> m b) \-> m b
    

In fact, once we have bind, we can always define `(>>)` as:

It's important to realise that both what it means to "run" a computation, and what "then" means in the above are both *up to the monad in question* (subject to a few simple constraints to be discussed later). This point will become clearer as one sees more and more examples.

### A few examples

On top of `return` and `(>>=)`, any given monad will typically define a bunch of primitive computations to get the user of the library started. The `IO` monad, for instance, has a large number of I/O operations such as `getLine :: IO String` and `putStrLn :: String -> IO ()`. The program:

main :: IO ()
main \= getLine \>>= putStrLn

gets a line of text from the user, and then prints it back out. For a slightly more complicated example, the program:

main :: IO ()
main \= putStrLn "Enter a line of text:"
         \>> getLine \>>= \\x \-> putStrLn (reverse x)

prompts the user for a line of text, gets the line of text from the user, and then prints it back out in reverse. A parsing monad might define `char :: Char -> Parser Char`, for constructing a parser which succeeds if the input string matches the given character. As a very simple example without getting into the details of parsing monads, the parser:

cat \= char 'c' \>> char 'a' \>> char 't' \>> return "It's a cat."

would try to match the string "cat", and if the parse succeeded, would return the string `"It's a cat."`.

### Do notation

Because computations are typically going to be built up from long chains of `(>>)` and `(>>=)`, in Haskell, we have some syntax-sugar, called do-notation.

The do-notation allows us to write our second IO program above as:

main \= do putStrLn "Enter a line of text:"
          x <- getLine
          putStrLn (reverse x)

The basic mechanical translation for the do-notation is as follows:

do { x } \= x

do { x ; <stmts\> }
  \= x \>> do { <stmts\> }

do { v <- x ; <stmts\> }
  \= x \>>= \\v \-> do { <stmts\> }

do { let <decls\> ; <stmts\> }
  \= let <decls\> in do { <stmts\> }

This gives monadic computations a bit of an imperative feel, but it's important to remember that the monad in question gets to decide what the combination means, and so some unusual forms of control flow might actually occur. In some monads (like parsers, or the list monad), "backtracking" may occur, and in others, even more exotic forms of control might show up (for instance, first-class continuations, or some form of parallelism).

### The monad laws

However, in order to maintain some semblance of sanity, we agree to make the monads we define follow some basic rules. I'll show the three rules both in terms of `return` and `(>>=)` and do-notation, and try to give some feel of what they really mean.

1. return v \>>= f \= f v

2. x \>>= return \= x

3. (x \>>= f) \>>= g \= x \>>= (\\v \-> f v \>>= g)

Rules 1 and 2 basically give one the sense that `return v` "does nothing" and results in `v`.

Rule 3 puts a bit of a constraint on what "then" is supposed to mean. It is perhaps easier at first to look at what it means for `(>>)`:

(x \>> y) \>> z \= x \>> (y \>> z)

This corresponds nicely with our usual reading of `(>>)` as "then":

putting on your tie, **then** (putting on your socks **then** putting on your shoes)

is the same thing as

(putting on your tie **then** putting on your socks) **then** putting on your shoes.

To get a bit of a different perspective on what the laws mean, let's see what they look like in do-notation:

1. do { w <- return v; f w }
 \= do { f v }

2. do { v <- x; return v }
 \= do { x }

These two are again consistent with the idea that return produces a computation that has no "side-effects", and just returns its parameter.

3. do w <- do v <- x
              f v
      g w

 \= do v <- x
      w <- f v
      g w

This is more interesting. It's telling us that asking for the result of a compound computation in the midst of a do-block will result in exactly the same thing as if that compound computation had been spliced in directly, and gives us a valid way to refactor code written in any monad. We're allowed to abstract a chunk of code out from the middle of a do-block and give it a name without worrying about whether we've changed the meaning of the code.

### The whole point

This is all very good, but apart from defining a pretty syntax for a certain kind of combinator library, the stuff we've done so far is fairly inessential. What's the point of recognising something as a monad?

The point, as I alluded to in the introduction, is that we can then write code which works for all monads, and have a whole library of code which is made available to us just for recognising that the library we're writing happens to be a monad. Since we have only return and bind to work with, this sort of code will serve to chain computations together in some methodical way. That is to say, it will consist of control structures.

All the examples I'll give are already defined in the [Control.Monad][4] library, along with many more.

The first example of such a control structure we'll look at is called `sequence`. It's a function which takes a list of computations of the same type, and builds from them a computation which will run each in turn and produce a list of the results:

sequence :: (Monad m) \=> \[m a\] \-> m \[a\]
sequence \[\]     \= return \[\]
sequence (x:xs) \= do v <- x
                     vs <- sequence xs
                     return (v:vs)

or, without the do-notation:

sequence :: (Monad m) \=> \[m a\] \-> m \[a\]
sequence \[\]     \= return \[\]
sequence (x:xs) \= x \>>= \\v \-> sequence xs \>>= \\vs \-> return (v:vs)

(one can start to see why do-notation might be desirable!)

In a parsing monad, we might pass it a list of parsers, and get back a parser which parses its input using each in turn. In the IO monad, a simple example might be the following:

main \= sequence \[getLine, getLine\] \>>= print

which gets two lines of text from the user, and then prints the list.

Since lists are lazy in Haskell, this gives us a sort of primordial loop from which most other kinds of loops can be built.

What is a for-each loop really? It's something which performs some action based on each element of a list. So we might imagine a function with the type:

forM :: (Monad m) \=> \[a\] \-> (a \-> m b) \-> m \[b\]

(as an added bonus, we'll have it collect the results of each iteration).

We can write this with sequence and map:

forM xs f \= sequence (map f xs)

we apply the function to each element of the list to construct the action for that iteration, and then sequence the actions together into a single computation.

For example:

main \= forM \[1..10\] $ \\x \-> do
          putStr "Looping: "
          print x

Since in this, and many other cases, the loop body doesn't produce a particularly interesting result, there are variants of `sequence` and `forM` called `sequence_` and `forM_`, which simply throw the results away as they run each of the actions.

sequence\_ :: (Monad m) \=> \[m a\] \-> m ()
sequence\_ \[\]     \= return ()
sequence\_ (x:xs) \= x \>> sequence\_ xs

forM\_ :: (Monad m) \=> \[a\] \-> (a \-> m b) \-> m ()
forM\_ xs f \= sequence\_ (map f xs)

Sometimes we only want a computation to happen when a given condition is true. For this, we can write the following:

when :: (Monad m) \=> Bool \-> m () \-> m ()
when p x \= if p then x else return ()

Remember that `return ()` is a no-op, so running this computation will run x when the condition is true, and will do nothing at all when the condition fails.

Another extremely common thing to do is to construct a computation which performs another computation and then applies a function to the result. This can be accomplished by using the `liftM` function:

liftM :: (Monad m) \=> (a \-> b) \-> m a \-> m b
liftM f x \= do v <- x
               return (f v)

Or:

liftM :: (Monad m) \=> (a \-> b) \-> m a \-> m b
liftM f x \= return . f \=<< x

where `(=<<)` is just bind with its parameters flipped.

This is also generalised by `liftM2, liftM3, ...` to running more than one computation before applying a function to the results:

liftM2 :: (Monad m) \=> (a \-> b \-> c) \-> m a \-> m b \-> m c
liftM2 f x y \= do v <- x
                  w <- y
                  return (f v w)

It's possible to rewrite sequence in terms of liftM2, return, and a fold over the list:

sequence :: (Monad m) \=> \[m a\] \-> m \[a\]
sequence xs \= foldr (liftM2 (:)) (return \[\]) xs

sequence\_ :: (Monad m) \=> \[m a\] \-> m ()
sequence\_ xs \= foldr (\>>) (return ()) xs

Anyway, these are just a few of the simpler examples to give a taste of what sorts of control structures you get for free by defining a combinator library as a monad.

### Some final notes

It's a common misconception that Haskell uses a monad for I/O out of necessity. Really, it could use any sort of combinator library to describe and combine I/O actions. It just happens that the most obvious way to formulate a library to describe I/O actions ends up being a monad. So we define it as such so as to be able to share all these control structures with other monadic libraries.

That's really the only reason why we ever define anything as a monad -- the abstraction allows us to make use of a bunch of shared code for free without writing it out over and over again (or worse yet, failing to abstract it at all).

At this point, you might want to look at some more examples of monads. One place which is a decent starting point for that is Part II of the ["All About Monads" tutorial][5]. You might also have a look at the [Hierarchical Libraries Documentation][6] for the libraries under Control.Monad.

\-- [CaleGibbard][7]

## See also

-   [Monad][8]
-   [Monads as containers][9]

[1]: https://wiki.haskell.org/Monad "Monad"
[2]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html
[3]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Traversable.html
[4]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html
[5]: http://www.haskell.org/haskellwiki/All_About_Monads#Introduction_2
[6]: http://www.haskell.org/ghc/docs/latest/html/libraries/index.html
[7]: https://wiki.haskell.org/User:CaleGibbard "User:CaleGibbard"
[8]: https://wiki.haskell.org/Monad "Monad"
[9]: https://wiki.haskell.org/Monads_as_containers "Monads as containers"
