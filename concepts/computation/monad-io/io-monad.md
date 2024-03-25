# IO monad

## Input/Output

Many PLs make a distinction between expressions and commands. Expressions evaluate to values and commands "evaluate" to (side) effectful actions. Haskell also makes this distinction. In Haskell, everything is an (pure) expression, so the question arises how to deal with things that are simply not expressions, but require side effects. The IO monad is a solution to the problem of how to remain pure and still have side effects (effectful computation).

Loosely put, we write Haskell using pure functions, for as long as possible. The effectful computations are not really pure functions (although they techically are) and they are wrapped in the IO monad. All impurity is contained there, within the `main`, which is the one that is first exceuted when a program is compiled and loaded. Techically, even the `main` function is pure - what really happens is that the Haskell's RTS (runtime system) executes `main` and deals with side effects, permitting Haskell, the language, to remain pure.

In general, if an action returns a value of type `a` then the command itself is of type `IO a`; it has an IO wrapped around the return type. If an action does not return anything useful, i.e. if it's called for its side effect, then the return value is unit, i.e. `IO ()`. This is the type of the `main` function.

We can call and combine functions with the IO type, but we can never escape it: e.g. we cannot extract a user typed string at the prompt and send it off to a pure function. Once we're inside `IO` we stay there. So, the usual approach is to push all the impurity into `main` (and auxillary IO functions) and keep the rest (as mush as possible) of the code pure.

If something is of type `IO`, then it's an action. Otherwise it's an expression. This makes it impossible to put an action inside an expression.

We cannot sneak an action (command) into an expression because in order to make an expression out of a command we'd have to extract a return value out of a command. But to do that we must use the `<-` ("slurp") operator. But we can only use `<-` inside a IO type, in a `do` block (more generally, only by using `>>=`). Thus, the result must to be a command. Once we are in a command, there is no escaping it.

Most that is said above is false, but it makes a start to garner intuition. In fact, Haskell only has expressions, which are used consistently to denote a wide variety of immutable values - from Booleans, streams, trees, animation, to (stateful) computations (aka actions, commands, IO values), and beyond.

Everything is denoted as a pure expressions with immutable data. The only essential difference between IO and, e.g. a list, is that the semantic interpretation associated with the `IO` is much more complicated (intractably so) than the one for lists. And "monadness" is no more essential to the `IO` type than it is to the list type. Monads are just a convenient structuring technique.




## Refs

* The IO Monad for People who Simply Don't Care, 2007
http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html

* What is IO monad? - Tsoding
https://www.youtube.com/watch?v=fCoQb-zqYDI

* Monadic i/o and UNIX shell programming
https://okmij.org/ftp/Computation/monadic-shell.html

* Strong monads
https://ncatlab.org/nlab/show/strong+monad

* Strong monads
http://blog.sigfpe.com/2023/08/

* Probability monad
http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/02/21/refactoring-probability-distributions/

* A Probabilistic Functional Programming Library for Haskell
https://web.engr.oregonstate.edu/~erwig/pfp/
