# STM

Software Transactional Memory (STM)

- https://www.youtube.com/watch?v=2lll2VbX8Vc&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=30
- https://gist.github.com/phagenlocher/353b24d969598198ca009e055eb2482d

"Composable Memory Transactions" paper
https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fstm%2Fstm.pdf

## The ST monad

Sometimes mutability is imposed by external demands. However, there are scenarios in which the requirement for mutable state is internal − that is, it is not reflected in any way in the overall results. For instance, sorting a list does not require mutability in any essential way, and so a function that sorts a list and returns a new list should, in principle, be functionally pure even if the sorting algorithm uses destructive updates to swap the position of the elements. In such case, the mutability is just an implementation detail.

The standard library provides a tool for handling such situations while still ending remaing pure: the `ST` monad from the `Control.Monad.ST` module.

```hs
data ST s a
```

`ST s a` looks a lot like `State s a`, and indeed they are similar in spirit. An ST computation uses internal state to produce results, except that the state is mutable. For that purpose, `Data.STRef` provides `STRef`. A `STRef s a` is exactly like an `IORef a`, but it lives in the `ST s` monad rather than in `IO`.

A major difference that sets apart `ST` from both `State` and `IO` is that `Control.Monad.ST` offers a `runST` function that lets us escape the monad. It has the following type:

```hs
runST :: forall a. (forall s. ST s a) -> a
```

At first glance, that should be a strange type signature. If ST involves mutability, how come we can simply extract a values from the monad? The answer lies in the `forall s` part of the type. Having a `forall s` enclosed within the type of an argument is like telling the type checker "`s` could be anything so don't make any assumptions about it". Not making any assumptions, however, means that `s` cannot be matched with anything else − even with the `s` from another invocation of `runST`. The `s` here is an example of an *existential type* - the only thing we know about it is that it exists.

```hs
GHCi> import Control.Monad.ST
GHCi> import Data.STRef

-- Attempt to keep an STRef around to pass to pure code:
GHCi> let ref = runST $ newSTRef (4 :: Int)

<interactive>
  Couldn't match type 'a' with 'STRef s Int'
    because type variable 's' would escape its scope
  This (rigid, skolem) type variable is bound by
    a type expected by the context: ST s a
  Expected type: ST s a
    Actual type: ST s (STRef s Int)
  Relevant bindings include ref :: a
  In the second argument of '($)', namely 'newSTRef (4 :: Int)'
  In the expression: runST $ newSTRef (4 :: Int)
-- The error message is quite clear:
-- "because type variable 's' would escape its scope"

-- Attempt to feed an STRef from one ST computation to another:
GHCi> let x = runST $ readSTRef =<< runST (newSTRef (4 :: Int))

<interactive>
  Couldn't match type 'STRef s1 Int' with 'ST s (STRef s a)'
  Expected type: ST s1 (ST s (STRef s a))
    Actual type: ST s1 (STRef s1 Int)
  Relevant bindings include x :: a
  In the first argument of 'runST', namely '(newSTRef (4 :: Int))'
  In the second argument of '(=<<)', namely
    'runST (newSTRef (4 :: Int))'
-- i.e. the 's' from each computation are necessarily not the same.
```

>The overall effect of the existential type is to insulate the internal state and mutability within each `ST` computation, so that from the point of view of anything else in the program `runST` is a pure function.

As a trivial example of `ST` in action, here is a very imperative-looking version of sum for lists:

```hs
import Control.Monad.ST
import Data.STRef
import Data.Foldable

sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  for_ xs \x -> modifySTRef n (+ x)
  readSTRef n
```

For all intents and purposes, `sumST` is no less pure than the familiar sum. The fact that it destructively updates its accumulator `n` is a mere implementation detail, and there is no way information about `n` could leak other than through the final result.

Looking at a simple example like this one makes it clear that the `s` type variable in `ST s a` does not correspond to anything in particular within the computation − it is just an *artificial marker*.

Another detail worth noting is that even though `for_` folds the list from the right, the sums are done from the left, as mutations are performed as applicative effects sequenced from left to right.

## Mutable data structures

Mutable data structures can be found in the libraries for the exceptional use cases for which they prove necessary. For instance, *mutable arrays* (alongside with immutable ones) can be found in the `vector` package or the `array` package bundled with GHC. There are also *mutable hash tables*, such as those from the `hashtables` package. In all cases mentioned, both ST and IO versions are provided.


## Refs

* The seventh chapter of *Write Yourself a Scheme in 48 Hours* provides an interesting example of IORefs being used to implement mutable variables in a language.
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Adding_Variables_and_Assignment

* Lennart Augustsson's blog shows how a true quicksort (that is, one using the original algorithm which performs destructive updates to sort the list) can be implemented in Haskell, just like we assured that was possible way back in Haskell/Higher-order functions. His implementation is quite amusing thanks to the combinators used to handle mutability, which make Haskell look like C. Be sure to check the two posts before the one linked to see how that was done.

Quicksort in Haskell
https://augustss.blogspot.com/2007/08/quicksort-in-haskell-quicksort-is.html
