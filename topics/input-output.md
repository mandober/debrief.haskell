# Input-output

The `getLine` function is pure as it will return the exact same *representation of a computation* every time. There is a huge difference between a data structure representing a computation and a computation itself. The `print` is an IO instruction that prints out its arg when executed. `getLine` is a computation object that returns a string when executed. IO objects can be thought of as self-contained encapsulated packets of assembly. Haskell produces data structures that represent computer instructions. The compiler will compile the `main` into an executable binary format. Every function or declaration that makes up our program is completely pure and side-effectless. In fact, the assembly of main itself is side-effectless and pure. We assemble the `IO ()` that `main` returns in a pure way. Therefore, `main` is pure, as well. Every time we *evaluate* `main`, we get the exact same *computational data structure*.

The `main` will always evaluate to the exact same computational data structure. `main` will always be the exact same program, no matter when you run it (e.g. a program that gets a string from stdin and prints it).

The processor, which is given a binary representation of the IO data structure, and is completely separate from the language itself, now executes this program (compiled data structure). Its execution of this binary (program) is, of course, potentially unpredictable and in general non-deterministic, and can depend on external things. The instructions that it follows will be the same every time, but the result of those instructions will be different every time.

The `main` is a function that returns/evaluates deterministically to a data structure representing a computation. The computation that it represents is not necessarily deterministic. This distinction between evaluation and execution is what sets apart this I/O model that permits its purity. `main` is a pure value. The instruction data structure `main` represents are impure instructions. That is how we have I/O in Haskell while remaining pure.

The pure Haskell produces a computational object and passes it off to the impure execution environment. The evaluation is the pure process, and execution is the impure one.



## Input-output and purity

One of the appeals of pure FP is that it is so amenable to equational reasoning. Non-strict semantics supports algebraic manipulation through the principle of substituting equals for equals. On the face of it, purity seems very limiting as it rules out computational effects; but many programs need impure computational effects, such as mutable state and nondeterminism. However, Moggi and Wadler have famously showed how to get round this problem by using monads to encapsulate the effects, leading in essence to a phase distinction, a pure functional evaluation yielding an impure imperative computation. A pure program can assemble a representation of an effectful computation as a plain value, which can then be executed at the top level instead of being printed out.

Still, it has not been clear how to reconcile that phase distinction with the continuing appeal of FP: does the impure imperative part become inaccessible to equational reasoning? We think not; and to back that up, we present a simple axiomatic approach to reasoning about programs with computational effects.

## Background

The categorical notion of a monad precisely expresses an abstraction of sequential computations, which is necessary for controlling the consequences of computational effects in a lazy functional programming language.

The `return` models the identity and `bind` models *sequential composition of computations*. Both methods are part of the `Monad` type class.

```hs
class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
```

The two operations are required to satisfy **three laws**, corresponding to the monoidal properties of sequential composition:

```js
   return x >>= k == k x
    mx >>= return == mx
(mx >>= k) >>= k' == mx >>= (λx -> k x >>= k')
```

We make use of two important specializations of the operations associated with a monad, which more precisely match the idea of the *identity computation* and *sequential composition*, `skip` and `>>`.

```hs
skip :: Monad m => m ()
skip = return ()

(>>) :: Monad m => m a -> m b -> m b
mx >> my = mx >>= const my

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= return . f
```

We also make significant use of the derived operator `liftM`. The unit and associativity properties imply that `liftM` satisfies the map laws of a functor.

```js
     liftM id == id
liftM (f . g) == liftM f . liftM g
```

An uncurried version of Haskell's `liftM2` can be defined as `liftM f . pair`, where `pair` sequences a pair of monadic computations:

```hs
pair :: Monad m => (m a, m a) -> m (a, a)
pair (mx, my) = mx >>= \x -> my >>= \y -> return (x, y)

-- or
pair :: Monad m => (m a, m a) -> m (a, a)
pair (mx, my) = do
  x <- mx
  y <- my
  return (x,y)
```
