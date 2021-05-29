# Input-output

## Effects in Pure Functional Languages

Moggi showed how *notions of computation* such as mutable state, exceptions, nondeterminism, and probability can be elegantly encapsulated as monads, and safely embedded within an otherwise pure functional language. It may seem that purity rules out interesting computational effects, such as update, exception handling, and choice. After all, if `coin` denotes the computation modelling a fair coin toss, a 50-50 choice between heads and tails, then two occurrences of 'coin' must certainly denote possibly different outcomes, thereby destroying referential transparency? This apparent problem is eliminated by distinguishing between types that represent values, such as 'True' or 'heads', and those that represent computations, such as 'coin'.

Two occurrences of `coin` *denote the same computation*, and it is the executions of these computations that may yield different outcomes.

Each class of effects, such as probabilistic choice, determines a notion of computation, in this case of probability distributions. So `coin` denotes not a single outcome, but a **distribution of outcomes**.

The operations and axioms of a notion of computation can be precisely and elegantly abstracted via the categorical notion of a *monad*. Equivalently, the operations and axioms can be captured as an algebraic theory, and equational reasoning can be safely conducted within such a theory.

With the monadic approach to computational effects, purely functional expressions are classified into two kinds:
- expressions that denote values (integer, string)
- expressions that denote computations with possible effects

However, both are represented as pure data - the computations are represented as pure terms in a certain abstract syntax, rather than some kind of impure action.

When the run-time system of a language encounters the first kind of expression, it evaluates it and prints it out; when it encounters the second kind, it evaluates it, interprets the term as the effectful computation it encodes, and executes that computation. Consequently, evaluation remains pure, and any impurities are quarantined within the run-time system.


There is a general framework consisting of just two operators, which in a sense model the compositional structure of computations; then for each class of effects, there is an extension to the general framework to model the primitives specific to that class.

In fact, the general framework and a specific extension together represent the *free term algebra* for the signature corresponding to the primitives for a particular class of effects. It is no coincidence that monads turn out to be useful for modelling such term algebras, because they were developed precisely as a categorical expression of *universal algebra*.




## Input-output

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
