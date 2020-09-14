# Parallel and Concurrent Programming in Haskell
Simon Marlow, O'Reilly, 2013


Haskell uses a lazy evaluation strategy - defining an expression causes a thunk to be built to represent it. The thunk remains unevaluated until the value is required. Once evaluated, the thunk is replaced by the value, so the value is only ever evaluated once. The seq function evaluates its argument only as far as the first ctor, and doesn't evaluate any more of the structure. We say that `seq` evaluates to *Weak Head Normal Form* (WHNF). *Normal form* means fully evaluated.

The map function builds a lazy list; this might be clearer if we rewrite its definition to make the thunks explicit:

```hs
map f (x:xs) = f x : map f xs

-- with explicit thunks:
map f (x:xs) = let x' = f x
                   xs' = map f xs
               in  x' : xs'
```

## Parallelism and concurrency

Basic functionality for creating parallelism is provided by the *Control.Parallel.Strategies* module.

```hs
data Eval a
instance Monad Eval
runEval :: Eval a -> a
rpar :: a -> Eval a
rseq :: a -> Eval a
```

Parallelism is expressed using the **Eval monad**, which comes with two operations, `rpar` for parallel, and `rseq` for sequantial evaluation.
* `rpar` combinator enables parallelism by making its arg available for parallel evaluation.
* `rseq` is used for forcing sequential evaluation because first its arg gets evaluated, then we wait for the result.

In both cases, evaluation is to weak head normal form (WHNF). The arg to `rpar` should be an unevaluated computation, i.e. a *thunk*. For if the arg was already evaluated, nothing useful would happen (there would be no work to perform in parallel).

The Eval monad provides a `runEval` function that performs the Eval computation and returns its result. `runEval` is completely pure, *there's no need to be in the IO monad here*.
