# MVar

- module: `Control.Concurrent.MVar`
- package: `base-4.15.0.0`
- docs: https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Concurrent-MVar.html


An `MVar t` is mutable location that is either empty or contains a value of type `t`.

It has 2 fundamental operations:
- `putMVar` which fills an MVar if it is empty and blocks otherwise
- `takeMVar` which empties an MVar if it is full and blocks otherwise

They can be used as
- synchronized mutable variables
- channels, with `takeMVar` as *receive*, `putMVar` as *send*
- binary semaphore `MVar ()`, with `takeMVar` as *wait*, `putMVar` as *signal*

They were introduced in the paper *"Concurrent Haskell"* by Simon Peyton Jones, Andrew Gordon and Sigbjorn Finne, though some details of their implementation have since then changed (in particular, a `put` on a full `MVar` used to error, but now merely blocks).

## Applicability

MVar offers more flexibility than `IORef`, but less flexibility than `STM`.

MVar is appropriate for building synchronization primitives and performing simple inter-thread communication; however they are very simple and susceptible to race conditions, deadlocks or uncaught exceptions. Do not use them if you need to perform larger atomic operations such as reading from multiple variables (use STM instead).

In particular, the "bigger" functions in this module (`swapMVar`, `withMVar`, `modifyMVar_` and `modifyMVar`) are simply the composition of a takeMVar followed by a putMVar with exception safety. These only have atomicity guarantees if all other threads perform a takeMVar before a putMVar as well; otherwise, they may block.

## Fairness

No thread can be blocked indefinitely on an `MVar` unless another thread holds that `MVar` indefinitely. One usual implementation of this fairness guarantee is that threads blocked on an `MVar` are served in a FIFO - but this is not guaranteed in the semantics.

## Issues

Like many other Haskell data structures, `MVars` are lazy. This means that if you place an expensive unevaluated thunk inside an `MVar`, it will be evaluated by the thread that consumes it, not the thread that produced it. Be sure to `evaluate` values to be placed in an `MVar` to the appropriate normal form, or utilize a strict `MVar` provided by the `strict-concurrency` package.

## Ordering

`MVar` operations are always observed to take place in the order they are written in the program, regardless of the memory model of the underlying machine. This is in contrast to `IORef` operations which may appear out-of-order to another thread in some cases.

## Example

<details>
<summary>MVar example</summary>

Consider the following concurrent data structure, a *skip channel*. This is a channel for an intermittent source of high bandwidth information (e.g. mouse movement events).

Writing to the channel never blocks, and reading from the channel only returns the most recent value, or blocks if there are no new values.

Multiple readers are supported with a `dupSkipChan` operation.

A skip channel, `SkipChan a`, is a pair of `MVars`:

- The first `MVar` contains the current value, `a` , and  a list of semaphores, `[MVar ()]`, that need to be notified when it changes.

- The second `MVar` is a semaphore, `MVar ()`, for this particular reader; it is full if there is a value in the channel that this reader has not read yet, and empty otherwise.

`MVar ()` is used as a binary semaphore:
- `takeMVar` as *wait*
- `putMVar`  as *signal*


```hs
data SkipChan a = SkipChan
    (MVar (a, [MVar ()]))
    (MVar ())

newSkipChan :: IO (SkipChan a)
newSkipChan = do
    sem  <- newEmptyMVar
    main <- newMVar (undefined, [sem])
    return (SkipChan main sem)

putSkipChan :: SkipChan a -> a -> IO ()
putSkipChan (SkipChan main _) v = do
    (_, sems) <- takeMVar main
    putMVar main (v, [])
    mapM_ (\sem -> putMVar sem ()) sems

getSkipChan :: SkipChan a -> IO a
getSkipChan (SkipChan main sem) = do
    takeMVar sem
    (v, sems) <- takeMVar main
    putMVar main (v, sem:sems)
    return v

dupSkipChan :: SkipChan a -> IO (SkipChan a)
dupSkipChan (SkipChan main _) = do
    sem <- newEmptyMVar
    (v, sems) <- takeMVar main
    putMVar main (v, sem:sems)
    return (SkipChan main sem)
```

(This example was adapted from the original 'Concurrent Haskell' paper. For more examples of MVars being used to build higher-level synchronization primitives, see `Chan` and `QSem`)


</details>
