# Internals and Performance

## GHC limits 

https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-8.10.1/src/Constants.html

```hs
-- All pretty arbitrary:

-- Should really match the number of decls in Data.Tuple
mAX_TUPLE_SIZE :: Int
mAX_TUPLE_SIZE = 62

-- Constraint tuples
-- Should match the number of decls in GHC.Classes
mAX_CTUPLE_SIZE :: Int
mAX_CTUPLE_SIZE = 62

mAX_SUM_SIZE :: Int
mAX_SUM_SIZE = 62

-- Default max depth for class instance search and type family reduction
-- See #5395.
mAX_REDUCTION_DEPTH :: Int
mAX_REDUCTION_DEPTH = 200

-- Default maximum constraint-solver iterations.
-- Typically there should be very few
mAX_SOLVER_ITERATIONS :: Int
mAX_SOLVER_ITERATIONS = 4

wORD64_SIZE :: Int
wORD64_SIZE = 8

-- Size of float in bytes.
fLOAT_SIZE :: Int
fLOAT_SIZE = 4

tARGET_MAX_CHAR :: Int
tARGET_MAX_CHAR = 0x10ffff
```

## Internals

https://notepad.mmakowski.com/Advanced%20Haskell


## Lists

- imperative: destructive update (ephemeral data structures)
- functinal: creation of a new value (persistent data structures)

[1,2,3,4] is syntactic sugar for 1:(2:(3:(4:[])))

When doing `let y = 0:xs`, the `xs` is reused; it is safe to share it, because it is immutable. The same happens when we `drop 3 ys`, it is pointer. However, when we `take 2 ys`, we have to copy the cons cells.

In-memory representation

Words of memory:

The first word identifies the constructor. The other words contain the payload (pointers to constructor args):

+---+----------+----------+
|(:)|hd ptr -> |tl ptr -> |
+---+----------+----------+


Thunks are represented similarly, as heap objects:

+---+----------+----------+
|fn |arg 1 ->  |arg 2 ->  |
+---+----------+----------+

The first field indicates whether it is a thunk or constructor. If thunk, it can be replaced by evaluated value (one of very few cases where in Haskell something gets changed destructively). The benefit is that if a shared thunk is evaluated, the value will be available to all places pointing to it.
