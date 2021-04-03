# GHC constants

https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-8.10.1/src/Constants.html

```hs
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
