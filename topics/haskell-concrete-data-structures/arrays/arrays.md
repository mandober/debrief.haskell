# Haskell :: Data structures :: Arrays

Data structures
- arrays
  - `Array i e` from `Data.Array`


- `Data.Array.Array i e` is based on `GHC.Prim.Array# e`


## Names

```hs
-- Array type (defined in GHC.Arr)
type role Array nominal representational
type Array :: Type -> Type -> Type
data Array i e = GHC.Arr.Array !i
                               !i
                {-# UNPACK #-}Int
              (GHC.Prim.Array# e)

-- Defined in GHC.Arr
instance forall i e. (Ix i, Eq e) => Eq (Array i e)

-- Defined in GHC.Arr
instance forall i. Functor (Array i)

-- Defined in GHC.Arr
instance forall i e. (Ix i, Ord e) => Ord (Array i e)

-- Defined in GHC.Arr
instance forall a b. (Ix a, Show a, Show b) => Show (Array a b)

-- Defined in Data.Foldable
instance forall i. Foldable (Array i)

-- Defined in GHC.Read
instance forall a b. (Ix a, Read a, Read b) => Read (Array a b)

-- Defined in Data.Traversable
instance forall i. Ix i => Traversable (Array i)




type Assoc i e = [(i, e)]
Assoc i e ≅ Array i e

-- array creation
A.array     :: (Ix i) => (i, i) -> Assoc i e -> Array i e
A.listArray :: (Ix i) => (i, i) -> [e] -> Array i e

{-
  Constructs an array identical to the first (array) arg
  but updated by the association list in the second arg.

  For example
  if m is a 1-origin, n⨯n matrix (2D array indexed by a pair) then

  >>> m // [ ((i,i), 0) | i <- [1..n] ]

  is the same matrix, except with the diagonal zeroed.

  Repeated indices in the association list are handled as for 'array':
    Haskell2010 specifies that the resulting array is undefined (i.e. bottom),
    but GHC simply prioritize the last association for each index.
-}
A.// :: (Ix i) => Array i e -> Assoc i e -> Array i e


A.accum :: Ix i => (e -> a -> e) -> Array i e -> Assoc i a -> Array i e
{-
  'accum f' further expects an array and an association list,
  and accumulates pairs from the list into the array 
  using the accumulating function f.

  Thus, accumArray can be defined using accum:

  >>> accumArray f z b = accum f (array b [(i, z) | i <- range b])

  accum is strict in all the results of applying the accumulation.
  However, it is lazy in the initial values of the array.
-}
A.accum :: forall i e a. Ix i =>
           (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e


A.accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> Assoc i a -> Array i e
{-
  The 'accumArray' function deals with repeated indices in the association
  list using an accumulating function which combines the values of
  associations with the same index.

  For example,
  given a list of values of some index type,
  hist produces a histogram of the number of occurrences
  of each index within a specified range:

  >>> hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
  >>> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

  accumArray is strict in each result of applying the accumulating function,
  although it is lazy in the initial value. Thus, unlike arrays built with
  'array', accumulated arrays should not in general be recursive.
-}
A.accumArray :: forall i e a. Ix i =>
                (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e


A.assocs :: (Ix i) => Array i e -> Assoc i e

A.!
A.ixmap
A.index
A.indices :: (Ix i) => Array i e -> [i]
A.bounds

A.elems

A.range
A.rangeSize
A.inRange

-- Ix class
class Ord a => Ix a where
  index                  :: (a, a) -> a -> Int
  range                  :: (a, a) -> [a]
  inRange                :: (a, a) -> a -> Bool
  rangeSize              :: (a, a) -> Int
  GHC.Ix.unsafeIndex     :: (a, a) -> a -> Int
  GHC.Ix.unsafeRangeSize :: (a, a) -> Int
{-# MINIMAL range, (index | unsafeIndex), inRange #-}
```

## Data.Array.Array

`Array i e` from `Data.Array` is a type of 
- immutable
- non-strict
- boxed
arrays with indices `i` and elements `e`.

* Indices

Indices `i` may be any type that implements the `Ix` class.

`GHC.Ix` module contains impls of `Ix` for
- ()
- Bool
- Ordering
- Word
- Char
- Int
- Integer
- Solo
  - `instance (Ix a) => Ix (Solo a)`
- tuples
  - `instance (Ix a, Ix b) => Ix (a, b)`
  - up to 15-tuple

So, even the unit may be a (strange choice of) index, but only for an array with a single entry (singleton array).

An entry is a pair (index, value), so an array (at least this type) is like a `Map k v` ≅ `Array i e`, with indices as keys, and values that are referred to as elements (thus the `e` type var). Being like a map, an association list is one of the way to create a new array, using the function `A.array` that expects an assoc list [(i,e)].

  A.array     :: Ix i => (i, i) -> [(i, e)] -> Array i e
  A.listArray :: Ix i => (i, i) -> [e] -> Array i e
