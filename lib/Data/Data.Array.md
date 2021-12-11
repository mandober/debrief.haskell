# Data.Array

Data.Array
Data.Array.Base
Data.Array.IArray
Data.Array.IO
Data.Array.IO.Internals
Data.Array.IO.Safe
Data.Array.Lens
Data.Array.MArray
Data.Array.MArray.Safe
Data.Array.ST
Data.Array.ST.Safe
Data.Array.Storable
Data.Array.Storable.Internals
Data.Array.Storable.Safe
Data.Array.Unboxed
Data.Array.Unsafe

```hs
-- ----------------------------------------------------------------------------
-- Creation
-- ----------------------------------------------------------------------------
-- make an array with the given bounds and values specified as an assoc list
array     :: forall i e. (Ix i) => (i, i)    -> [(i, e)] -> Array i e

-- make an array from a pair of bounds and a list of values.
listArray :: forall i e. (Ix i) => (i, i)    -> [e]      -> Array i e

{-| Make an array identical to arg1, but updated by the assoc in arg2.

  For example, if m is a 1-origin, n⨯n matrix, then

  >>> m // [ ((i,i), 0) | i <- [1..n] ]

  is the same matrix, except with the diagonal zeroed.

  Repeated indices in the association list are handled as for 'array'.
-}
(//)      :: forall i e. (Ix i) => Array i e -> [(i, e)] -> Array i e




{-| `accum f` takes an array and an assoc list and accumulates pairs from
  the list into the array with the accumulating function f. Thus accumArray can be defined using accum:

  >>> accumArray f z b = accum f (array b [(i, z) | i <- range b])

  accum is strict in all the results of applying the accumulation.
  However, it is lazy in the initial values of the array.
-}
accum :: forall i e a. (Ix i)
      => (e -> a -> e)
      -> Array i e
      -> [(i, a)]
      -> Array i e

{-| accumArray deals with repeated indices in the assoc list using an
  accumulating function which combines the values of associations with the same index. For example, given a list of values of some index type, `hist` produces a histogram of the number of occurrences of each index within a specified range:

  >>> hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b

  >>> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

  accumArray is strict in each result of applying the accumulating function, although it is lazy in the initial value. Thus, unlike arrays built with 'array', accumulated arrays should not in general be recursive.
-}
accumArray :: forall i e a. (Ix i) 
           => (e -> a -> e)
           -> e
           -> (i, i)
           -> [(i, a)]
           -> Array i e

{-| ixmap allows for transformations on array indices.
  It may be thought of as providing function composition on the right with the mapping that the original array embodies. A similar transformation of array values may be achieved using fmap from the 'Array' instance of the 'Functor' class.
-}
ixmap :: forall i j e. (Ix i, Ix j)
      => (i, i)
      -> (i -> j)
      -> Array j e
      -> Array i e


-- ----------------------------------------------------------------------------
-- Query
-- ----------------------------------------------------------------------------
-- The value at the given index in an array.
(!)       :: forall i e. (Ix i) => Array i e -> i -> e

-- The list of elements of an array in index order.
elems     :: forall i e.           Array i e -> [e]

-- The list of indices of an array in ascending order.
indices   :: forall i e. (Ix i) => Array i e -> [i]

-- The list of associations of an array in index order
assocs    :: forall i e. (Ix i) => Array i e -> [(i, e)]

-- The bounds with which an array was constructed.
bounds    :: forall i e. Array i e -> (i, i)

-- ----------------------------------------------------------------------------
-- Indices
-- ----------------------------------------------------------------------------
-- The position of a subscript in the subrange
index     :: forall a. (Ix a) => (a, a) -> a -> Int

-- The list of values in the subrange defined by a bounding pair.
-- >>> range (1,3) -- [1,2,3]
range     :: forall a. (Ix a) => (a, a) -> [a]

-- The size of the subrange defined by a bounding pair.
rangeSize :: forall a. (Ix a) => (a, a) -> Int

-- Check if the given subscript lies in the range defined the bounding pair.
inRange   :: forall a. (Ix a) => (a, a) -> a -> Bool

-- ----------------------------------------------------------------------------
-- Array data type
-- ----------------------------------------------------------------------------
-- Defined in 'GHC.Arr'
type role Array nominal representational
type Array :: Type -> Type -> Type
data Array i e = GHC.Arr.Array
                !i
                !i
                {-# UNPACK #-}Int
                (GHC.Prim.Array# e)

-- Defined in 'GHC.Arr'
instance forall i e. (Ix i, Eq e) => Eq (Array i e)
-- Defined in 'GHC.Arr'
instance forall i. Functor (Array i)
-- Defined in 'GHC.Arr'
instance forall i e. (Ix i, Ord e) => Ord (Array i e)
-- Defined in 'GHC.Arr'
instance forall a b. (Ix a, Show a, Show b) => Show (Array a b)
-- Defined in 'GHC.Read'
instance forall a b. (Ix a, Read a, Read b) => Read (Array a b)
-- Defined in 'Data.Foldable'
instance forall i. Foldable (Array i)
-- Defined in 'Data.Traversable'
instance forall i. Ix i => Traversable (Array i)



-- Defined in GHC.Ix
type Ix :: Type -> Constraint
class Ord a => Ix a where
    range                   :: (a, a) -> [a]
    inRange                 :: (a, a) -> a -> Bool
    index                   :: (a, a) -> a -> Int
    GHC.Ix.unsafeIndex      :: (a, a) -> a -> Int
    rangeSize               :: (a, a) -> Int
    GHC.Ix.unsafeRangeSize  :: (a, a) -> Int
{-# MINIMAL range, (index | unsafeIndex), inRange #-}

-- Ix instances (in GHC.Ix)
instance Ix Word
instance Ix Ordering
instance Ix Integer
instance Ix Int
instance Ix Char
instance Ix Bool
instance Ix ()
instance forall a b.   (Ix a, Ix b) =>       Ix (a, b)
instance forall a b c. (Ix a, Ix b, Ix c) => Ix (a, b, c)
-- n-tuples…
```
