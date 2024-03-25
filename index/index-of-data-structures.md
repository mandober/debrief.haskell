# Haskell :: Index :: Data Structures

- Data.Tuple: `data (,,,) a b c d`
  - unit
  - Solo, `data (a,)`
  - pair: `data (,) a b`
  - triple: `data (,,) a b c`
- lists
  - Data.List, GHC.OldList: `data [] a = [] | a : [a]`
  - Data.List.NonEmpty, `data NonEmpty a = a :| [a]`
  - Data.Sequence, as if `data Seq a = Empty | a :<| !(Seq a)`
    https://hackage.haskell.org/package/containers/docs/Data-Sequence.html
    https://en.wikibooks.org/wiki/Haskell/Libraries/Data_structures_primer
- sets
  - Data.Set, `data Set a`
- hash
  - unordered-containers `Data.HashMap`
  - unordered-containers `Data.HashSet`
- maps
  - containers.Data.Map, `data Map k v`
  https://hackage.haskell.org/package/unordered-containers-0.2.5.1/docs/src/Data-HashMap-Base.html#HashMap
  - containers.Data.IntMap
  - containers.Data.IntSet
  - containers.StrictPair, `data StrictPair a b = !a :*: !b`
    - same as a regular pair, except: `(x :*: ⊥) = (⊥ :*: y) = ⊥`
  - containers: more
- arrays
  - https://hackage.haskell.org/package/array
    - Data.Array.Base
    - Data.Array.IArray
    - Data.Array.MArray
    - Data.Array.ST
    - Data.Array.Storable
    - Data.Array.Unboxed
    - Data.Array.Unsafe
  - GHC.Arr
  - GHC.ArrayArray
  - GHC.IOArray
  - Data.Array
  - Data.Array.Byte
- vectors
  - https://hackage.haskell.org/package/vector
  - Data.Vector - Boxed vectors of arbitrary types
  - Data.Vector.Unboxed - Unboxed vectors with adaptive repr
  - Data.Vector.Storable - Unboxed vectors of Storable types
  - Data.Vector.Primitive - Unboxed vectors of primitive types
  - Data.Vector.Generic
- ADTs
  - Data.Maybe: `data Maybe a = Nothing | Just a`
  - Data.Either: `data Either a b = Left a | Right b`
- Data.Tree, `data Tree a = Node a [Tree a]`
- Data.Graph
- Data.Unique
- Data.Proxy
- Data.STRef

- Data.IORef
- Data.STRef
- Data.Dynamic
- Data.Functor.Identity
- Control.Arrow
- Control.Category

Control.Concurrent.STM
Control.Concurrent.STM.TArray
Control.Concurrent.STM.TBQueue
Control.Concurrent.STM.TVar
Control.Concurrent.STM.TMVar
Control.Concurrent.STM.TChan
Control.Concurrent.STM.TSem
Control.Concurrent.STM.TQueue
Control.Concurrent.QSem
Control.Concurrent.QSemN
Control.Concurrent.Chan
Control.Concurrent.MVar
