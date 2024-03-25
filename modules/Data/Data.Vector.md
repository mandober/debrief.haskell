# Data.Vector


```hs
import Data.Vector qualified as V
-- V.<TAB> 214 names

type Vector :: Type -> Type
data Vector a = Data.Vector.Vector
    {-# UNPACK #-}Int     -- len
    {-# UNPACK #-}Int     -- cap?
    {-# UNPACK #-}Array a
 -- {-# UNPACK #-}(primitive-0.7.4.0:Data.Primitive.Array.Array a)

V.!
V.!?
V.++
V.//

V.MVector
V.Vector

V.fromArray
V.toArray
V.toArraySlice

V.toList
V.fromList
V.fromListN

V.all
V.and
V.any

V.backpermute
V.break
V.catMaybes
V.cmpBy
V.concat
V.concatMap

V.cons

V.constructN

V.constructrN
V.convert
V.copy

V.create
V.createT

V.drop
V.dropWhile

V.elem
V.elemIndex
V.elemIndices
V.empty

V.enumFromN
V.enumFromStepN
V.enumFromThenTo
V.enumFromTo

V.eqBy
V.filter
V.filterM

V.find
V.findIndex
V.findIndices

V.forM
V.forM_

V.force
V.freeze

V.generate
V.generateM
V.group
V.groupBy
V.head
V.headM

V.last
V.lastM
V.length

V.map
V.mapM
V.mapM_
V.mapMaybe
V.mapMaybeM

V.maxIndex
V.maxIndexBy
V.maximum
V.maximumBy
V.maximumOn

V.minIndex
V.minIndexBy
V.minimum
V.minimumBy
V.minimumOn


V.modify
V.notElem
V.null
V.or

V.partition
V.partitionWith

V.product
V.replicate
V.replicateM
V.reverse

V.sequence
V.sequence_

V.singleton
V.slice
V.snoc
V.span
V.splitAt
V.sum
V.tail
V.take
V.takeWhile
V.thaw

V.uniq
V.uncons
V.unsnoc
V.unstablePartition
V.update
V.update_

V.accum
V.accumulate
V.accumulate_


V.ifoldM
V.ifoldM'
V.ifoldM'_
V.ifoldM_
V.ifoldl
V.ifoldl'
V.ifoldr
V.ifoldr'
V.iforM
V.iforM_
V.imap
V.imapM
V.imapM_
V.imapMaybe
V.imapMaybeM
V.ifilter
V.indexM
V.indexed
V.init
V.iscanl
V.iscanl'
V.iscanr
V.iscanr'
V.iterateN
V.iterateNM

V.fold1M
V.fold1M'
V.fold1M'_
V.fold1M_
V.foldM
V.foldM'
V.foldM'_
V.foldM_
V.foldMap
V.foldMap'
V.foldl
V.foldl'
V.foldl1
V.foldl1'
V.foldr
V.foldr'
V.foldr1
V.foldr1'

V.postscanl
V.postscanl'
V.postscanr
V.postscanr'

V.prescanl
V.prescanl'
V.prescanr
V.prescanr'

V.scanl
V.scanl'
V.scanl1
V.scanl1'
V.scanr
V.scanr'
V.scanr1
V.scanr1'

V.unfoldr
V.unfoldrExactN
V.unfoldrExactNM
V.unfoldrM
V.unfoldrN
V.unfoldrNM

V.unzip
V.unzip3
V.unzip4
V.unzip5
V.unzip6

V.izipWith
V.izipWith3
V.izipWith4
V.izipWith5
V.izipWith6
V.izipWithM
V.izipWithM_

V.zip
V.zip3
V.zip4
V.zip5
V.zip6
V.zipWith
V.zipWith3
V.zipWith4
V.zipWith5
V.zipWith6
V.zipWithM
V.zipWithM_

V.unsafeAccum
V.unsafeAccumulate
V.unsafeAccumulate_
V.unsafeBackpermute
V.unsafeCopy
V.unsafeDrop
V.unsafeFreeze
V.unsafeFromArraySlice
V.unsafeHead
V.unsafeHeadM
V.unsafeIndex
V.unsafeIndexM
V.unsafeInit
V.unsafeLast
V.unsafeLastM
V.unsafeSlice
V.unsafeTail
V.unsafeTake
V.unsafeThaw
V.unsafeUpd
V.unsafeUpdate
V.unsafeUpdate_
```
