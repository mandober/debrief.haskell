# Data.Set

```hs
import Data.Set qualified as S

S.Set                   S.foldl                 S.member
S.\\                    S.foldl'                S.minView
S.alterF                S.foldr                 S.notMember
S.cartesianProduct      S.foldr'                S.null
S.delete                S.fromAscList           S.partition
S.deleteAt              S.fromDescList          S.powerSet
S.deleteFindMax         S.fromDistinctAscList   S.showTree
S.deleteFindMin         S.fromDistinctDescList  S.showTreeWith
S.deleteMax             S.fromList              S.singleton
S.deleteMin             S.insert                S.size
S.difference            S.intersection          S.spanAntitone
S.disjoint              S.isProperSubsetOf      S.split
S.disjointUnion         S.isSubsetOf            S.splitAt
S.drop                  S.lookupGE              S.splitMember
S.dropWhileAntitone     S.lookupGT              S.splitRoot
S.elemAt                S.lookupIndex           S.take
S.elems                 S.lookupLE              S.takeWhileAntitone
S.empty                 S.lookupLT              S.toAscList
S.filter                S.lookupMax             S.toDescList
S.findIndex             S.lookupMin             S.toList
S.findMax               S.map                   S.union
S.findMin               S.mapMonotonic          S.unions
S.fold                  S.maxView               S.valid
```
