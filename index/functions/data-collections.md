# Data Collections

* Data.List
  - CList, cons list: `data List a = Nil | Cons (a, List a)`
  - SList, snoc list: `data List a = Nil | Snoc (List a, a)`
  - TList, cons + snoc list,
    `data List a = Cons (a, List a) | Nil | Snoc (List a, a)`
  - HList, heterogeneous
  - DList, difference list
  - AList, associative list
  - ZList, zipper list, `([a],a,[a])`
  - ZipList - lists, but with an Applicative functor based on zipping.
- Data.Sequence
- Data.Set
- Data.Map
- Data.Tree
- Data.Vector
- Data.Array


* collections
  * kind
    - homogeneous
    - heterogeneous
  * representation
  * invariants
  * persistance
    - semi-persistant
    - fully-persistant
  * mutability
    - mutable
    - purely functional
  * access operations
    - time complexity
      - O(1)        constant
      - O(log n)    logarithmic
      - O(n log n)  linearithmic
      - O(n)        linear
    - amortization
    - space complexity
    - overhead


structure   cons  snoc  concat
List        O(1)  O(n)  O(n)
DList       O(1)  O(n)  O(1)
