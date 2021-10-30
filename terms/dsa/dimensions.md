# Dimensions of data structures

In Haskell, data structures may be classified into some of the traditional categories, but since Haskel is pure FP with non-strict (lazy) semantics, most of the categories and the classification process itself will have to be reworked.

The notion of a data structure and a data type is more blurred in Haskell. All data structures are naturally data types in every PL, but in Haskell particularly, the types become intertwined with the operation and meaning of a data structure, they become intechangable.


General dimensions
- pure v mutable (FP mutation)
- persistent v nonpersistent
- succint v non
- finite v infinite
- recursive v non-recursive
- nested

Dimensions
- the amount of overhead


Lists
- the empty list
- singleton
- finite list
- infinite list

## Recursive data structures

It is remarkable that recursion can be separated from a recursive data structure, permitting for a non-recursive definition of a data structure; a a later time (and possibly different module), recursion can be plugged back in, like a sort of a structural enhancement, or an opt-in feature!
