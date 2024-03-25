# Index of pragmas

* GHC pragmas
  * File-header pragmas
    - LANGUAGE
    - OPTIONS_GHC
    - INCLUDE
  * info
    - WARNING
    - DEPRECATED
  * source code annotations
    - ANN
    - ANN type
    - ANN module
  * inlining
    - INLINE
      - INLINE CONLIKE (modifier)
    - NOINLINE
      - NOINLINE CONLIKE (modifier)
    - INLINABLE
  * source file location
    - LINE
    - COLUMN
  * rewriting
    - RULES (specify rewrite rules)
    * specialization
      - SPECIALIZE
      - SPECIALIZE INLINE
      - SPECIALIZE instance
  * packing
    - UNPACK (in data ctor fields)
    - NOUNPACK
  - SOURCE (after import to break a module loop)
  - COMPLETE
  * class instances
    - OVERLAPPING
    - OVERLAPPABLE
    - OVERLAPS
    - INCOHERENT


The pragmas OVERLAPPING, OVERLAPPABLE, OVERLAPS, INCOHERENT are used to specify the overlap behavior for individual instances (as described in Overlapping instances). The pragmas are written immediately after the instance keyword, like this: `instance {-# OVERLAPPING #-} C t where ...`

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#rewrite-rules
