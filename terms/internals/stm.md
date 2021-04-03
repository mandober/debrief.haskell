# Software Transactional Memory


STM implementation

* [Software Transactional Memory](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/stm)

* papers [Composable Memory Transactions](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm.pdf) and [Transactional memory with data invariants](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm-invariants.pdf)

* Additional details can be found in the Harris et al book [Transactional memory](http://www.morganclaypool.com/doi/abs/10.2200/s00272ed1v01y201006cac011)

* Some analysis on performance can be found in the paper [The Limits of Software Transactional Memory](https://www.bscmsrc.eu/sites/default/files/cf-final.pdf) though this work only looks at the coarse grain lock version.

* Many of the other details here are gleaned from the comments in the source code.

* STM documents assumes the reader is familiar with some general details of GHC's execution and memory layout. A good starting point for this information is can be found here: [Generated Code](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/generated-code)


## Overview of Features

At the high level, transactions are computations that read and write to `TVar`s with changes only being committed atomically after seeing a consistent view of memory. Transactions can also be composed together, building new transactions out of existing transactions. In the RTS each transaction keeps a record of its interaction with the TVars it touches in a `TRec`. A pointer to this record is stored in the TSO that is running the transaction.
