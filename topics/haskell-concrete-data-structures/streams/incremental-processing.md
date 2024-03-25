# Streams and incremental processing

Streams and Incremental Processing: http://okmij.org/ftp/Streams.html

**Stream processing** defines a *pipeline of operators* that transform, combine, or reduce (even to a single scalar value) large amounts of data.

Commonly, data is
- accessed linearly (rather than randomly and repeatedly) and
- processed uniformly

The upside of the limited expressiveness is the opportunity to process large amount of data efficiently, in constant space.

## TOC

Local
- Introduction
- Stream Fusion, to Completeness
- Functional Stream Libraries and Fusion
- Streams in Linear Algebra
- How to zip folds: A library of fold transformers (streams)

External
- Iteratee
  http://okmij.org/ftp/Haskell/Iteratee/
- Music of Streams in a strict PL
  http://okmij.org/ftp/ML/powser.html
- Simple generators
  http://okmij.org/ftp/continuations/PPYield/
- Shell-like pipes
  http://okmij.org/ftp/ML/myawk/index.html
- Differentiating Parsers
  http://okmij.org/ftp/continuations/differentiating-parsers.html
- Justifying layered streams for input processing
  http://okmij.org/ftp/Haskell/Iteratee/index.html#for-layering
- Generators: the API for traversal and non-determinism
  http://okmij.org/ftp/continuations/generators.html



## Introduction
http://okmij.org/ftp/Streams.html#intro

Stream data processing (already present in COBOL!) has been coming to forefront with the popularity of *big data* and `MapReduce`.

The uniformity and the predictability of data access help efficiently handle vast amount of data, far more than fits within the memory of a single processor.

Stream processing also exhibits the painful *abstraction vs. performance trade-off*:
* Manually written loops and state-machines offer the highest performance and the least memory overhead, but are not reusable or extensible.
* Libraries of freely composable stream components let programmers quickly assemble an obviously correct stream application, but suffer from the high overhead of abstractions, mainly due to the repeated creation and disposal of *intermediate data structures* (closures, captured continuations, objects, collections, and most of all, intermediate streams). Eliminating such intermediate data structures is broadly known as *stream fusion*.



## Functional Stream Libraries and Fusion
http://okmij.org/ftp/Streams.html#shonan-136

Functional stream libraries let us easily build stream processing pipelines, by composing sequences of simple *transformers* (`map` or `filter`) with *producers* (backed by an array, file, generating function) and *consumers* (*reducers*).

The purely applicative approach of building a complex pipeline from simple immutable pieces simplifies programming and reasoning: the assembled pipeline is an executable specification. To be practical, however, a library has to be efficient: at the very least, it should avoid creating intermediate structures - especially structures like files and lists whose size grows with the length of the stream. Even the bounded-size intermediate structures significantly, up to two orders of magnitude, slow down the processing. Eliminating the intermediate structures is the central problem in stream processing: so-called stream fusion.

Several main questions come up over and over again:
- *Push and pull duality*, their expressiveness and efficiency
- error handling, and generally, control of stream processing sparsity of data

As a tangible outcome, the meeting has identified a set of problems and challenges to help drive and evaluate further research: `filterMax`, *sorted merge*, *multiple appends*, *parallel merge*.


## Zipping streams

How to zip folds: A library of fold transformers (streams)
http://okmij.org/ftp/Streams.html#zip-folds
