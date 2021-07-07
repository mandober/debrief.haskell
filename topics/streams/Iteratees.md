# Iteratees

## Incremental multi-level input processing and collection enumeration
http://okmij.org/ftp/Haskell/Iteratee/

*Iteratee/Enumeratee* is a way to process data (file bytes, network datagrams or matrix elements) incrementally, declaratively and with tight resource control.

The approach has been inspired by fold, parser/printer combinators and circuit diagrams. There are several variants of the Iteratee-based I/O, available as independent libraries on Hackage or as part of bigger applications such as web servers. Here we give motivation, justification and explanation, pointing out earlier and experimental versions as well as related projects.

## Iteratee IO

Iteratee IO is a style of incremental input processing with precise resource control.

The style encourages building input processors from a user-extensible set of primitives by chaining, layering, pairing and other modes of compositions. The programmer is still able, where needed, to precisely control look-ahead, the allocation of buffers, file descriptors and other resources. The style is especially suitable for processing of communication streams, large amount of data, and data undergone several levels of encoding such as pickling, compression, chunking, framing. It has been used for programming high-performance (HTTP) servers and web frameworks, in computational linguistics and financial trading.

We exposit programming with iteratees, contrasting them with *Lazy IO* and the *Handle-IO* (handle-based, stdio-like IO).

We relate them to online parser combinators. We introduce a simple implementation as free monads, which lets us formally reason with iteratees. As an example, we validate several equational laws and use them to optimize iteratee programs. The simple implementation helps understand existing implementations of iteratees and derive new ones.
