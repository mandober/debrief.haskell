# Enumerator

## enumerator package
https://hackage.haskell.org/package/enumerator

Typical buffer-based incremental I/O is based around a single loop, which reads data from some source (such as a socket or file), transforms it, and generates one or more outputs (such as a line count, HTTP responses, or modified file). Although efficient and safe, these loops are all single-purpose; it is difficult or impossible to compose buffer-based processing loops.

Haskell's concept of "lazy I/O" allows pure code to operate on data from an external source. However, lazy I/O has several shortcomings. Most notably, resources such as memory and file handles can be retained for arbitrarily long periods of time, causing unpredictable performance and error conditions.

Enumerators are an efficient, predictable, and safe alternative to lazy I/O. Discovered by Oleg Kiselyov, they allow large datasets to be processed in near-constant space by pure code. Although somewhat more complex to write, using enumerators instead of lazy I/O produces more correct programs.

This library contains an enumerator implementation for Haskell, designed to be both simple and efficient. Three core types are defined, along with numerous helper functions:

Iteratee: Data sinks, analogous to left folds. Iteratees consume a sequence of input values, and generate a single output value. Many iteratees are designed to perform side effects (such as printing to stdout), so they can also be used as monad transformers.

Enumerator: Data sources, which generate input sequences. Typical enumerators read from a file handle, socket, random number generator, or other external stream. To operate, enumerators are passed an iteratee, and provide that iteratee with input until either the iteratee has completed its computation, or EOF.

Enumeratee: Data transformers, which operate as both enumerators and iteratees. Enumeratees read from an outer enumerator, and provide the transformed data to an inner iteratee.
