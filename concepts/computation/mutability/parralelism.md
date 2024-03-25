# Parralelism

`ThreadScope` - a graphical tool for profiling parallel Haskell programs
https://wiki.haskell.org/ThreadScope
https://wiki.haskell.org/ThreadScope_Tour
https://github.com/haskell/ThreadScope/
https://www.youtube.com/watch?v=qZXq8fxebKU

* A Tutorial on Parallel and Concurrent Programming in Haskell
Satnam Singh, Simon Peyton Jones, 2008

https://www.microsoft.com/en-us/research/publication/a-tutorial-on-parallel-and-concurrent-programming-in-haskell/

This practical tutorial introduces the features available in Haskell for writing parallel and concurrent programs. We first describe how to write semi-explicit parallel programs by using annotations to express opportunities for parallelism and to help control the granularity of parallelism for effective execution on modern operating systems and processors. We then describe the mechanisms provided by Haskell for writing explicitly parallel programs with a focus on the use of software transactional memory to help share information between threads. Finally, we show how nested data parallelism can be used to write deterministically parallel programs which allows programmers to use rich data types in data parallel programs which are automatically transformed into flat data parallel versions for efficient execution on multi-core processors.
