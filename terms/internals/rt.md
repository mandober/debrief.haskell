# RT

GHC's runtime system is a slightly scary beast: 50,000 lines of `C` and `C--` code, much of which seems at first glance to be completely obscure.

RTS includes:
- everything required to execute Haskell code that is not compiled into the program (code) itself, such as the code to raise an exception when you call `error`, code to allocate `Array#` objects, code to implement `takeMVar#`.
- sophisticated storage manager, including a multi-generational garbage collector with copying and compacting strategies.
- user-space scheduler for Haskell threads with support for scheduling them across multiple CPUs and allowing them to call foreign functions in separate OS threads.
- byte-code interpreter for GHCi and a dynamic linker for loading up object code into a GHCi session.
- heap-profiling (of various kinds), time-profiling and code coverage.
- support for Software Transactional Memory (STM)




## Refs

What does the Haskell runtime look like?
https://www.reddit.com/r/haskell/comments/1f48dc/what_does_the_haskell_runtime_look_like/

GHC Commentary: The Runtime System
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts

https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging/runtime-system

Implementing functional languages: a tutorial
https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2FPapers%2Fpj-lester-book%2F
