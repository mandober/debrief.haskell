# Links

http://haskell.spreadshirt.com/oleg-already-did-it-A6499531

Why iteratees are hard to understand, 2012
http://blog.ezyang.com/2012/01/why-iteratees-are-hard-to-understand/

Sequences, streams, and segments, 2008
http://conal.net/blog/posts/sequences-streams-and-segments

From Bits to Cells: Simple Cellular Automata in Haskell, 2006
http://praisecurseandrecurse.blogspot.com/2006/12/from-bits-to-cells-simple-cellular.html

A tutorial on the enumerator library, 2011
http://www.mew.org/~kazu/proj/enumerator/

Understanding iteratees, 2010
https://web.archive.org/web/20111129015704/https://john-millikin.com/articles/understanding-iteratees/

Evaluating cellular automata is comonadic, 2006
http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html


## Packages

### Conduit

https://hackage.haskell.org/package/conduit
conduit is a solution to the streaming data problem, allowing for production, transformation, and consumption of streams of data in constant memory. It is an alternative to lazy I/O which guarantees deterministic resource handling.
For more information about conduit in general, and how this package in particular fits into the ecosystem, see the conduit homepage:
https://github.com/snoyberg/conduit#readme
For up to date docsm see: http://www.stackage.org/package/conduit

Batteries included conduit - adapters for common libraries:
https://hackage.haskell.org/package/conduit-extra


### Pipes

https://hackage.haskell.org/package/pipes
pipes is a clean and powerful stream processing library that lets you build and connect reusable streaming components.
Advantages over traditional streaming libraries:
- Concise API: Use simple commands like for, (>->), await, and yield
- Blazing fast: Implementation tuned for speed, including shortcut fusion
- Lightweight Dependency: pipes is small and compiles very rapidly, including dependencies
- Elegant semantics: Use practical category theory
- ListT: Correct implementation of ListT that interconverts with pipes
- Bidirectionality: Implement duplex channels
- Extensive Documentation: Second to none!

Import [Pipes](https://hackage.haskell.org/package/pipes-4.3.16/docs/Pipes.html) to use the library. Read Pipes.Tutorial for an extensive tutorial.

Pipes.Tutorial
https://hackage.haskell.org/package/pipes-4.3.16/docs/Pipes-Tutorial.html


### Quiver

https://hackage.haskell.org/package/quiver

Quiver is a powerful stream processing library for combinatorial and monadic representation of computations over both inductive and coinductive data streams.

It is similar to Gabriel Gonzalez's pipes and Michael Snoyman's conduit, but generalises both with support for functor-based computations and a clean support for finite (i.e., inductive) data streams, both upstream and downstream of the computation being defined.

The underlying data structure, P, is almost identical to the Proxy data structure of the Pipes library, except that the Consume and Produce constructors (corresponding, respectively, to Request and Response in the Pipes' Proxy data type) include an additional argument which explicitly captures the processor's behaviour in the event of input stream depletion (for Consume) or output decoupling (for Produce). This simple mechanism subsumes Conduit's need for elaborate unconsumed-input tracking mechanisms, and allows us to provide a mathematically-clean framework for processing of finite data streams.

This library is currently very young, and users should expect significant changes to the Quiver core combinators as the underlying theory is developed and the interface stabilises asymptotically to the future version 1.0.

### iterIO

iterIO: Iteratee-based IO with pipe operators
https://hackage.haskell.org/package/iterIO
https://hackage.haskell.org/package/iterIO-0.2.2/docs/Data-IterIO.html

Iteratee-based IO is an alternative to lazy IO that offers better error handling, referential transparency, and convenient composition of protocol layers or parsers. This package provides iteratees based around pipe operators for hooking together application components and directing data flow. New users should see the tutorial in the Data.IterIO module documentation. Highlights of the library include:

Heavy emphasis on ease of use, ease of learning, and uniformity of mechanism.

Copious documentation.

Consistent EOF and error handling to avoid resource leaks and other issues in corner cases.

A set of iteratee parsing combinators providing LL(*) parsing while generally not consuming large amounts of memory for backtracking.

Seamless integration with attoparsec for LL(1) parsing.

See Data.IterIO for a discussion of the differences between iterIO and the two previous iteratee implementations (iteratee and enumerator).


### other

https://hackage.haskell.org/package/enumerator

streams: Various Haskell 2010 stream comonads, 2010
https://hackage.haskell.org/package/streams






## Blogs

A Neighborhood Of Infinity
http://blog.sigfpe.com/

Praise, Curse, and Recurse
http://praisecurseandrecurse.blogspot.com/

Conal Elliott - Inspirations & experiments, mainly about FP in Haskell
http://conal.net/blog/

Ezyang's blog - the arc of software bends towards understanding
http://blog.ezyang.com/
