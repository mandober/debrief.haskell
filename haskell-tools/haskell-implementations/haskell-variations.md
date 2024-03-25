# Variations of Haskell

<!-- TOC -->

- [Curry](#curry)
- [Template Haskell](#template-haskell)
- [Eden](#eden)
- [Discus](#discus)
- [Data Field Haskell](#data-field-haskell)
- [Dependent Haskell](#dependent-haskell)
- [Chameleon](#chameleon)
- [Gofer](#gofer)
- [GPH](#gph)
- [pH](#ph)
- [Goffin](#goffin)
- [PolyP - a polytypic programming language](#polyp---a-polytypic-programming-language)
- [O'Haskell](#ohaskell)

<!-- /TOC -->

## Curry
Curry seamlessly combines features from functional programming (nested expressions, higher-order functions, lazy evaluation), logic programming (logical variables, partial data structures, built-in search), and concurrent programming (concurrent evaluation of expressions with synchronization on logical variables). Many Haskell programs are also valid Curry programs.

## Template Haskell
https://wiki.haskell.org/Template_Haskell

Template Haskell is an extension to Haskell 98 that allows you to do type-safe compile-time meta-programming, with Haskell both as the manipulating language and the language being manipulated.

## Eden
https://www.mathematik.uni-marburg.de/~eden/
Eden is a parallel functional language that provides a new perspective on *parallel programming*. It gives programmers enough control to implement their parallel algorithms efficiently (including granularity issues) and at the same time frees them from the low level details of process management.

Eden is explicit about processes and their incoming and outgoing data, but abstracts from the transfer of these data between processes and the necessary synchronisation. Eden extends the Haskell but overrules lazy evaluation whenever it's necessary to support parallelism.

## Discus

Home Page:    http://www.discus-lang.org/
GitHub Page:  https://github.com/discus-lang

The Discus language (ex Disciple language) is an experimental dialect of Haskell which investigates static typing and program transformation in the presence of computational effects. The main language features are:
- Haskell-like source language, so Haskell-like programs should work with minor modifications
- Modal region and effect system using 'box' and 'run' to suspend and force computations
- Higher rank polymorphism with bidirectional type inference
- Simple two space copying garbage collection
- Default call-by-value evaluation
- Typed external core language

The latest version is 0.5.1, released on 2017-10-23, which includes new support for copying garbage collection and implicit function parameters.

Discus is a dialect of Haskell that uses *strict evaluation* as the default and supports destructive update of arbitrary data structures. Disciple includes *region typing*, *effect typing* and *closure typing*, and this extra information provides a handle on the operational behaviour of code that isn't available in other languages. Programs can be written in either pure-functional or effecful-imperative style, and one of our goals is to provide both styles coherently in the same language.


## Data Field Haskell

http://www.mrtc.mdh.se/projects/DFH/

Data Field Haskell implements an instance of `Data Fields`, a *generalization of arrays*. The purpose is to support generic array and data parallel programming on a very high level, for rapid prototyping of parallel algorithms. The most important language extension to Haskell is the forall-construct, which allows convenient definitions of data fields.


## Dependent Haskell

https://wiki.haskell.org/Dependent_type

Dependent Haskell is an ongoing research in empowering Haskell with proper dependent types.


---

## Chameleon
Chameleon is a Haskell-style language which provides a flexible overloading mechanism based on *Constraint Handling Rules* (CHRs), which the user can employ to impose conditions on overloaded identifiers.

## Gofer
Gofer is a small interpreter by Mark Jones supporting a language based on the Haskell report version 1.2. Gofer is intended as an experimental language, particularly where type classes are involved. Although Haskell has adopted a number of ideas from Gofer, the Gofer type class system is still more flexible than the Haskell one. Available for all Unix platforms including Linux, DOS, and Macs. Gofer is no longer developed and is suceeded by Hugs.

http://web.cecs.pdx.edu/~mpj/goferarc/index.html

## GPH
https://web.archive.org/web/20071120014352/http://www.cee.hw.ac.uk/~dsg/gph/
Glasgow Parallel Haskell (GPH) is an extension of Haskell for parallel programming. It adds just two new primitives to the language, namely, a form of parallel composition `par`, and sequential composition `seq`. With judicious use of `par` and `seq` it is possible to express how a program should be evaluated in parallel. `GranSim` is a simulator for the parallel execution of GPH programs. There is [A Gentle Introduction to GPH][GI]

[pH]: https://web.archive.org/web/20011101014708/http://abp.lcs.mit.edu/projects/ph/
[Papers1]: https://web.archive.org/web/20071121035053/http://www.cee.hw.ac.uk/~dsg/gph/papers
[Papers2]: https://web.archive.org/web/20071121035138/http://www.cee.hw.ac.uk/~dsg/gph/papers/index.html
[Algorithms]: https://web.archive.org/web/20071121035048/http://www.cee.hw.ac.uk/~dsg/gph/nofib
[AlgoCode]: https://web.archive.org/web/20071120014352/ftp://ftp.macs.hw.ac.uk/pub/gph/gum-4.06-test-prgs.tgz
[GranSim]: https://web.archive.org/web/20071123154109/http://www.dcs.gla.ac.uk/fp/software/gransim
[GI]: https://web.archive.org/web/20061010161435/http://www.macs.hw.ac.uk/~dsg/gph/docs/Gentle-GPH/gph-gentle-intro.html

## pH
The pH (parallel Haskell) language is a parallel, eagerly-evaluated variant of Haskell with syntactic provisions for loops, barriers, and `I-` and `M-` structure storage. The eager evaluation model of pH is similar to that of `Id`. The current version of the pH compiler shares a back end with the Id compiler, producing code for the `Monsoon` dataflow machine. The front end of the pH compiler is a modification of HBC.

## Goffin
A Haskell extension for parallel and distributed programming. M. T. Chakravarty's page for newer work on parallel extensions to GHC:
https://web.archive.org/web/20050131094711/http://www.cse.unsw.edu.au/~chak/

[Papers]: https://web.archive.org/web/20180725192337/http://www.cse.unsw.edu.au/~chak/papers/


## PolyP - a polytypic programming language
`PolyP` is a functional language made by taking a subset of Haskell and extending it with a construct that permits defining polytypic functions (a polytypic function is a function that is defined by induction on the structure of user-defined datatypes).

## O'Haskell
O'Haskell is an object-oriented extension (interpreter) to Haskell developed at Chalmers. O'Haskell conservatively adds two major features to the Haskell core:
- a monad of concurrent, state-encapsulating, reactive objects
- a type system with subtyping between records as well as datatypes

`O'Hugs` is the available implementation (a derivative of Hugs 1.3b)
