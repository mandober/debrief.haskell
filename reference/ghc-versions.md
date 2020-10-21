# GHC versions

All downloads:
https://downloads.haskell.org/ghc/

Latest GHC (always pointing to the latest GHC version)
https://downloads.haskell.org/ghc/latest/

Edison - Chris Okasaki's Efficient Data Strucutres - User manual
https://downloads.haskell.org/ghc/old_docs/edison/

GHC Documentation
https://ghc.gitlab.haskell.org/ghc/doc/

- [GHC Home Page](http://www.haskell.org/ghc/)
- [GHC User Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/index.html)
- [GHC  Libraries](https://ghc.gitlab.haskell.org/ghc/doc/libraries/index.html)
- [GHC        API](https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-8.11.0.20200827/index.html)
- [GHC Devel Home](https://gitlab.haskell.org/ghc/ghc/wikis/)


GHC @gitlab compiler
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler

GHC @gitlab compiler repo
https://gitlab.haskell.org/ghc/ghc/-/tree/master/compiler

GHC @gitlab Wiki
https://gitlab.haskell.org/ghc/ghc/wikis/index

GHC User Guide 8.11.0.20200827 (@readthedocs)
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/index.html

GHC User Guide 9.01 (@readthedocs)
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/index.html

Linear Types GHC proposal
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst


## Papers

https://downloads.haskell.org/ghc/papers/

https://downloads.haskell.org/ghc/papers/arrow-rules.pdf                        88,837
https://downloads.haskell.org/ghc/papers/classhask.ps.gz                        56,314
https://downloads.haskell.org/ghc/papers/composing-haggis.ps.gz                 39,020
https://downloads.haskell.org/ghc/papers/concurrent-haskell.ps.gz               54,664
https://downloads.haskell.org/ghc/papers/core-6.10.ps.gz                       143,918
https://downloads.haskell.org/ghc/papers/core.ps.gz                             87,299
https://downloads.haskell.org/ghc/papers/except_ps.gz                           84,321
https://downloads.haskell.org/ghc/papers/extendGHC.ps.gz                        52,755
https://downloads.haskell.org/ghc/papers/extendGHC_ps.ps.gz                     52,755
https://downloads.haskell.org/ghc/papers/grasp-jfit.ps.gz                       53,591
https://downloads.haskell.org/ghc/papers/hep.ps.gz                              73,872
https://downloads.haskell.org/ghc/papers/imperative.ps.gz                       60,797
https://downloads.haskell.org/ghc/papers/lazy-functional-state-threads.ps.gz    79,932
https://downloads.haskell.org/ghc/papers/new-rts.ps.gz                          96,993
https://downloads.haskell.org/ghc/papers/nofib.ps.gz                            33,324
https://downloads.haskell.org/ghc/papers/profiling.ps.gz                        91,497
https://downloads.haskell.org/ghc/papers/rts.ps.gz                              92,986
https://downloads.haskell.org/ghc/papers/run-time-system.ps.gz                 570,535
https://downloads.haskell.org/ghc/papers/spineless-tagless-gmachine.ps.gz      174,025
https://downloads.haskell.org/ghc/papers/th2.ps                                 69,337
https://downloads.haskell.org/ghc/papers/threads.ps.gz                          54,930
https://downloads.haskell.org/ghc/papers/unboxed-values.ps.gz                   76,881



## Version 8.x.x

* 9.0.1-alpha1/
  - https://downloads.haskell.org/ghc/9.0.1-alpha1/
  - https://downloads.haskell.org/ghc/9.0.1-alpha1/docs/html/
  - UG @rtd https://ghc.gitlab.haskell.org/ghc/doc/users_guide/index.html
* 8.10.x
  - 8.10-latest/ --> 8.10.1 (as of 17.Oct 2020)
  - 8.10.2/
    - https://downloads.haskell.org/ghc/8.10.2/
    - https://downloads.haskell.org/ghc/8.10.2/docs/html/
  - 8.10.1/
    - https://downloads.haskell.org/ghc/8.10.1/
    - https://downloads.haskell.org/ghc/8.10.1/docs/html/
* 8.8-latest/
  - 8.8.4/
  - 8.8.3/
  - 8.8.2/
  - 8.8.1/
* 8.6-latest/
  - 8.6.5/
  - 8.6.4/
  - 8.6.3/
  - 8.6.2/
  - 8.6.1/
* 8.4-latest/
  - 8.4.4/
  - 8.4.3/
  - 8.4.2/
  - 8.4.1/
* 8.2-latest/
  - 8.2.2/
  - 8.2.1/
* 8.0-latest/
  - 8.0.2/
  - 8.0.1/


## GHC on Windows
GHC on Windows needs MSYS2 with these packages installed:
http://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-{PACKAGE}

Packages
  - binutils-2.27-2
  - gmp-6.1.1-1
  - gcc-6.2.0-2
  - gcc-libs-6.2.0-2
  - isl-0.17.1-1
  - mpc-1.0.3-2
  - mpfr-3.1.4.p3-4
  - zlib-1.2.8-9
  - crt-git-5.0.0.4745.d2384c2-1
  - headers-git-5.0.0.4747.0f8f626-1
  - winpthreads-git-5.0.0.4741.2c8939a-1
  - libwinpthread-git-5.0.0.4741.2c8939a-1
  - libidn-1.32-3

Libraries
* isl
  - Manipulating sets and relations of int points bounded by linear constraints
* mpc
  - Multi precision complex library
  - http://www.multiprecision.org/mpc/
* mpfr
  - Multiple-precision floating-point library
  - http://www.mpfr.org
* gmp


### ISL

- [Homepage](http://isl.gforge.inria.fr/)
- [User Manual Online](http://isl.gforge.inria.fr/user.html)
- [User Manual PDF](http://isl.gforge.inria.fr/manual.pdf)
- [ISL git repo](http://repo.or.cz/w/isl.git)

Integer Set Library (ISL) is a library for manipulating sets and relations of integer points bounded by linear constraints.

Supported operations on sets include intersection, union, set difference, emptiness check, convex hull, (integer) affine hull, integer projection, computing the lexicographic minimum using parametric integer programming, coalescing and parametric vertex enumeration.

It also includes an ILP solver based on generalized basis reduction, transitive closures on maps (which may encode infinite graphs), dependence analysis and bounds on piecewise step-polynomials.

For an introduction to the underlying concepts, see *Presburger Formulas and Polyhedral Compilation*. The *barvinok package* contains an ISCC calculator that exposes some of the operations supported by ISL.

- [barvinok package](http://barvinok.gforge.inria.fr/)
- [Presburger Formulas and Polyhedral Compilation](https://lirias.kuleuven.be/handle/123456789/523109)


---

http://isl.gforge.inria.fr/
http://www.multiprecision.org/mpc/
https://www.mpfr.org/

---

https://gitlab.haskell.org/ghc/ghc/-/wikis/home
https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/coding-style

https://downloads.haskell.org/ghc/9.0.1-alpha1/
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/9.0.1-notes.html
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst
https://ghc.gitlab.haskell.org/ghc/doc/libraries/index.html

https://downloads.haskell.org/ghc/8.10.2/docs/
https://downloads.haskell.org/ghc/8.10.2/docs/html/

https://www.haskell.org/ghc/
https://www.haskell.org/onlinereport/haskell2010/haskellch20.html
https://en.wikibooks.org/wiki/Haskell/ParseExps
https://downloads.haskell.org/ghc/old_docs/edison/users002.html
http://book.realworldhaskell.org/read/data-structures.html#id634459

STM
http://hackage.haskell.org/package/stm

Tree annotation
http://okmij.org/ftp/Haskell/AlgorithmsH.html



https://iohk.io/
