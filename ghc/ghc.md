# GHC

GHC is *de facto* Haskell compiler and interpreter as *GHCi*.

## GHC Releases

https://downloads.haskell.org/~ghc/latest/
https://downloads.haskell.org/~ghc/

- 2019-08-25 8.8.1 *latest*


# GHC 8.10.1

https://www.haskell.org/ghc/blog/20200324-ghc-8.10.1-released.html

GHC 8.10.1 brings a number of new features including:

- new *UnliftedNewtypes* extension allowing newtypes around unlifted types.
- new *StandaloneKindSignatures* extension allows users to give top-level kind signatures to type, type family, and class declarations.

- new warning, `-Wderiving-defaults` to draw attention to ambiguous deriving clauses

- number of improvements in code generation, including a new loop analyzer, optimisation of memset, memcpy and array allocation, more aggressive specialisation, and pointer tagging for larger data types.

- new GHCi command `:instances` for listing a class instances

- An upgraded Windows toolchain lifting the MAX_PATH limitation

- a new low-latency garbage collector

- Improved support profiling, including support for sending profiler samples to the eventlog, allowing correlation between the profile and other program events

A full accounting of the changes:
https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/8.10.1-notes.html


# Haskell Specification


## Haskell Specs
- First specs:  Haskell 98   Report in 1990 (30 years ago)
- Latest specs: Haskell 2010 Report in 2010 (10 years ago), FFI added
- Next specs:   Haskell 2020 Report is underway


## GHC Releases

- 2019-08-25 8.8.1 *latest*
- 2019-04-23 8.6.5
- 2019-03-05 8.6.4
- 2019-02-22 8.6.2
- 2019-02-17 8.6.3
- 2019-02-16 8.6.2
- 2019-02-15 8.6.3
- 2019-02-15 8.6.1
- 2019-02-15 8.4.4
- 2019-02-15 8.4.3
- 2019-02-15 8.4.2
- 2019-02-15 8.4.1
- 2019-02-15 8.2.2
- 2019-02-15 8.2.1
- 2019-02-15 8.0.2
- 2019-02-15 8.0.1

https://downloads.haskell.org/~ghc/latest/
https://downloads.haskell.org/~ghc/


### Haskell Interpreters
- GHC *de facto*
- Huggs
- Gofer

### Haskell Language Variations
- Gofer
- Curry
- Frege



## GHC steering committee

https://github.com/ghc-proposals/ghc-proposals


The current members with the GitHub handle, join/left date:

* Simon Peyton-Jones    @simonpj        2017-02             co-chair
* Simon Marlow          @simonmar       2017-02             co-chair
* Joachim Breitner      @nomeata        2017-02             secretary
* Richard Eisenberg     @goldfirere     2017-02
* Iavor Diatchki        @yav            2017-02
* Ryan Newton           @rrnewton       2017-02 - 2018-09   former member
* Roman Leshchinskiy    @rleshchinskiy  2017-02 - 2018-11   former member
* Ben Gamari            @bgamari        2017-02 - 2019-07   former member
* Manuel Chakravarty    @mchakravarty   2017-02 - 2019-07   former member
* Christopher Allen     @bitemyapp      2017-02 - 2020-05   former member
* Eric Seidel           @gridaphobe     2018-09
* Vitaly Bragilevsky    @bravit         2018-09
* Sandy Maguire         @isovector      2019-07 - 2019-12   former member
* Arnaud Spiwack        @aspiwack       2019-07
* Alejandro Serrano     @serras         2020-01
* Cale Gibbard          @cgibbard       2020-01
* Tom Harding           @i-am-tom       2020-01

https://github.com/simonpj


https://github.com/ghc-proposals/ghc-proposals/issues?q=label%3AImplemented
