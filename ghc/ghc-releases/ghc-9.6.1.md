# GHC :: Release notes :: Version 9.6.1

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/9.6.1-notes.html

## Language

- Record updates for GADTs and other existential datatypes are now fully supported.

- Error messages are now assigned unique error codes, of the form [GHC-12345].

- GHC Proposal #106 has been implemented, introducing a **new language extension** __`TypeData`__. This extension permits type data declarations as a more fine-grained alternative to `DataKinds`.
  - Define Kinds Without Promotion:
    https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst
    - author: Iavor Diatchki
    - date-accepted: 2018-09-12
    - implemented: 9.6
    - ticket-url: https://gitlab.haskell.org/ghc/ghc/-/issues/6024

- GHC now does a better job of solving constraints in the presence of multiple matching quantified constraints.

- GHC proposal #170 Unrestricted __`OverloadedLabels`__ has been implemented.
  - Unrestricted Overloaded Labels:
    https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0170-unrestricted-overloadedlabels.rst
    - author: howtonotwin
    - date-accepted: 2018-11-05
    - implemented: GHC 9.6
    - ticket-url: https://gitlab.haskell.org/ghc/ghc/issues/11671


## Compiler

- The `TypeInType` is now marked as deprecated. Its meaning has been included in `PolyKinds` and `DataKinds`.

- The `-Woperator-whitespace` warning no longer ignores constructor symbols (operators starting with `:`).

## GHCi

GHCi will now accept any file-header pragmas it finds, such as 
`{-# OPTIONS_GHC ... #-}` and `{-# LANGUAGE ... #-}`.

For example, instead of using `:set` to enable `-Wmissing-signatures`, you could instead write:

    ghci> {-# OPTIONS_GHC -Wmissing-signatures #-}

This can be convenient when pasting large multi-line blocks of code into GHCi.

## Runtime system

- The Delimited continuation primops proposal has been implemented, adding native support for first-class, delimited continuations to the RTS. For the reasons given in the proposal, no safe API to access this functionality is provided anywhere in base. Instead, the prompt# and control0# primops are intended to be consumed by library authors directly, who may wrap them a safe API that maintains the necessary invariants. See the documentation in GHC.Prim for more details.

- The behaviour of the `-M` flag has been made more strict. It will now trigger a heap overflow if the total amount of memory used by the Haskell heap exceeds the limit. Previously only live blocks were taken into account. This makes it more likely to trigger promptly when the heap is highly fragmented.

## base library

- Exceptions thrown by weak pointer finalizers are now caught and reported via a global exception handler. By default this handler reports the error to stderr although this can be changed using 
`GHC.Weak.Finalize.setFinalizerExceptionHandler`.

- GHC now provides a set of operations for introspecting on the threads of a program, `GHC.Conc.listThreads`, as well as operations for querying a thread's label (`GHC.Conc.threadLabel`) and status (`GHC.Conc.threadStatus`).

- Change default `Ord` implementation of `>=`, `>`, and `<`, to use __`<=`__ instead of `compare`, per CLC proposal:

  * Change Ord method defaults #24
  https://github.com/haskell/core-libraries-committee/issues/24

- Updated to Unicode 15.0.0.
  Unicode 15.0.0. released 13-Sep 2022
  https://www.unicode.org/versions/Unicode15.0.0/

  Unicode 15.0 adds 4,489 characters, for a total of 149,186 characters. These additions include 2 new scripts, for a total of 161 scripts, along with 20 new emoji characters, and 4,193 CJK (Chinese, Japanese, and Korean) ideographs.
  https://unicode.org/emoji/charts-15.0/emoji-released.html
  https://www.unicode.org/emoji/charts-15.0/emoji-counts.html

- Add standard Unicode case predicates 
__`Data.Char.isUpperCase`__ and __`Data.Char.isLowerCase`__. 
These predicates use the standard Unicode case properties and are more intuitive than `Data.Char.isUpper` and `Data.Char.isLower`.


## Included libraries

The package database provided with this distribution also contains a number of packages other than GHC itself. See the changelogs provided with these packages for further change information.

Package	Version	Reason for inclusion
ghc	9.7	The compiler itself
Cabal-syntax	3.9.0.0	Dependency of ghc-pkg utility
Cabal	3.9.0.0	Dependency of ghc-pkg utility
Win32	2.13.3.0	Dependency of ghc library
array	0.5.4.0	Dependency of ghc library
base	4.18.0.0	Core library
binary	0.8.9.1	Dependency of ghc library
bytestring	0.11.3.1	Dependency of ghc library
containers	0.6.6	Dependency of ghc library
deepseq	1.4.8.0	Dependency of ghc library
directory	1.3.8.0	Dependency of ghc library
exceptions	0.10.5	Dependency of ghc and haskeline library
filepath	1.4.100.0	Dependency of ghc library
ghc-boot-th	9.7	Internal compiler library
ghc-boot	9.7	Internal compiler library
ghc-compact	0.1.0.0	Core library
ghc-heap	9.7	GHC heap-walking library
ghc-prim	0.10.0	Core library
ghci	9.7	The REPL interface
haskeline	0.8.2	Dependency of ghci executable
hpc	0.6.2.0	Dependency of hpc executable
integer-gmp	1.1	Core library
libiserv	9.7	Internal compiler library
mtl	2.3.1	Dependency of Cabal library
parsec	3.1.15.1	Dependency of Cabal library
pretty	1.1.3.6	Dependency of ghc library
process	1.6.16.0	Dependency of ghc library
stm	2.5.1.0	Dependency of haskeline library
template-haskell	2.19.0.0	Core library
terminfo	0.4.1.5	Dependency of haskeline library
text	2.0.1	Dependency of Cabal library
time	1.12.2	Dependency of ghc library
transformers	0.6.0.4	Dependency of ghc library
unix	2.8.0.0	Dependency of ghc library
xhtml	3000.2.2.1	Dependency of haddock executable
