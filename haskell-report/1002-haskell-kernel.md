# 1.2 The Haskell Kernel

https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-110001.2

Haskell has adopted many of the convenient syntactic structures that have become popular in functional programming. The meaning of such syntactic forms is given by translating them into the simpler constructs. If these translations are applied exhaustively, the result is a program written in a small subset of Haskell that we call the **Haskell kernel** or the **Haskell Core**.

Although the Haskell kernel is not formally specified, it is essentially a slightly sugared variant of the lambda calculus with a straightforward denotational semantics.

The core is based on a slightly sugared and enhanced variant of the lambda calculus, perhaps more of a variant of System F, with a straightforward denotational semantics.

The translation of each syntactic structure into the kernel is given as the syntax is introduced. This modular design facilitates reasoning about Haskell programs and provides useful guidelines for implementors of the language.
