# Dependent Types in Haskell - Theory and Practice
Richard A. Eisenberg (thesis), Stephanie Weirich, 2016

The complete source code for this document is available from
http://github.com/goldfirere/thesis

## Abstract

Haskell, as implemented in the GHC has been adding new type-level programming features for some time. Many of these features (GADTs, type families, kind polymorphism, promoted datatypes) have brought Haskell to the doorstep of dependent types. Many dependently typed programs can even currently be encoded, but often the constructions are painful.

In this dissertation, I describe *Dependent Haskell*, which supports full dependent types via a backward-compatible extension to today's Haskell. An important contribution of this work is an implementation, in GHC, of a portion of Dependent Haskell, with the rest to follow. The features I have implemented are already released, in GHC 8.0.

This dissertation contains several practical examples of Dependent Haskell code, a full description of the differences between Dependent Haskell and today's Haskell, a novel dependently typed lambda-calculus, called `Pico`, suitable for use as an intermediate language for compiling Dependent Haskell, and a type inference and elaboration algorithm, `Bake`, that translates Dependent Haskell to type-correct Pico. Full proofs of type safety of Pico and the soundness of Bake are included in the appendix.
