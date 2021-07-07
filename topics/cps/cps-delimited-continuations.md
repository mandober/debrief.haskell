# Delimited Continuations

The concept of continuations arises naturally in programming: a conditional branch selects a continuation from the two possible futures; raising an exception discards a part of the continuation; a tail-call or 'goto' continues with the continuation.

Although continuations are implicitly manipulated in every PL, by manipulating them explicitly as first-class objects we can implement co-routines and non-deterministic searches.

There are several approaches and naming schemes for the related control keywords when first-class delimited continuations are concerned, but the most popular choice seems to be using the control operators `shift` and `reset`.

## Introduction

Continuations represent the rest of the computation. Manipulation of continuations is a powerful tool to realize complex control flow of programs without spoiling the overall structure of programs. It is a generalization of exception handling, but is far more expressive.




## Delimited control in Haskell

Delimited control, like its instance, exceptions, is an effect, and therefore, we have to use monads, that is, the `Cont` monad from `mtl`, which is the monad for the *delimited* control.
