## Introduction

A routinely dispensed advice, related to programming in Haskell, is to approach a problem by first modelling the types, because once the types are defined, you can rely on them to drive the development further.

If this is the case at the project level, it should be the case at the level of a single function. With a function's signature determined, the correct implementation shouldn't be too hard to write.

This is indeed the case with the signatures of parametrically polymorphic functions, whose implementations are practically writing themselves.
