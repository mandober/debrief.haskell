## Chasing types in Haskell


## Introduction

There's a common situation in Haskell programming arising when you're about to write a definition for a function, having already worked out its signature.


There's a common situation arising in Haskell programming when you're about to write a definition for a function, having already worked out its signature.





Introduction
There's a common situation arising in Haskell programming when you're about to write a definition for a function, having already worked out its signature.
Type chasing (or foozling according to connoisseurs) is a particular situation that often arises when performing the Haskell katas, the undepletable source of which is the standard library. The approach is browsing the official API, finding interesting types and then writing out the implementation for oneself, as an exercise.
You avoid to peek at the source code that contains the correct, nonetheless official, solution, because it's comforting to that a solution does exist, and it is readily-available just a click away (rescue me before I fall into despair, ooh).
Trying to come up with a solution to a, usually polymorphic, function's definition in Haskell. Having worked out a function's signature, all that remains is to write the function's implementation driven by those types. But instead, you're driven to tears.















## Type fuzzle

- search Hackage for `mtl` package
- locate the docs about the `ContT` monad transformer
- consult the docs about its `Applicative` instance
- copy the signature
- now try to implement it yourself
- relying on the types from the signature for guidance


Knowing (or maybe not) that a valid implementation exists, and moreover, that the implementation uses nothing strange (like summing an unexpected force that appears out of nowhere and solves the whole thing, i.e. deusex function, that is, dualsexyeahparty) is to be called *fuzzling*
