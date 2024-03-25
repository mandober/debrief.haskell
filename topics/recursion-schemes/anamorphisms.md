# Anamorphisms

Anamorphism


Anamorphism is a (co)recursion scheme that builds up a possibly infinite (co)data structure from a given seed value.

Ana and cata are duals of each other. 

In terms of lists only, anamorhism is the `unfoldr` function in Haskell. 

Anamorphism is generally unbounded, consuming finite data and producing infinite codata structures using corecursion. Structures like streams are naturally defined in terms of anamorphisms.
