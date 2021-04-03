# Graph reduction

Programming is not only about writing correct programs, answered by *denotational semantics*, but also about writing fast ones that mind the time and space issues, particularly memory usage.

For that kind of analysis, we need to know how they're executed on a machine, which is described by *operational semantics*. However, the Haskell standard deliberately lacks this operational description, leaving implementations to choose their own strategy. So far, every implementation has followed the *execution model of lazy evaluation*.

The explanation of how is a Haskell program executed on a real machine might be beneficial as a foundation for analyzing time and space usage, so here we detail lazy evaluation and subsequently use this execution model to explain and exemplify the reasoning about time and memory complexity of Haskell programs.






## Refs

https://en.wikibooks.org/wiki/Haskell/Graph_reduction
