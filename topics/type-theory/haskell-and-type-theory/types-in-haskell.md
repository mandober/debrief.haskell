# Types in Haskell

This is about types in Haskell with a lean toward type theory and the similarities between types as found in type theory as compared to the Haskell's types.

All Haskell types can be described as a form of *Algebraic Data Types* (ADTs). At least this was once the case, until the generalization of ADTs was introduced, known as GADTs or *General Algebraic Data Types*. In Haskell today, both kinds are available - ADTs by default and GADTs possibly behind a flag (in Haskell98, Haskell2010, but not in the GHC2010 pseudo standard). The two are normally distinguishable by their syntax (although there are ways to express a GADTs using ADT syntax as well). Languages based/written in Haskell (Agda, Idris) only provide GADTs (and GADT syntax) since GADTs subsume ADTs as the more general types.
