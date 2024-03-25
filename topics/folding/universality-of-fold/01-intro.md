# Universality and expressiveness of fold
"A tutorial on the universality and expressiveness of fold" by Graham Hutton

## 1. Introduction

*Iteration* over a data structure (such as a list or an array) may be expressed imperatively, for example using a for-loop, but becomes more elegant and readable expressed with recursion. Recursion and mathematical induction are the same thing in a different domain, thus recursion naturally translates into mathematical induction in the formal proof of program's correctness. FP gives us means to abstract many programming patterns that are inconvenient or impossible to generalize using imperative paradigm. One of these reoccurring patterns - iteration over a data structure - may be abstracted using folding.

The `foldr` function, representative of the folding family of functions, is a recursion operator that abstracts recursion over data structures (in the text that follows, fold is mostly used as a synonym for foldr from Data.Foldable), and also comes endowed with the proof principle called universality.

*Universality* encapsulates a common pattern of inductive proofs, especially those concerning lists. The fold function, together with and its *universal property*, forms the basis of a calculational theory of list processing, that may be generalised to other data types. We'll focus on *the universal property of fold* together with the derived fusion property.

As *proof principles*, they let us elide inductive proofs. 
As *definition principles*, they guide the transformation of recursive functions into definitions that rely on fold.
