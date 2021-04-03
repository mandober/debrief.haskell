# Universality and expressiveness of fold

*A tutorial on the universality and expressiveness of fold* - Graham Hutton

Iteration over a data structure, such as list or array, may be expressed imperatively (using, e.g. the for loop), but becomes elegant and more readable when expressed with recursion. Recursive expressions naturally translate to mathematical induction in the formal proof of programs' correctness. Recursion and math induction are, after all, the same thing in different domains.

FP gave us means to abstract many programming patterns that were inconvenient or impossible to generalize using the imperative paradigm. One of these reoccurring patterns, iteration over a data structure (a collection), may be abstracted using the concept of *folding*.

The `fold` function, more precisely `foldr` function, is the *recursion operator* that encapsulates the reoccurring recursive patterns during list processing. The *fold recursion operator* also comes equipped with a proof principle of *universality* that encapsulates a common pattern of inductive proof regarding lists. The fold and its *universal property* together form the basis of a simple but powerful calculational theory of programs that process lists. This theory may be further generalised, from lists to a variety of other data types.

Here, we focus on the use of *the universal property of fold*, together with the derived *fusion property*, both as proof principles, that avoid the need for inductive proofs, and as a definition principle, that guides the transformation of recursive functions into definitions using fold.
