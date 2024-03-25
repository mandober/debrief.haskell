# Recursion schemes

- [Fixed points](./fixpoints.md)
- [Functor fixed points](./functor-fixpoints)
- [Algebra and coalgebra](./algebra-and-coalgebra.md)

Papers
- [FP with Bananas, Lenses, Envelopes and Barbed Wire](./bananas-lenses-envelopes-barbedwire.md) by Erik Meijer, Maarten Fokkinga, Ross Paterson, 1991
- [Bird-Meertens formalism](./bird-meertens-formalism.md)
- [Lectures on Constructive FP](./lectures-on-constructive-fp.md)
- [Monoidal catamorphisms](./monoidal-catamorphisms.md)
- [Recursion Schemes](./recursion-schemes-by-Traian_Florin_Serbanuta.md) by Traian Florin Serbanuta
- [Recursion Schemes](./recursion-schemes.md) by Patrick Thomson


## Duals

data                              codata
recursion                         corecursion
induction                         coinduction
algebra                           coalgebra
F-algebra                         F-coalgebra
category of F-algebras            category of F-coalgebras 
initial algebra                   final coalgebra
least fixpoint                    greatest fixpoint
natural numbers                   conatural numbers
iteration                         coiteration
primitive recursion               primitive corecursion
course-of-value iteration         course-of-value coiteration
course-of-value recursion         course-of-value corecursion


## Terms

- recursion
  - data
  - algebra
  - induction
- corecursion
  - codata
  - coalgebra
  - coinduction
- recursion schemes - species of morphisms
  - Folds
    - cata      tears down a structure level by level
    - para      tears down a structure with primitive recursion
    - zygo      tears down a structure with the aid of a helper function
    - prepro    tears down a structure after repeatedly transforming it
    - histo     tears down a structure with the aid of its previous answers; 
                course-of-value recursion
  - Unfolds
    - ana       builds up a structure level by level
    - apo       builds up a structure opting to return a single level 
                or an entire branch at each point
    - futu      builds up a structure multiple levels at a time
    - postpro   builds up a structure and repeatedly transforms it
  - Refolds
    - hylo      builds up and tears down a virtual structure
    - chrono    builds up a virtual structure with a futumorphism 
                and tears it down with a histomorphism
    - synchro   a high level transformation between data structures using a 
                third data structure to queue intermediate results
    - exo       a high level transformation between data structures 
                from a trialgebra to a bialgebra
    - meta      (Erwig) A hylomorphism expressed in terms of bialgebras
    - meta      (Gibbons) A fold followed by an unfold; change of representation
    - dyna      builds up a virtual structure with ana and tears it down 
                with histo; captures dynamic programming
    - elgot     (Elgot algebra) builds up a structure and tears it down but 
                may shortcircuit the process during construction
    - elgot     (Elgot coalgebra) builds up a structure and tears it down but 
                may shortcircuit the process during deconstruction
    - mutu      mutual recursion
    - hoist
    - lambek
    - prepro
  - Combinations of recursion schemas
    - zygo-histo-morphic prepromorphism
- (co)recursion and (co)induction
  - types of recursion
    - iterative functions, iteration
    - fold, folding, fold operator
    - primitive recursion
    - primitively recursive functions (PRFs)
    - general primitive recursion
    - general primitively recursive functions (GRFs)
    - μ operator
    - direct recursion
    - mutual recursion
    - course-of-values induction scheme
  - types of (co)recursion
    - course-of-values corecursion scheme
    - iterative functions on inductive types
    - recursive functions on inductive types
    - representation of iterative and recursive functions on inductive types
    - course-of-values coiteration
    - primitive corecursion scheme
    - true coinduction (bisimilarity ⇒ equality)
    - bisimilarity
    - indexed corecursion
- Algebras
  - algebra
  - coalgebra
  - bialgebra
  - elgot algebras
