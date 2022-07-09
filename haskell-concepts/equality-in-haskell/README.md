# Equality in Haskell

* Notions of equality
  * Symbols for equality
    - `=`
    - class `Eq`
      - comparison operator, `==`
    - class `~`
    - class `~~`
    - type `:~:`
    - type `:~~:`
    - eqPrimTyCon,      `ty1 ~#  ty2`
    - eqReprPrimTyCon,  `ty1 ~R# ty2`  (at role Representational)
    - eqPhantPrimTyCon, `ty1 ~P# ty2`  (at role Phantom)
    - equalityTyCon
  * Sorts of equality
    - propositional equality
    - definitional equality
    - Martin-Löf identity
    - Leibniz equality
    - substitutional equality
    - intensional equality
    - extensional equality
    - equivalence relation
      - reflexivity
      - symmetry
      - transitivity
      - connex (totality)
    - congruence relation
    - isomorphism
    - syntactic equality
    - semantic equality
    - observational equality
    - value equality
      - equality of primitives
      - equality of compounds
      - class `Eq` of types that can be compared for equality
      - using comparison operator, `==`
      - equality of functions
        - undecidable in general
          - requires comparing tuples of `(A,f,B)` that define functions:
            so not only comparing two entire graphs (sets of ordered pairs)
            but also their domains and their codomains. Infeasible.
        - decidable only in very special cases
        - extensional equality of total functions on compact spaces possible
    - type equality
      - equality of type functions
        - decidable since they are very special
      - lifted homogeneous equality, `(~)`    (equal types)
      - lifted heterogeneous equality, `(~~)` (equal types, equal kinds)
    - kind equality
