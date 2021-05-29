# Curry-Howard correspondence between logic and Haskell

* The Curry-Howard Isomorphism
  - propositions-as-types, proofs-as-programs
  - simply typed lambda calculus
  - natural deduction
  - proof derivation
  - Fitch-style diagrams
  * intuitionistic logic
    - sequent
    - proposition, formula,
    - assumption, hypothesis, conclusion, consequence, antecedent, consequent
  * Rules of inference: formations
    - introduction rules
    - elimination rules
  * Rules of inference: connectives
    - conjunction
    - disjunction
    - implication

* Functions in Haskell
  - parametric polymorphism
  - type variables
  - function's definition
    - function's signature
    - function's implementation
  - function arguments
  - binding arguments
    - with 'proper' parameters on the LHS
    - with lambda on the RHS
    - equality of proper and lambda arg binding
* "The Triplet Squad" (often encountered types)
  - Reader, Writer, State, RWS, Exception, IO, Cont, etc.
  - "The Triplets" (type variants that come in threes):
    - as bare function type, e.g. `(a -> r) -> r`
    - as newtype wrapper, e.g. `Cont`
    - as MT, e.g. `ContT`
- FAM (Functor, Applicative, Monad) instances
- continuation monad transformer, `ContT`
* newtypes
  - type vs newtype vs data
  - working with newtype wrapper
  - un/re/wrapping
* Function type
  - function type ctor
  - partially applied function type ctor
  - Reader, `(->) r`
  - Reader, `r -> a` is covariant, `a -> r` is contravariant
  - associativity of function type ctor
  - right-associativity and redundant parens
  - newtype: only single-fielded data ctor
  - data ctor in the way of LHS arg binding, must use lambda
