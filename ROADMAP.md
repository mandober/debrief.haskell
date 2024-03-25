# Haskell :: Roadmap

A roadmap to learn Haskell

1. Prerequisites
  - set theory
    - basics of set theory
    - Cartesian product
    - disjoint union
    - powerset
    - set family, family of sets
    - indexed set
  - relation theory
    - relational vs functional view
      - functional view: A + B = C, mappings, input and output
      - relational view: A B C are related in a particular way
        A B -> C, A and B uniquely determine C
        A C -> B, A and C uniquely determine B
        B C -> A, B and C uniquely determine A
        Prolog-like; a single equation defines several things
        _ + _ = _
        A + B = _
        A + _ = C
        _ + B = C
        _ + _ = C
        A + _ = _


  - order theory (Eq, Ord)
  - function theory
    - function type
    - arity
    - currying
    - combinators
    - lambda calculus
      - untyped lambda calculus, λ
      - simply-typed lambda calculus, λ→
      - λ², System-F, polymorphic LC
      - λ²
      - λ²
  - type theory
    - type systems
  - abstract algebra
    - algebraic structures
      - group-like algebraic structures (Semigroup, Monoid)
  - TOC
  - TPL
    - type
      - type declaration
      - type constructor
      - data/value constructor
      - algebraic data type (ADT)
        - sum types
          - disjoint union
          - tagged union
      - generelized algebraic data type (GADT)
      - recursive data types
    - polymorphism
      - parametric polymorphism
      - ad hoc polymorphism

2. Fundamental Haskell concepts
  - function
  - purity
  - pure functions
  - closure, anonymous function, lambda function
