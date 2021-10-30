# Type Classes

type class, class
class declaration
class' required functions
minimal set of required functions
default implementations
class functions' signatures
class constraints, subclass, superclass
instance declaration
signatures in instance declaration block
pragma `{-# LANGUAGE InstanceSigs #-}`
redefining default implementations
class' type parameter, `Functor f`
matching instance type params
fixing excessive type params of an instance
additional constraints of the instance type param


# Laws

Axioms applicable to data types, type classes, etc.

Axioms:
- Totality [TOT] or Closure [CLO]
- Associativity             [ASS]
- Identity                  [ID]
  - Left identity             [IdL]
  - Right identity            [IdR]
  - Total identity            [ID]
  - identity element, ϵ: $$\epsilon$$
- Invertability             [INV]     (Inverse, Reversibility)
  - inverse element: $$x^{-1}$$
- Commutativity             [COM]
- Distributivity            [DIS]
  - Left distributivity       [DiL]
  - Right distributivity      [DiR]


Data types:
- numbers: arithetic ops
- functions: composition, identity
- Boolean: boolean algebra
- unit
- list
- Maybe
- Either

Axioms of classes:
- Magma:              TOT
- Semigroup:          TOT, ASS
- Monoid:             TOT, ASS, ID
- Group¹:           TOT, ASS, ID, INV
- Abelian Group¹:   TOT, ASS, ID, INV, COM
- Field¹: 2 binary ops, TOT, ASS, ID, INV, COM
- Ring¹: 4  binary ops, TOT, ASS, ID, INV, COM
- Foldable
- Functor
- Applicative
- Monad
- Apply[^2]
- Comonad[^3]
- Groupoid


¹ not in Haskell's std

[^2]: `Apply` can be found in the *semigroupoids* package
[^3]: `Comonad` in the *comonad* package.
