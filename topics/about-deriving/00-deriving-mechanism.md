# Deriving mechanisms

https://kowainik.github.io/posts/deriving

Contents
- type class
- instance declaration
- deriving mechanism
- auto-deriving for data or newtype declarations
- `deriving` keyword

Strategies
Standard deriving
Auto derived
Typeable
Coercible
HasField
Derive Whatever
Functor
Foldable
Traversable
Generic and Generic1
Data
Lift
Newtypes
Any class derivations
Generic anyclass
Exception anyclass
Anyclass ambiguity
Via
Standalone deriving
Empty Deriving
Best practices with Deriving
Meta boilerplate and possible future improvements
Summary
Quiz: Lock, Stock and Two Smoking Barrels
Training 1: Specify strategy
Training 2: Disambiguate
Puzzle 1: Semigroup and Monoid
Puzzle 2: Infinite deriving
Conclusion
Sources


The *deriving mechanism* is the compiler feature that automatically generates instances of one or more classes for a data type.

There are different ways of deriving, but the general idea is described by the Haskell 2010 language report: A *derived instance* is an *instance declaration* that is generated automatically in conjunction with a data or newtype declaration. The body of a derived instance declaration is derived syntactically from the definition of the associated type.
