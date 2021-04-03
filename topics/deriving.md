# Deriving

https://kowainik.github.io/posts/deriving

https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/deriving.html

## Contents

Intro
Typeclasses and Instances
  Santa letters
  Haskell Reports
  Countable sets
Motivation
Deriving
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

## Intro



Derivable classes:
* Auto-Derivable classes by default:
  - Eq, Ord
  - Show, Read
  - Bounded, Enum
  - Num
