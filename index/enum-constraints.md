# List of constraints

Constraints
- empty constraint
  - e.g. `() => a`
- type class
  - e.g. `(Num a) => a`
  - e.g. `Show a => a`
- type classes
  - e.g. `(Foldable t, Traversable t, Num a) => t a`
  - e.g. `Foldable t => Traversable t => Num a => t a`
- type function
  - type equality, e.g. `(a ~ Int) => a`
  - type operator, e.g. `(a >= (6 :: Nat)) => a`
- quantified constraint
  - e.g. `(forall a. Show a) => forall a. a -> a`
- mix of the above
  - e.g. `(a ~ Int, Show a) => a`
