# Constraints
(Thinking with types)

The `Constraint` *kind* is reserved for things that can appear on the left side of the fat context arrow (`=>`). This includes fully-saturated typeclasses (like `Show a`), tuples of other constraints, and type equalities (`Int ~ a`), which are enabled with `GADTs` pragma.
