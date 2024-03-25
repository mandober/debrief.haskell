# Index of monads

Monads are a convenient way to to sequence computation with effects. Different monads can provide different kinds of effects:
- `Identity`  nop monad, no side effects allowed
- `IO`        input/output communication, world-changing side effects
- `Reader`    access to a readonly environment
- `State`     models a mutable state
- `Maybe`     models failure and allows for early exit
- `Either`    models failure with errors, allows for early exit
- `List`      models nondeterministic computations allowing multiple results
- `RWS`       compound monad that encompasses Reader, Writer and State monads
- `Cont`      models continuations (the mother monad)
- `Select`    selection monad
- `Logic`     backtracking search
