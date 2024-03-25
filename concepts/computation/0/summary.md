# Effectful computation :: Summary

Some side effects are external and must be dealt with the `IO` monad, but there is a class of side effects around the notion of a *global state* that need not use such a heavy machinery. It turns out that we can model these *"internal" side effects* using nothing but pure functions.

## TL/DR

- side effects:
  - external -> must use IO
  - internal -> may be modeled purely
- internal side effects:
  - boil down to a global value
  - declaring a global variable that holds a mutable value
- internal side effects include:
  - global configuration, *readonly environment*
  - *global log*: appending individual entries to a log
  - *global mutable state*
- required permissions to access a global value (and name of monad):
  - environment: read-only,  `+r`  (Reader)
  - logging:     write-only, `+w`  (Writer)
  - state:       read-write, `+rw` (State)
- modeling internal side effects:
  - all these can be modelled in terms of a global state:
    - env: read-only global state
    - log: write-only global state
- modeling global state:
  - altering functions by adding:
    - extra input parameter
    - extra return value
