# Haskell Terminology and Definitions


## Application of functions
Application is normally associated with functions (in their various flavors and shapes). Applying a function means invoking a function, while at the same time, passing to it, at least one arg. Specifying only a bare function name without passing it any args, means referencing that function, e.g. to pass it off as an arg to a HOF (although this is still the case when some, but not all, args are supplied, which passes a new function obtained from that partial application).

## Functions, operationally
Operationally, it is the substitution operation in which each supplied argument is bound to the corresponding formal parameter, then all the formal parameter's application occurrences are consistently replaced with their binding occurrence.

## Functions, elements
Teminology of function elements:

- function (type) signature
- function head(er)
- function body
- nested function
- subfunction
- function un/currying
- formal parameters
  - type parameter
    - concrete
    - ∀
    - ∃
    - skolem
    - rigid
    - forbidden type application (surrounded in braces)
    - with accompanying kind signature/annotation
  - value parameter
- parameter occurrences
  - binding occurrence (single)
  - application occurrences (none to many)
- arguments
  - type args (type-level type applications/instantiations)
  - value args (term-level expressions)
- function application
  - type application (type-level)
  - type instantiation
  - application of (term-level) args
- input value/type
- output value/type
