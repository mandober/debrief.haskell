# Monads vs arrows

Arrows, like monads, express computations that happen within a context, but arrows are a more general abstraction than monads. Arrows allow for contexts beyond those allowed by Monad class, e.g. a computation that may be partially static (independent of the input), a computation that may take multiple inputs.

Arrows, defined by John Hughes, are an abstract view of computation, serving the similar purpose as monads in that they also provide a common structure for libraries.

> The essential difference between these two abstractions:

Just like a monadic type `m a` 
represents 
  a computation delivering an `a`,

an arrow type `a b c` 
  (i.e. the *application* of 
  the parameterised type `a` 
  to the two parameters `b` and `c`, 
represents 
  a computation with 
  the input of type `b` 
  delivering a `c` (functions!).


Arrows make this dependence on input explicit.
