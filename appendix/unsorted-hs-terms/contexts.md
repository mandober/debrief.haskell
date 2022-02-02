# Contexts

*A value in a context* is a phrase often used to describe types whose most general form is `m b`. The type variable `m` represents a *context*, and the type variable `b` is the type of the value within the context `m`.

Sintactically, `m` is a type constructor. Semantically, `m` represents a context that is, by convention, associated with a particular data type.

For example, lists are values of type `a` in the context `[]`, that is, `[] a` or `[a]`. Lists, and therefore the list type ctor, `[]`, are often associated with non-deterministic computations, being a suitable type to express the possibility of a computation to return multiple, or even all possible, results.

Conventional associations:
○ `Maybe`  : simple failure, fallible computational context
○ `Either` : exceptions
○ `ListT`  : indeterministic computational context, non-determinism, search
○ `StateT` : statful computations
○ `ReaderT`: readonly environment, configuration
○ `WriterT`: output, logging, tracing
○ `ContT`  : continuations, cooperative concurrency

○ `a -> a` generally: `a -> b`
○ `a -> m b` Kleisli arrows
