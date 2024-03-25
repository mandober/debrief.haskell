# Monad Transformers

* Monad transformers are data types that allow us to roll two monads into one that incorporates the behaviors of both.

* We can stack up diff monads by nesting one monad in another.
* Such a stack of monads has a base monad as the most deeply nested one.
* While the stacking order is arbitrary, the logistics of it all usually drives the stacking towards an optimal order.
* However, the `IO` monad, if used, must always be the base monad.
* On the outside is the outermost monad, in terms of which the entire monad stack is typed. All references fall to the outmost monad.
* To address a nested monad, an operation gets `lift`ed into the stack.
* In fact, `lift` only elevates unary functions, `lift2` is for binary functions, etc., up to `lift5`.
* `liftIO`


## Rationale

Each monad is usually specialized for a particular tasks. The `Maybe` monad is used to signal computation failure, the `Either` monad can do the same while also providing a detailed error message, the list monad represents nondeterministic computation by returning multiple results, the `IO` monad hosts effectful external communications, the `Reader` monad may be used to provide access to a shared environment, the `Writer` monad can accumulate and log the outputs, the `State` monad can model stateful computations, the `Cont` monad is the mother monad since it can represent other monads, and with that, almost every aspect of computation.


Particular monads help in handling IO, Maybe, lists and state by providing a common way to use these useful general-purpose tools, but it would be useful if we could use the capabilities of different monads at the same time.

For instance, a function might want to use both `IO` and `Maybe` exception handling. While a type like `IO (Maybe a)` would work suffice, it would force us to do pattern matching within `IO`'s `do`-blocks to extract values, which is something that `Maybe` monad was meant to aliviate.
