# Monads

("All About Monads" guide)

A monad is a way to structure computations in terms of values and sequences of computations using those values. Monads allow the programmer to build up computations using sequential building blocks, which can themselves be sequences of computations. The monad determines how combined computations form a new computation and frees the programmer from having to code the combination manually each time it is required. It is useful to think of a monad as a strategy for combining computations into more complex computations.

For example, the type `Maybe` represents the type of computations which may fail to deliver a result. The Maybe type suggests a strategy for combining computations which return Maybe values: if the combined computation consists of the computation `B` that depends on the result of the computation `A`, then the combined computation should fail if either A or B fails, and it should fail immediately (no point in wasting time to compute B if A has already failed). If `A` succeeds its result is forwarded to `B`, and if `B` succeeds then the overall computation succeeds as well.

Other monads exist for building computations that perform I/O, interact with some state, even computations that return multiple results. There are as many different type of monads as there are strategies for combining computations, but there are certain monads that are especially useful and are common enough.

For programmers, monads are useful tools for structuring functional programs. They have 3 properties that make them especially useful:
* *Modularity*: they allow computations to be composed from simpler computations and separate the combination strategy from the actual computations being performed.
* *Flexibility*: they allow functional programs to be much more adaptable than equivalent programs written without monads. This is because the monad distills the computational strategy into a single place instead of requiring it be distributed throughout the entire program.
* *Isolation*: they can be used to create imperative-looking computational structures which remain safely isolated from the main body of the functional program. This is useful for incorporating side-effects (such as I/O) and state (which violates referential transparency) into a pure functional language.


## Type constructors

A type constructor is a parameterized type definition used with polymorphic types. By supplying a type constructor with one or more concrete types, you can construct a new concrete type.

`Maybe` is a type constructor, `Nothing` and `Just` are data constructors. You can construct a data value by applying the `Just` data constructor to a value. In the same way, you can construct a type by applying the `Maybe` type constructor to a type.

Polymorphic types are like containers that are capable of holding values of many different types. So Maybe Int can be thought of as a Maybe container holding an Int value (or Nothing) and Maybe String would be a Maybe container holding a String value (or Nothing). In Haskell, we can also make the type of the container polymorphic, so we could write "m a" to represent a container of some type holding a value of some type!

We often use type variables with type constructors to describe abstract features of a computation. For example, the polymorphic type Maybe a is the type of all computations that may return a value or Nothing. In this way, we can talk about the properties of the container apart from any details of what the container might hold.
