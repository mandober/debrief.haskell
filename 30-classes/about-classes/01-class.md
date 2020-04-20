# Type Classes


## Rationale

**Type classes**, or just **classes**, are a way to generalize an operation or a computation pattern across the types.

To abstract a reoccuring pattern or a principle recognized across different types, we use a type class for it provides a means of organization and standardization. 

of common functionalities.

in many other ways but remain sufficiently similar wrt to the core principle we're abstracting.


They have some similarities to the concept of the interface that is present in some imperative languages. Like interfaces, they codify a set of common functionalities under one name and prescribe the set of functions (methods) that a conforming type must implement.


For example, the `Num` type class is about sharing the (symbolic) names of the common arithmetic operators (`+`, `-`, `*`, etc.) across the number types. So instead of having each number type (`Int`, `Integer`, `Float`, `Double`,  `Fraction`, `Rational`, `Real`, etc.) define its own, e.g., addition function under a different name (`addInts`, `addIntegers`, `addFloats`, etc.), they can just implement the `Num` type class by defining their own implementation of the required class operations in a way that is the most appropriate and the most efficient for each type. Then, e.g., the addition of two values of any number type is done by using the same operator, `+`. Thus, the operator (function) `+` is polymorphic, having the one and the same name regardless of what concrete type it operates on, providing a uniform way to deal with many of the Haskell's number types.

As long as a type implements the `Num` type class, you know you can use the arithmetic operators on the values of that type.

However, this doesn't mean that values of two different types, albeit being numeric and both implementing the `Num` class, can be combined together (at least not before performing a type conversion, if applicable).

Further, even though the `Num` class declares and standardizes the names for the common arithmetic operators, it doesn't prevent non-numeric types from implementing it. In fact, any type can implement any type class as long as it provides the implementation of the prescribed functions (while respecting the *typeclass coherency* rules). So a type, no matter how complex, can define for itself what the addition means for it. On the other hand, this means that the plus operator might not work as expected with some custom third-party types. This is the concept of *operator overloading*, which is almost never found in this liberal form in other languages (one can go wild and define pretty much all the mathematical operators).




Haskell takes inspiration from **Abstract Algebra** by replacing a carrier set with a type and set members with values that inhabit a type. An attached operation obeys the same set of axioms in both cases.

Math vs Haskell - the thing that differ
- algebra (algebraic structure) vs type class
- (carrier) set vs type
- set members vs values of a type
- operation over a carrier set vs operation over a type

Math vs Haskell - the things that are the same
- attached operation is the same wrt arity
- axioms of closure, associativity, identity, invertability and commutativity remain the same


Haskell implements algebras as type classes. A **type class** declares a set of operations that a type that wants to implement it must define.

An **instance** of a class is a type that defines its own implementation of operations a type class declares. In Haskell, we think of types as having an instance of a typeclass.

Each type class prescribes a **minimal set of operations** (checked by the compiler) an implementing type must define. However, many classes also offer **default definitions** for some operations (these are the operations that can be easily defined in terms of the provided ones from the minimal set) and instances are free to accept or override them (with e.g. more efficient implementation).


As the simlest example, Haskell's `Num` class provides a uniform way to deal with Haskell's number types. Thus we can reuse the same set of names (either alphabetical or symbolic) for many operations that operate on numbers. Concretely, we can always use the symbol `+` to perform addition, regardless what the concrete number type is (as long as it is the instance of `Num` class). This also saves us to come up with different names for each number type for essentially the same operations: instead of addInt, addInteger, addDouble, etc. we just use `+`, which is a polymorphic operator constrained to types that are instances of the `Num` class.
