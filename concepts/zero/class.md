# Type Class

- typeclass
- typeclass vs interface
- typeclass coherency
- operator overloading
- ad-hoc polymorphism

**Type classes** are a way to generalize an operation or a computation pattern across the types. They are a way to have ad-hoc polimorphic functions, where a function with the same name works across different types.


## Typeclass vs interface

- multi-param classes
- default methods
- minimal set of methods for valid implementation
- superclass
- class constraints


Number literals are *polymorphic constants*. They act like any type with a Num class instance: `42 :: Int`, `42 :: Float`, `42 :: Double`, etc.


At its most fundamental level, an example is the plus operator working with different types of numbers. This is achieved by means of the `Num` class that declares a set of functions (methods) a type must define in order to become a member of the `Num` class. Then, if you know that some type is a member of `Num`, you know that you can use arithmetic operators on its values.

Along the organizational aspect, classes more often abstract a recurring pattern recognized across different types. We can then define a type class as a way to standardize involved functions and their names.

Like interfaces from imperative languages, classes codify a set of common functionalities under one name and prescribe a set of functions (methods) that a conforming type must define.

For example, the `Num` type class allows common arithmetic operators (which are indeed functions) to be shared and used by all number types; instead of having each number type (and there are quite a few of them, including `Int`, `Integer`, `Float`, `Double`, `Fraction`, `Rational`, `Real`) define its own function for addition under a unique name (e.g. `addInts`, `addIntegers`, `addFloats`, etc.), they implement the `Num` class. That means each type still defines its own way of doing addition, but at least they all share that one name for it (in fact a symbolic name). In fact, unlike interfaces, classes offer additional service in that you can optionally define a default implementation for any of the prescribed methods - each type can then decide whether to keep the default or to redefine it, perhaps in a more efficiant way.Thus, the operator `+` is polymorphic - it has the same name regardless of what type it is used with, thus providing a uniform way to deal with many of the Haskell's number types.

As long as a type implements the `Num` type class, you know you can use the arithmetic operators on the values of that type. However, this doesn't mean that values of two distinct `Num` types can be combined (perhaps possible with a prior type conversion).

The `Num` class doesn't prevent non-numeric types from applying for membership. Any type can implement any type class as long as it defines the minimal set of functions (while respecting the *typeclass coherency* rules). Any type may decide for itself what it considers addition; this implies that some well-known operators may not work as expected. This liberal form of *operator overloading* is rarely, if ever, found in other languages (go berzerk defining all the mathematical operators).


## As algebras

Besides codifying common functionality and/or behavior across different types (`Num`, `Ord`, `Eq`, `Bounded`, `Enum`, etc.), type classes offer a good way of representing algebraic structures (`Semigroup`, `Monoid`, etc.)

**Abstract algebra** describes many *algebraic structures*, also called *algebras* in terms of sets: an algebraic structure is a set, called the **carrier set**, **together** with a set of operations (sometimes called **attached operations**) and a set of axioms that must be upheld (the carrier set and the attached operations must ensure the prescribed laws are respected).

For example, a **Monoid** is an algebraic structure where consisting of a set together with a binary operation, and respecting the axioms of *closure*, *associativity* and *identity*. A concrete example of a monoid is *the set of natural numbers under addition*; to disect: the set of natural numbers is the carrier set, addition is the binary operation - together they make sure the 3 monoidal axioms are upheld.

**Closure** or **totality** means that combining any two elements of the carrier set, under the binary operation, produces a result that is also in that set. With (carrier) sets corresponding to types, this means that the output type of a function (operation) must be the same as the input types.



In Haskell, the binary combining function for monoids is the `mappend` operator: `<> :: a -> a -> a`

Sets are inhabited with their elements, and in programming languages, sets correspond to types that are inhabited with their values.

An attached operation obeys the same set of axioms in both cases.

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
