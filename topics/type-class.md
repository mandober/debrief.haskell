# Type class

A type class defines a set whose members can be arbitrary types as long as they implement the specified set of functions (methods).

Monomorphic functions only work with one particular type, while polymorphic functions work with all types. Between these two extremes (one type v all types) are functions that are constrained with one or more type classes. That is, a polymorphic functions's type param may be constrained to a class like `Show`, making that function work only with types that impl Show (which are most types, except, most notably, for the function types, since functions cannot be printed).

When a type class method is called, the compiler uses type inference to determine which implementation of the method to select, based on the inferred arg types; it is somewhat similar to overloading in Java.

## Type class v Interface

A type class is similar to an interface in OOP in that both define a set whose members are types that implement a specified set of operations (methods). However, type classes are more general than interfaces:

* A class (e.g. in Java) must specify the interfaces it wants to implement upon its definition (cannot be delayed). On the other hand, in Haskell, declarations of types, declarations of type classes, declarations of type class' instances are all done separately.

* The signatures of type class methods are more general and flexible than the signatures of interface methods, especially with multi-parameter type classes.

* A type class may define default overridable implementation. That way a type class can offer many default methods that depend on a few explicitly defined ones.

* Haskell type classes can also have polyadic methods with regards to the "privileged" parameters. In Java, there can be only one parameter used for method dispatching. For example, an interface that defines addition can only have one privileged param (of the two params that are involved in addition), i.e. the one off of which "plus" method is called. This asymmetry makes n-ary methods awkward. Furthermore, because of Java's subtyping, two params of an interface method may not be equivalent types, which makes implementing binary operators cumbersome.

* *Multi-parameter type classes* are enabled by a language pragma. For example, the following type class performs multi-dispatch: the implementation chosen by the compiler depends on both `a` and `b`.

```hs
class Multi a b where
    multi :: a -> b -> Bool
```
