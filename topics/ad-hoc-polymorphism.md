# Ad-hoc polymorphism

Ad hoc polymorphism is a kind of polymorphism in which the same polymorphic function is applied to arguments of different types.

The canonical example of ad hoc polymorphism, i.e. of overloading is the plus arithmetic operator that is denoted the same way whether it is applied to two integers, two floats, two doubles, or two values of the same numeric type that is an instance of the `Num` class, which defines arithmetic operators including `+`.

Therefore, when an overloaded function name (or symbolic name) is encountered, it is not evident what implementation is supposed to be invoked *before* the type of arguments is examined. Only when the type of args is determined, the appropriate implementation is called. This process is known as *dispatching*, and here the dispatching on types (on the type of arguments) was described.

The term "ad hoc" in this context is not intended to be pejorative; it simply refers to the fact that this type of polymorphism is not a fundamental feature of a type system.

Ad hoc is contrasted by the *parametric polymorphism*, in which a polymorphic function uses type variables instead of mentioning any specific type, and a single abstract implementation is applicable to any type at all; unlike an ad hoc polymorphic function, it is unrestricted.

This kinds of polymorphism were introduced by Christopher Strachey in 1967.

In Haskell, polymorphism is parametric or ad hoc, the latter being the overloading polymorphism of type classes.







## References

https://en.wikipedia.org/wiki/Ad_hoc_polymorphism
