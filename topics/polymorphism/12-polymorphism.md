## Polymorphism

In Haskell, polymorphism occurs in two major forms, mostly function-related:

[Ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) is the phenomenon when the same identifier (like function's name) has different implementations depending on the types involved (e.g. depending on the type of its arguments). In Haskell, ad hoc polymorphism is associated with type classes and the overloading of functions' names introduced thereby.

[Parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) is the phenomenon when the same identifier (function name) retains a single form but works with any type nevertheless. A parametrically polymorphic function uses type variables instead of concrete types (like monomorphic functions) or an implicit set of concrete types (like ad hoc polymorphic functions), maintaining a single implementation that works with any type (one size fits all). It can be said that the (concrete) types are parameterized out. When called, each caller decides the concrete types the function's type parameters are instantiated at. Normally, the concrete type, to instantiate a type variable at, is inferred from the argument.

[offside]: Haskell keps the one form of PP functions (until when? I stubled somewhere: down to assembly but hey), but Rust, for example, has only syntactically PP functions, since it monomorphisizes apparently parametrically polymorphic functions, possibly on each call (if it uses arguments of different types). That way, a single PP function can be specialized to many individual functions where each handles some specific set of argument (types), which can be more efficient at the expense of a code bloat (although, Rust offers other ways to handle this, like dynamic dispatch).




once for each different set of concrete types.

type value is decided for a.


Parametric polymorphism, types are not concrete but specified by type variables that stand for any type.

Ad hoc polymorphism defines a common interface for an arbitrary set of individually specified types.



The more polymorphic a function, the more it is constrained. The more types a function accepts, the less behavior those type have in common.


Parametric polymorphism may be contrasted with *ad hoc polymorphism*, in which a single polymorphic function can have a number of distinct, and very likely heterogeneous implementations, depending on the type of arguments to which it is applied. Thus, ad hoc polymorphism can generally only support a limited number of such distinct types, since a separate implementation has to be provided for each type.


PPF are practically forcing the only possible implementation, so a programmer just needs to "follow the types", as it's often said.

Type-driven development is feasible thanks to Haskell's purity, polymorphic and otherwise advanced type system. 

However, sometimes a function's signature alone fails to inspire the correct implementation, and on such occasions turning to the Curry-Howard correspondence for help can prove beneficial.


Given only a signature of a combinator, we can infer its definition and behavior.

For example, one of the basic combinators, `id`, can only ever return its argument, which we can infer from its type signature alone `id :: a -> a`. Moreover, the signature is forcing the only possible definition of `id`; despite that, we're still required to spell out that definition, `id x = x`. It is the same with other combinators like `const`, `flip`, `on`, `(.)`, `($)`, `(&)`, `fix`, `curry`, `uncurry`, `fst`, `snd`, etc. (and a lot of as lambda calculus' combinators translated to Haskell).
