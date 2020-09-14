# Arity and kind

Arity is the number of arguments a language construct (function, type ctor, tuple, etc.) needs before it becomes fully *saturated* (at which point it becomes fully operational).

Programming language constructs like functions, type constuctors, data constuctors, and tuples are the types with intrinsic notion of arity. Actually, since type and data ctors are similar to functions, there are possibly only two language constructs that are strongly affected by arity, functions and tuples.

Functions declare a number of parameters and exactly this number is their arity, or at least, their original or initial arity. So all functions are `n`-ary, where `n >= 0` [^1] .

The intermediate states between the initial, unapplied, state and the final, fully saturated, state, are also made possible through partial application. Partially applied language constructs have reduced arity since they have received some, but not all, of their args.




Functions take value parameters (values as parameters) to produce values, while type constructors take type parameters (types as parameters) to produce types.

Aside: **Valency** is the combining power of an atomic element, especially as measured by the number of hydrogen atoms it can combine with. For example, carbon has a valency of 4, and when fully bounded it becomes *saturated*.

One of the things functions, type ctors, tuples have in common is the notion of *arity* or *adicity*, which is a measure similar to valency. Such types declare upfront the number of elements they need before they become "fully functional".

For example, a nonary (9-ary) function cannot do shit until it is completely saturated. A quintuple (5-tuple) doesn't become fully operational (to be e.g. passed around) before it fills all of its capacity.

Aside: Arity (adicity, adinity) is expressed in latin-rooted terms: nullary, unary, binary, ternary, quoternary, quinary, senary, sepatary, octary, nonary, decenary

and they will keep accepting elements until they are *saturated*.


Both functions and type ctors, may be partially applied. A ternary function is said to have arity of 3, which is like a valency factor that signifies 



applied to one arg only produces a binary function, and it's similar with type ctors. 

Similar to functions which take value parameters and can be partially applied, type constructors take type parameters (types as parameters), even partially, to produce concrete *saturated types*.



Functions take parameters (values as parameters) to produce values. 

Like functions, type constructors (which are also functions) can be partially applied by supplying only the first type parameter to the type ctor.





[^1]: Although `n` is a natural number, it should stay reasonably small; that is, when time comes that a function needs more then a few (3-4) arguments, either it has become complex enough to deserve splitting it into two, or you need an alternative approach to the ancient dilemma that is a very needy function versus maintaining usability and readbility; the popular solution in Haskell is designing the new type that will represent this function's arguments. The needy function msut be reworked to accept and work with this new type that is its argument collection. Then, you instantiate the arg collection and populate it with default values. This lends itself great as a sort of optional argument implementation for Haskell because the clients are spared to enter every argument; that is, any arg not stated explicitly will take on the default value. In order for this to work you also need to create a new factory function that creates new instances based on the defualt instance. Along with selective exports and smart ctors. Other languages' approach this problem similarly, that is, passing some sort of collection (object, array, map).
