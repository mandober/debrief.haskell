# Type constructor

* Type constructors are type-level functions, i.e. functions on types, and like their term-level counterparts, they also support currying and partial application.
* The arity of a type ctor is expressed through their *kind*. A fully saturated type ctor has kind `Type` or `*` (being deprecated).

* Type constructors express functional relations, where one type (the argument type) uniquely determines the other. For example, the relation between the type ctor for a list and the type of list's elements is a functional relation, expressed by the type constructor `[] :: * -> *`, which maps an arbitrary type `a` to the type lists of a's, `[a]`.

* A type constructor maps its argument types uniformly, incorporating them into a more complex type *without inspecting them*.
