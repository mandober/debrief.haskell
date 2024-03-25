# Polytypic functions

*Polymorphism* abstracts types, just as functions abstract values. *Higher-kinded polymorphism* takes things a step further, abstracting both types and type constructors, just as higher-order functions abstract both first-order values and functions.

Here is a function with a higher-kinded type. The function `when` conditionally executes an action. 

```hs
when :: ∀ (m :: * -> *). Monad m => Bool -> m () -> m ()
when b m = if b then m else return ()
```

The kind ascription `* -> *` makes explicit the fact that `m` is a higher-kinded type variable: it abstracts type constructors such as `Maybe` and `[]`, which can be applied to types such as `Int` and `()` to build new types. The type of `when` says that its second argument and return value are monadic computations returning `()`, but the monad itself is not fixed: `when` can be used at any type `m ()` where `m` builds a type from a type and is an instance of the `Monad` class.

In Haskell `data` and `newtype` definitions create fresh data types. It is possible to hide the data constructors of such types by leaving them out of the export list of the defining module, but the association between a type name and the data type it denotes cannot be abstracted. It is therefore straightforward for the type checker to determine whether two type names denote the same data type: after expanding synonyms, type names denote the same data types exactly when the names themselves are the same.



❉ ❉ ❉ 

A polytypic program is a program that behaves uniformly over a large class of datatypes. This uniformity is achieved by parameterizing functions over type constructors to obtain polytypic functions.

A polytypic function may be defined by induction on the structure of *regular type constructors*, or in terms of other polytypic functions.

A type constructor `f` is regular if the data type `f a` contains no function spaces and if the `f`'s type argument (`a`) is the same on both sides of its type declaration.

A typical polytypic function is `fmap` that parameterizes the type ctor `f` of a datatype `f a`. The type variable `f` (that represents a type ctor) can be instantiated at, e.g. `[]`, `Maybe` and many other types of kind `* -> *`. And the element or value type variable `a` can then be instantiated at varoius base types, i.e. the types of kind `*`.

So, a datatype represented by `f a` has two dimensions of abstraction: it (e.g.) vertically parameterizes unary type ctors, and then for each concrete type ctor, it horizontally parameterizes the type of its elements. The base types are on the x-axes, and the unary type ctors are on the y-axes. It seemingly achives full abstraction, but not quite: there are plenty more things to a datatype that can be abstracted, including kind-polymorphism, levity polymorphism (lifted and unlifted types), rank polymorphism, linearity polymorphism, constraint polymorphism (e.g. `Set` has the `Ord` constraint on the type of its elements).
