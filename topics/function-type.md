# Function type

- function type
- function type ctor
- Reader == (r ->)


The most general type of functions is `a -> b`. However, this signature cannot stand alone and unrestricted - there are no concrete functions with this (polymorphic) signature because it suggests that a type `a` can somehow be converted into a type `b`.

Aside: Actually, there is an unsafe function with this signature, `unsafeCoerce :: a -> b`, intended to convert only between the types that have the identical internal representations (else runtime corruption). Its alias `coerce` is a method of the internal `Coerce` class whose instances are automatically implemented by the compiler; a type and its newtype wrapper can be converted to one another using `coerce` as they have identical internal representations.

Back to safety. The proper, explicit, type signature is `∀a b. a -> b`, which more strongly suggest that any type can be converted into any other type.

Therefore, this type signature should be considered a type schema, where `a` and `b` can be instantiated at different (or possibly the same) concrete types.

A similar function type is `∀a. a -> a` but since its input and output type params are in fact the same, it doesn't suggest any sort of type conversion. In fact, the only thing this function can ever do is return the input unharmed (deserving its name`id`).




## Function type constructor

The general type of functions is `r -> a` where `r` is the input type and `a` is the output type.

This type can be written in alternate notation: ((->) r a),
that is, if we were allowed to hide the predefined function type, we could define our own similar to:

```hs
data (->) r a = (->) r a
type FunIn r = ((->) r)
```


The (...) indicate that function type is special as we can't define its data ctor directly (should we write ((->) r a)? or (\r -> a)? something else?).

However, the order of its type params is fixed: the first type param represents the input type, while the second represents the output type.

The function type ctor has similar behaviour as other type ctors; mainly, it supports auto-currying, so it can exhibit 3 possible kinds corresponding to its 3 levels of saturation:

function type ctor | shape      | kind
-------------------|------------|------------------
fully-saturated    | ((->) r a) | *
semi-saturated     | ((->) r)   | * -> *
unsaturated        | ((->))     | * -> * -> *

This means, the function type ctor has evident instance for the classes that expect fully-saturated type ctors of kind `*` (e.g. Show, Read, etc.), and of classes that expect type ctors of kind `* -> * -> *` (which classes are these?). But, for classes that expect type ctors of kind * -> * (Functor, Applicative, Monad, etc.) despite the fact that it has 2 type params, its "natural" form for the instance of these classes is ((->) r), that is, only its first type param can be fixed, leaving the seconf type param to vary. This was predetermined by the order of its type params in its declaration. This means, the function type ctor can be made, e.g., a Functor only with its input type param fixed, while leaving its output type param to vary.

By the way, another name for the Functor class is Covariant, and the function type ctor can only be made its instance exactly in the shape ((->) r), i.e. in its natural shape with the fixed input type param. If using a newtype, we instead fix the output type while letting the input type vary, we would not be able to make it an instance of Functor i.e. Covariant becasue in that shape it is contravariant so it could only be made an instance of the Contravariant class. The function type is the only algebraic type that exhibits variance; its type params are subject to variance, so each of its type param can have its own variance. A type param in the negative position is contravariant and negative position is always before the arrow. All other positions are positive: the position after the arrow, as well as all the position of sum and product types (sum and product types are therefore always covariant). If the same type param is in both positions (before and after the arrow), then it is invariant.


## ((->) e), Reader, ReaderT and MonadReader
