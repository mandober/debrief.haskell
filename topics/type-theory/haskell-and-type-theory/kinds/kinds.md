# Kinds and data kinds

Haskell has a layered type system. *Types* are defined using a much more limited language than *terms*. Perhaps the most obvious omission is the lack of a plain lambda abstraction on the type level. Nevertheless, types are structured, and there is the possibility to apply types to one another, and to parameterize named types. Therefore, types need their own, non-trivial, types, which are called *kinds*.

The kind `*` is for types that classify terms (even though not every type of kind `*` need be inhabited). In particular, whenever we define a new datatype using `data`, its fully applied form is of kind `*`. This also implies that kind `*` is open. New members of kind `*` can be added via data at any time.

If `k` and `l` are kinds, we can form the function kind `k -> l` to indicate types of kind `l` that are parameterized over types of kind `k`.

We can even define new kinds ourselves, via datatype promotion. Whenever we define a new datatype, we can also use it as a kind.

`DataKinds` promotes everything one level up, so, e.g. `Bool` also becomes a new kind with two type constructors: `'False :: Bool` and `True :: Bool`. Just like there are only two values of type `Bool` (ignoring bottom), there are only two types of kind `Bool`. The types (type ctors) `'False` and `'True` are uninhabited.

The kind `*` remains the kind of types that classify values. All the other new kinds we gain by promotion are uninhabited types.
