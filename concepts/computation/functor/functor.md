# Functor

Functor is a concept (and name) from CT. The concept is about "lifting" functions so a "regular" function that works element-wise, e.g. a function that takes an `a` to `b`, generically speaking, `f : a -> b`, can be "lifted" to a data type (data structure) in order for that function to be applied to all the elements within. The target data type is functorial in nature if it can be mapped (using that function). A list is the canonical example of a functor and the list mapping function is called `map`, but generalizing from list, `[] a`, where `[] :: * -> *` is the type name, almost all type ctors of arity >= 1 are functorial, i.e. can be mapped.

Such a generalized type is represented as a type ctor, say `f`, that has at least one type parameter, so at minimum `f a`. Here `f :: * -> *` and `a :: *`, meaning `a` must be a base type. The type `f` here is not a type that classifies values - only types of kind `*` can do that. The type `f` here is a unary type ctor, meaning we need to apply it to a (base) type to get a proper (base) type - that is, when the type ctor `f` is saturated it may (also) be denoted by `f a`. More concretely, `f` may be a list type ctor, `[]`, or `NonEmpty` type ctor, `Maybe`, `IO`, etc. These types are all functors.

Moreover, polyadic type ctors (which have more than 1 type param) can also be functors in their last type param (the previous ones are "fixed" at some types). So, the type `Either e a` is a functor as `Either e`, the type `(,) a b` is a functor as `(,) a`, the type `(->) a b` is a functor as `(->) a`, etc.

Many of these polyadic types are also functorial in their other type params, not just the last one. To do so, we can make type aliases (newtypes) for those types that can be functors in other typer params (except the first) as well. For example, a pair, `() a b` is by default a functor is the last param `b`, but if we create a newtype `FlippedPair b a = FlippedPair (a, b)`, we can make it a functor i nthe "first" type param as well. Of course, types that are functors in more than one type param (usually we consider both type params) can be made instances of the `Bifunctorial` class directly (which provides two maps, one for each type param).

The funtion type ctor, `(->)`, is special because it is only a functor (a *covariant functor*) in the second type param, i.e. in the return type. And it is a `Contravariant` (a *contravariant functor*) in the input type.

## Lifting

Given a function `f : a -> b` (that works element-wise; it could be said that it "maps" a single element ), we can lift it so it can map the entire functorial type structure. The term "functorial" means the structure that can be mapped (in a certain-set-of-laws-abiding way).

The concept of lifting means the we don't even need to examine the given function - if it maps a single object (element), it can map a bunch of them.

## Functor abstraction

However, generalizing all functorial types requires defining some carrier data type to represent them. In Haskell, we use type classes to define functorial behavior of types - the class `Functor` classifies (groups) all such types. The class has a single method, `fmap`, that is the generalized mapping function.

Thus, if we are given an *element-wise function* `a -> b`, we can lift it to a functor, getting the *functor-wise function* `f a -> f b`. This is one way of interpreting the `fmap` signature. The other way is that given a funtion `a -> b` and a functor `f a`, we can use the funtion to map the functor-full of `a`'s, i.e. `f a`, to get a functor-full of `b`'s, i.e. `f b`.

>Every functorial type defines its own implementation of the `fmap` method. That is, `fmap` has a general, one-size-fits-all, signature, but each type will implement it in its own way, thereby also specializing signature of `fmap`.

However, in a generic context, we can just call `fmap` on the types we know are functors without worrying about each type's concrete implementation of `fmap`.



## Functor type class

```hs
class Functor (f :: * -> *) where
  fmap  :: (a -> b) -> f a -> f b
```
