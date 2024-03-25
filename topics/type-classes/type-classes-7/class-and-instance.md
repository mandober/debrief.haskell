# Type classes

```hs
instance TreeLike Tree
-- where doesn't hurt either
instance TreeLike Tree where

-- methodless class can skip 'where'
-- it can be used to group the types by a property or behaviour
class FlatPrimitive a
instance FlatPrimitive Int
instance FlatPrimitive Word
instance FlatPrimitive Char
instance FlatPrimitive Float
instance FlatPrimitive Double
instance FlatPrimitive Void   -- ?
instance FlatPrimitive ()
```

## Classes and instances

A type class is a way to group types by a set of common properties and behaviours, or even completely arbitrary.

Between the two extremes - a monomorphic type (`Int`) on one end, and a fully polymorphic type (repr by a type var `a`) on the other end - some languages have nothing in the middle. In Haskell type class allow us to populate the middle ground by grouping types into classes based on common bahaviour, convenience, or any other factor.

The plus operation surely works on an Int, but it also works on a Float and many other numric types, but it definitely does not work on all types. This means that we cannot enjoy being parametrically polymorphic in arithemtic operations, nor are we to surrunder and proceed to manually implement a set of such operations for every numeric type we need it to work on. Note: the ad hoc polymorphism would allow us to have the same name for, e.g. plus operation., but what we really want is *restricted parametric polymorphism*.

In order to constrain e.g. arithemtc ops to numeric types only, we use a class like `Num` to group suitable and applicable types. In case of this particular class, this is already done, but in general, this is manual labor - we need to opt-in each type we want to make a member of some class by providing the definition of all the required methods. If a class provides defulat implementations for all required methods, we need not do very much, but this is not often the case; when it is, we may decide to override the default with a more efficient implementation.

Thus, we start to carve the middle ground between the two extremes - we can stay seemingly polymorphic and still be able to say "for all types `a`...", followed by an appendage, "...that are members of the `Num` class". The type is still polymorphic* (but then we hide behind the asterisk). In a signature, this is expressed as a *constraint* that introduces a restricted form of parametric polymorphism.

```hs
add10 :: Num a => a -> a
add10 x = x + 10
```

It's like saying "pick any color you want, what-so-ever!", and then quitely adding, "as long as it's black".





---

In another sense, a class describes the interface of some value without telling you the implementation details.

An instance is a representation of the typeclass ↔︎️ data type relations. In order to show that the data type obeys the typeclasses rules and to use the methods of the typeclass on the data values, you need to provide the work instructions under this particular typeclass. And that is the instance of the data type for the particular typeclass.
