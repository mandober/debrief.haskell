# Isomorphisms

There isn't one notion of equality in mathematics, but several related notions. The question of whether two objects are the same can be posed in relation to different properties of objects. Being completely identical may be sometimes a useful notion, but more often we're interested in a weaker form, that is, if two objects are the same only regarding a set of properties we care about. Another problem that comes with equality is that it is sometimes difficult to define. For example, functions have several modes of equality: intensional, extensional, referential (only in CS, in PLs).

When two object are 'equal enough" we say they are equal *up to* anther set of properties. Thus, we have a set of important properties and both objects share these properties (they are the same in both instances), but there is also a set of properties we don't care about and the two objects may be different in that (unimportant for us) regard.

Two objects are isomorphic when, even though they are not identical, we can still convert between them. In PLs, especially in Haskell, we say that two types are isomorphic if there are two functions (usually, `into*` and `from*`) that we can use to convert between them (but there may be just one function that can convert in both directions).

The keyword `type` introduces a type synonym, or more precisely, a type shorthand since this is most useful to shorten a long type signature. The compiler will still treat the two types as being the same.

The keyword `newtype` does introduce some difference between the two types, which is useful when we want to separate meters from bytes even though they're both integers at heart. A newtype wrapper is thin abstraction and can be explicitly removed on request (with `coerce`) since the underlying types are really the same. By default, they won't live after the compilation phase, so at run-time they will be stripped to the underlying type.

```hs
-- the regular id function
id :: a -> a
id x = x

-- and the wrapped id function
newtype Identity a = Id (a -> a)

-- are isomorphic
id ≅ Identity
```
