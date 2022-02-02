# Multi-parameter type classes

https://wiki.haskell.org/Multi-parameter_type_class

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/multi_param_type_classes.html

- pragma `MultiParamTypeClasses`, implies `ConstrainedClassMethods`
- since: 6.8.1

The `MultiParamTypeClasses` GHC language extension allows defining type classes that have more than one type parameter. Type classes defined like this really specify relations between types.

```hs
class Collection c a where
    union :: c a -> c a -> c a
```
