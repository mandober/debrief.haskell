# MultiParamTypeClasses

- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MultiParamTypeClasses
- Implies: `ConstrainedClassMethods`
- Allow the definition of typeclasses with more than one parameter.
- For example:

```hs
class Collection c a where
  union :: c a -> c a -> c a
  -- etc.
```
