# ConstrainedClassMethods

```hs
{-# LANGUAGE ConstrainedClassMethods #-}
```


- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstrainedClassMethods
- Allows the definition of further constraints on individual class methods.

Haskell 98 prohibits class method types to mention constraints on the class type variable, thus:
```hs
class Seq s a where
  fromList :: [a] -> s a
  elem     :: Eq a => a -> s a -> Bool
```

The type of elem is illegal in Haskell 98, because it contains the constraint `Eq a`, which constrains only the class type variable (in this case `a`).

More precisely, a constraint in a class method signature is rejected if:

1. The constraint mentions at least one type variable. So this is allowed:

```hs
class C a where
  op1 :: HasCallStack => a -> a
  op2 :: (?x::Int) => Int -> a
```

2. All of the type variables mentioned are bound by the class declaration, and none is locally quantified.

Examples:

```hs
class C a where
  -- Rejected: constrains class variable only
  op3 :: Eq a => a -> a
  
  -- Accepted: constrains a locally-quantified variable `b`
  op4 :: D b => a -> b

  -- Accepted: constrains a locally-quantified variable `b`
  op5 :: D (a,b) => a -> b 
```

GHC lifts this restriction with language extension `ConstrainedClassMethods`. The restriction is a pretty stupid one in the first place, so `ConstrainedClassMethods` is implied by `MultiParamTypeClasses`.
