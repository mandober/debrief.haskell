# Constrained methods

Haskell 98 prohibits method types to mention constraints on the class type variable. 

More precisely, in a method signature, **a constraint is rejected** if:

* The constraint mentions a type variable.

You can only put constraints that don't mention any type variables, so these are allowed:

```hs
class C a where
  op1 :: HasCallStack => a -> a  -- nullary class
  op2 :: (?x::Int) => Int -> a   -- implicit param
```

* All type variables mentioned in a constraint are bound by the class declaration, and none is locally quantified.

```hs
class C a where
  op3 :: Eq a => a -> a    -- Rejected: constrains class variable only

  op4 :: D b => a -> b     -- Accepted: constrains a locally-quantified var b
  op5 :: D (a,b) => a -> b -- Accepted: constrains a locally-quantified var b
```
