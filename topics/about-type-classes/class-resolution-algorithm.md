# Class resolution algorithm

- class declaration
- class instance declaration

An *instance declaration* has the following shape:

instance <constraints> => C <types> where

The part to the left of the `=>` is called *instance context*, while the part to the right is the *instance head*.

When GHC attempts to determine which class instance to use to solve a *class constraint*, only instance head matters; instance context is completely ignored.

Once GHC picks an instance, it commits to its choice, and only then considers *instance context*.


```hs
class UnitList as where
    unitList :: HList as

instance UnitList '[] where
    unitList = HNil

-- (1)
instance UnitList as => UnitList (() ': as) where
    unitList = () :↑ unitList

-- (2)
instance (a ~ (), UnitList as) => UnitList (a ': as) where
    unitList = () :↑ unitList

-- (3)
h8 = unsingleton unitList :: ()  -- ()
```

The two `UnitList` instances behave differently wrt (3). The (3) type checks only with instance head (2), not with (1).
- (1) instance head has `(() ': as)`
- (2) instance head has `(a  ': as)` instead, but also `a ~ ()` in context

This explains why our two `UnitList` instances behave differently:
* Given the instance head `UnitList (() ': as)`, 
  GHC won't select it unless it knows the first element of the list is `()`.

* But given the instance head `UnitList (a ': as)`, 
  GHC will pick the instance regardless of the type of the first element. 
  All that matters is that the list is at least one element long.


After the `UnitList (a ': as)` instance is selected, GHC attempts to solve the constraints in the instance context, including the `a ~ ()` constraint. This forces `a` to be `()`, resolving the ambiguity and allowing type inference to proceed.

This distinction might seem excessively subtle, but in practice it is enormously useful for it means that programmers have direct control over the type inference process:

* If you put a *type in the instance head*, 
  you're asking GHC to figure out 
  how to make the types match up somehow. 
  This can be useful if you want 
  that type to inform which instance to pick.

* If you put an *equality constraint* 
  in the *instance context*, 
  the roles are reversed: 
  you're saying to the compiler 
  that you will tell it what type it is, 
  which effectively is giving you 
  a role in type inference itself.
