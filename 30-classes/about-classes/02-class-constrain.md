# Class constraints

- `=>` is a type constraint operator
- `(+) :: Num a => a -> a -> a` (i.e. + works for any `Num` instance)
- multi-constraints: `(^) :: (Num a, Integral b) => a -> b -> a`



Places to put class constraints:

* In `class` declarations:
  - makes the current class a subclass of another class

```hs
-- class declaration
class Num a where

-- class declaration with Eq constraint for `a`
class (Eq a) => Num a where

-- the type `a` now first has to
-- be in Eq before being in `Num`

-- Num is a subclass of Eq
-- Eq is a superclass of Num
```

Subclass
- type `a` now first has to be a member of `Eq`, before it can be in `Num`
- `Num` is a *subclass* of `Eq`
- `Eq` is a *superclass* of `Num`
- This also means that if we define default impl for some functions of the Num class, we can use functionality that `Eq` superclass provides, i.e. (==)


## Class extension

When defining a class `Ord` we may find it useful if Ord would integrate the methods available on `Eq` class alongside defining its own set of comparison operations and minimum and maximum functions. In such case we'd say that Eq is superclass of Ord. This is implemented similarly to type constraints only this time a class is constrained with another:

```hs
class (Eq a) => Ord a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a
```

We say that Eq is a **superclass** of Ord or, conversely, that Ord is a **subclass** of Eq. 

> Any type which is an instance of Ord must also be an instance of Eq.

One benefit of such class inclusions is shorter *contexts*: a type expression for a function that uses operations from both the Eq and Ord classes can use the context `(Ord a)`, rather than `(Eq a, Ord a)`, since Ord "implies" Eq.

More importantly, methods for subclass operations can assume the existence of methods for superclass operations.

Haskell also permits *multiple inheritance*, since classes may have more than one superclass.

Class methods are treated as *top level declarations* in Haskell. They share the same namespace as ordinary variables; a name cannot be used to denote both a class method and a variable or methods in different classes.

Contexts are also allowed in data declarations, although very discouraged.

Class methods may have additional class constraints on any type variable apart from those defined in the class declaration context. For example, in this class:
```hs
class C a where
  m :: Show b => a -> b
```
the method `m` requires that type `b` is in class `Show`. However, the method `m` could not place any additional class constraints on type `a`. These would instead have to be part of the context in the class declaration.
