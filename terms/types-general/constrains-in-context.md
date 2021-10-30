# Type constraints in a class declaration

- `=>` is a type constraint operator
- single constraint:    `(+) :: Num a => a -> a -> a`
- multiple constraints: `(^) :: (Num a, Integral b) => a -> b -> a`



## Class constraints

Class constraints are stated after the `instance` keyword as a list of type classes, each stating the affected type param.

In case of multiple class constraints parenthesis are obligatory, otherwise optional. The list of constraints in separated from the implementing type's declaration by the *type constraint operator*, `=>`.

Besides class constraints that affect the entire class and containing functions, each function can declare own additional type constraints.

```hs
class Eq a where
  (==) :: a -> a -> Bool

class Eq a => Ord a where
  compare :: a -> a -> Ordering

class Num a where
  (^) :: (Integral b) => a -> b -> a

class Applicative m => Monad (m :: * -> *) where
class Functor f => Applicative (f :: * -> *) where
class Semigroup a => Monoid a where
class Category (cat :: k -> k -> *) where

-- outside classes, method signatures include the inherited class constraints:
compare :: Ord a => a -> a -> Ordering
(^) :: (Num a, Integral b) => a -> b -> a
```



The `Eq` class declaration:

```hs
-- Defined in ghc-prim-0.5.3:GHC.Classes
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  -- it also gives this default impl, meaning that it is enough to
  -- define only one of this two functions, the fact also acknowledged
  -- by the MINIMAL class remark.
  x /= y = not (x == y)
  -- very broad default impl defines operators in terms of each other,
  -- so you only need to define one and the other is then defined by this
```

The Eq class expects a concrete type judging by (1) the type param, `a`, as it appears in the header line `class Eq a where`, (2) then comparing to how it appears in the signatures, `a -> a -> Bool`; it is "alone" so it must be a concrete type.

```hs
instance Eq a => Eq (Maybe a)
instance (Eq a, Eq b) => Eq (Either a b)
instance (Eq a, Eq b) => Eq (a, b)
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)

-- Defined in ‘Data.Map.Internal’
instance (Eq k, Eq a) => Eq (Map k a)

-- Defined in ‘ghc-prim-0.5.3:GHC.Classes’
instance Eq a => Eq [a]
instance Eq Ordering
instance Eq Int
instance Eq Float
instance Eq Bool
```



```hs
{-# LANGUAGE InstanceSigs #-}

class Functor f where
  fmap :: (a -> b) -> f a -> fb

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map
```


Type params

How to add constraint on param `a`, e.g. `Eq a`?

```hs
-- NOOOOOOOOOOO
class Eq a => Junctor f a where
  fmap :: (a -> b) -> f a -> fb

instance Junctor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map
```







## Constraints placement

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
