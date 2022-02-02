# Type Classes



Type classes provide a form of bounded, or ad hoc, polymorphism, which is the type of polymorphism that constrains a type variable, so it can no longer be instantiated at any type (as in the case of unbounded, or parametric, polymorphism), but only at those types that are members of the constraining class.

The unbounded or parametric polymorphism puts no constraints on a type parameter, letting it be instantiated at any type at all. The bounded or ad hoc polymorphism restricts a type parameter, so it can only be instantiated to a subset of all types; it restricts the types to only those that implement the particular class.

We would define a new class when we need to constraint a type parameter in some way. In ad hoc polymorphism this is used to make sure that a type has a set of methods, that we're counting on, available.

Probably one of the classes with the widest scope (in the sense of how much types it admits vs the types it doesn't) is the `Show` class that admits almost all types except the function types (because functions, for various, justified, reasons cannot have the `show` method).


single-parameter typeclasses in Haskell are sets of types, and a lawful instance is a proof that the type is in the set; multi-parameter classes (MPTCs) are relations on types; and functional dependencies and type families allow functions on types. Subclasses are subsets (or subrelations): class Eq a => Ord a says that if a type is in Ord, then it must also be in Eq, therefore Ord is a subset of Eq. (The => arrow is backward implication.) –


## Single parameter class

### Class as constraint

In a single parameter simple type class there is just the name of the class along with one type parameter that stands for potential instance types.

```hs
-- The definition of the Show class
class Show a where
  show :: a -> String

-- A type that agrees to become a class' instance
-- is obliged to define the show method
instance Show Int where
  show :: Int -> String
  show n = int2str n

-- The class can be now used as a constraint to restrict the types the type var `a` can be instantiated to; we can be positive that whatever the type it is, it will have the `show` method available (so we can invoke it, worry free).
showMe :: Show a => a -> String
showMe a = show a
```

### Class as a club

However, a class is not obliged to define any methods. We can define a class that is method free and serves just to name the subset of types; this subset will contain only the types willing to become members. This might be a good way to group types arbitrarily, but it also has more advanced uses, especially when one or more superclasses are added.

```hs
class Primitives a
instance Primitives Int
instance Primitives Char
instance Primitives Float
instance Primitives Double
instance Primitives ()
```

```hs
class Inductive a => Inductive (f a)
```


### Single parameter constructor class:

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b


instance Functor (f) where
  fmap :: (a -> b) -> f a -> f b
  fmap f k = undefined
```
