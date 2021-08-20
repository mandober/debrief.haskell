# Type class declaration

- type class declaration
- grouping arbitrary types
- label defining class
- behavior defining class
- declaring required methods
- defining defaults for methods
- grouping types according to shared behaviour 
- overloading
- context
- notation
- blanked instancing (?)
- superclass
- class implementation details (hauling dictionaries)
- multparameter type classes
- functional dependencies


## Type class declaration

Type classes allow us to group types in named classes (sets). Such classes can be completely unburdened, requiring nothing from the potential members. Note that `where` keyword is still necessary.

```hs
-- free group
class Textual a where

-- group member types
instance Textual Char where
instance Textual String where

texter :: Textual a => a -> a
```

With `Textual` class declared and with String and Char as its instances, a polymorphic function can now be defined to work with them both by placing the `Textual` constraint. Then the type param `a` can only be substituted for String or Char.


Type classes usually require additional behaviour from the potential members by declaring a set of methods they must define (the cost of membership). For example, the predefined type class `Eq` provides the common global names (that is, symbolic names) for doing equality comparisions.

```hs
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

Builtin types have nothing new to gain by being members of this class as each of them certainly already has a function for comparision (of values of the same type) and newly defined types can also declare their own comparison functions. However, by being instances of the `Eq` class two enhancements are gained: overloading and grouping. Overloading means using only one name, i.e. (`==`), for doing equality comparision, regardless of the type at hand. Grouping means being able to place the `Eq` constraint on a polymorphic function to restrict it to only accept types that can be compared for equality (which can be crucial for e.g. functions working with sets in order to detect duplicated elements).

```hs
insert :: (Eq a) => a -> Set a -> Set a
```

## Notation

* Context is delimited with a fat arrow (=>)
* Restricting a polymorphic function is done by placing a class name in the function's context. The connection between the type the function is working with and the constraint is through the shared type parameter.
* Single class constraint needs no parenthesis.
* Multiple class constraints can be given:
  - as a tuple (each class states the type parameter even if common)
  - in a curryied form, by repeating the context (multiple =>)


```hs
-- single class constraint: parens are optinal
delete :: Eq a => a -> Set a -> Set a
delete :: (Eq a) => a -> Set a -> Set a

-- multiple class constraints: tuple form
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- multiple class constraints: curryied form
sequenceA :: Applicative f => Traversable t => t (f a) -> f (t a)
```
