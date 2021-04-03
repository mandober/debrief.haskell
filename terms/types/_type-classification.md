# Type sorts

**Scalar type** 
Scalars are the set of the most fundamental types from which other types are derived, or which other type constructs need in order to become complete.
List is an incomplete type until the type of its elements is known.
More in math then CS, a scalar is a one-dimensional "flat" type like Int, Integer, Bool, Char, unit (as opposed to value constructors like list and tuple and especially as opposed to vectors and matrixes, i.e. multi-dimensional types). These are the level-0 types.

**Generic type**
A type parameter, usually a single letter lowercase, denotes a generic, or "any", type. `Maybe` type is parameterized by 1 (generic) type: `Maybe a`. So, Maybe has 1 type parameter, and when the concrete type is provided, Maybe becomes complete, e.g. Maybe Int. So `Maybe Int` is a concrete type, but `Maybe` is not actually a type but a *type constructor* (also called *value constructor*) that takes 1 concrete type as the parameter.

**Concrete type**
Sometimes it is said that basic types (like Int, Bool) but also type params,like `a` are concrete types. That is, even a generic type param like `a` represents a concrete type that will eventually be filled in. On the other hand,a type class might be parameterized by one (or more) type params, but it can also be parameterized by a type constructor that is itself parameterized by one or more concrete types.

**Base type**
(complete, basic type) e,g, (), Bool, Int, Integer, Char

**Aliased type** 
Type aliasing is making the type availableunder a diff name.
e.g. `String = [Char]`

**Type Constructor** 
is not a type but a *Value Constructor*. Maybe Int is a concrete type, but `Maybe` is not a type but a *type constructor* or *value constructor*, that takes 1 concrete type as the parameter. **Type Constructors are functions**.


Declarations

* Module declaration
  - keyword `module`
  - module name
  - optional: comma-sep imports in parenthesis
  - keyword `where`
  - e.g. `module ModName (Data.List) where`

* Type signature
  - prior to definition of func, type, class
  - func :: (Eq a, Num b) => a -> (a -> b) -> b

* Data declaration:
  - keyword `data`
  - type constructor, type name
  - data constructors (values)

* Inline items
  - type annotation, `read 5 :: Int`

* Module elements:
  - module declaration
  - functions, types, typeclasses
