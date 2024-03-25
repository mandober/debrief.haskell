# Constraint kinds

https://stackoverflow.com/questions/31317159/constraintkinds-explained-on-a-super-simple-example

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/constraint_kind.html


There are two practical examples the constraint kind allows us to do:
- Write type classes that allow their instances to specify required constraints
- Parametrize a type by a type class constraint (?)

One of the famous Haskell issues is that you cannot declare a Functor instance for the `Set` type since the `Set` requires an `Ord` constraint on its elements.

The declaration, `data Set a = ...`, doesn't explicitly constraint its type parameter `a` to an `Ord` instance (because data context is deprecated), but any function or a class involving `Set` will bear the explicit `Ord` constraint, e.g. `instance (Ord a) => Monoid (Set a)`.

The reason is that the standard Haskell 98 doesn't allow putting constraints on method signatures; if a constraint is needed, it must be placed in the class head, to constrain the appropriate type variable there (as a superclass). Of course, that would affect every method declared in that class to carry that constraint. For example, `class (Semigroup a) => Monoid a`
