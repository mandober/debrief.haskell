# Type classes


## Instance declaration

An instance declaration consists of an instance header and instance body.

In the simple case, an instance header consists of the keyword `instance`, followed by the class context, followed by the class name, followed by the recepient type ctor called the **instance head**, followed by the keyword `where`, followed by the definitions of class methods.

```hs
instance (class_context) => ClassName Head where
  -- defs of methods…
```

The simple case means a manually defines instance (as opposed to a standalone or attached instance derivation), without multiple recepient types (no `MultiparameterTypeclass`), functional dependencies; and no type families and default methods in the instance body.


## Orphaned instances

An instance of a type class for a data type is commonly defined in two manners. A module that declacres a new data type imports the modules where the type classes are declared, defining its own instance for each class. Alternative approach, often used in the standard library, is for a module that declares a class to import the modules of the data types it wants to define instances for. The third approach, in which both the class and data types are imported, and than instances of the imported data types are defined for the imported class results in the *orphaned instances*.

## Type classes are open

Type classes are open (an open world): Unlike a function definition which has a closed set of clauses matched from top to bottom, typeclass instances are open and unordered (there have been proposals to introduce ordered instances, known as *the instance chains*, but as of GHC 9.4 they are not implemented).


## Overlapping instances

A data type can only have a single instance of some class. In case the data type is discrete, containing no type variables, this rule is easily enforcable. In that case, there can be no superclass context since the recepient type ctor is a discrete type, so there can be no chains of "inheritence", e.g.

```hs
-- we cannot say
instance (Eq Int) => Show Int where
-- The constraint 'Eq Mint' is no smaller than the instance head 'Show Mint'
-- (Use UndecidableInstances to permit this)
```

However, when the recepient type ctor is parameterized or consists solely of a type parameter (like `a`), it opens the possibility of overlapping instances.

```hs
instance IsUnit () where
  isUnit = True

instance IsUnit a where
  isUnit = False
```
