# Derivable classes in Haskell 2010

- automatically deriving classes
- class deriving
- associated deriving clause

Haskell 98 allows associating a `deriving`-clause with a type declaration in order to generate standard instance declarations for one or more classes specified in the clause.

However, not all classes are derivable this way; the programmer can only pick one or more classes from this standard class set:
- `Show`
- `Read`
- `Eq`
- `Ord` (depends on `Eq`)
- `Bounded`
- `Enum`

Moreover, not all classes from the standard class set are applicable to all types.

```hs
newtype Celssius = C Int

instance Eq Celssius where
    C n == C m = n == m
```

## Haskell 2010 Report: 4.3.3 Derived Instances

https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-780004.3.3

* Data and newtype declarations may contain an *optional deriving form (clause)*. If the clause is included, then *derived instance declarations* are automatically generated for the datatype in each of the named classes.

* These instances are subject to the same restrictions as user-defined instances. When deriving a class `C` for a type `T`, instances for all superclasses of `C` must exist for `T`, either via an explicit instance declaration or by including the superclass in the deriving clause.

* *Derived instances* provide convenient commonly-used operations for user-defined datatypes. For example, derived instances for datatypes in the class `Eq` define the methods `==` and `/=`, freeing the programmer from writing them manually.

* The only classes in the Prelude for which derived instances are allowed are `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`.

* The precise details of how the derived instances are generated for each of these classes are provided in [Specification of Derived Instances](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18200011), including a specification of when such derived instances are possible.

* Classes defined by the standard libraries may also be derivable.

* A static error results if it is not possible to derive an instance declaration over a class named in a deriving form. For example, not all datatypes can properly support class methods in `Enum`.

* It is also a static error to give an explicit instance declaration for a class that is also derived.

* If the deriving form is omitted from a data or newtype declaration, then no instance declarations are derived for that datatype; that is, omitting a deriving form is equivalent to including an empty deriving form, `deriving ()`.
