# 4.1 Overview of Types and Classes

Haskell uses a traditional Hindley-Milner polymorphic type system to provide a static type semantics, but the type system has been extended with *type classes* (or just "classes") that provide a structured way to introduce *overloaded functions*, aka *ad hoc polymorphism*.

A **class declaration** introduces a new type class and the overloaded operations (methods) that must be supported by any type that is an instance of that class. A class declaration may be read "a type `a` is an instance of the class `C` if there are class methods `f` and `g`, of the given types, defined on it".

An **instance declaration** declares that a type is an instance of some particular class and includes the definitions of the overloaded methods *instantiated at the named type*. An instance declaration may be read as "`Int` is an instance of the class `Num` as witnessed by the definitions for the class methods `f` and `g`".

HISTORICAL NOTE: The term "type class" was used to describe *the original Haskell 1.0 type class system*, during which time the term *"constructor type classes (by Jones)"* was used to indicate an extension to that original type class system. However, soon there after, the extension had been fully integrated into what is now known by the name *type class system*. In general, there is no reason to use two different terms. Some authors, however, use the term *"constructor class"* still topday to indicate the sort of class, such as Functor, i.e. the classes whose type param is a type ctor (of e.g. kind `* -> *`) and not just a saturated type od kinf `*`. This report uses the term "type class" to include both the original Haskell type classes and the constructor classes introduced by Jones.


The original Haskell 1.0 type class system only allowed class' type params of saturated types, i.e. types of kind `*`. For example, in the class `Eq a`, the TP `a` ranges over arbitrary types, but they all must have the kind `*`.

```hs
-- TPs are explicitly kind-annotated

-- plain, original class spec v.1
class Eq (a :: *) where
  (==) :: a -> a -> Bool

-- constructor class
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

**Constructor classes** were a generalization of the type class system in Haskell 1.0. These are classes that include a type parameter that ranges over a set of type constructors that must have a particular kind, like in the `Functor` class where the `f` TP has kind `* -> *`. In the `fmap` method, the type variables `a` and `b` represent arbitrary types of kind `*`, while the type variable `f` ranges over the set of type ctors of kind `* -> *`.
