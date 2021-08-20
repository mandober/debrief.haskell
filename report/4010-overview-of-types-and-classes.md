# 4.1 Overview of Types and Classes
4010-overview-of-types-and-classes.md

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


# 4.1.1 Kinds

To ensure they are valid, *type expressions* are classified into different kinds, which take one of two possible forms:
- the `*` represents the kind of all saturated type ctors
- if `κ₁` and `κ₂` are kinds, then `κ₁ → κ₂` is the kind of types that take a type of kind `κ₁` and return a type of kind `κ₂`.


```
C :: κ₁ → κ₂    t :: κ₁
----------------------- [kind-app]
      C t :: κ₂
```

Kind inference checks the validity of type expressions in a similar way that type inference checks the validity of value expressions.


## 4.1.2 Syntax of Types

The syntax for Haskell type expressions

```js bnf
type  :=  btype [-> type]             (function type)

btype := [btype] atype                (type application)

atype
 := gtycon
  | tyvar
  | ( type1 , … , typek )             (tuple type, k ≥ 2)
  | [ type ]                          (list type)
  | ( type )                          (parenthesised constructor)

gtycon
 := qtycon
  | ()                                (unit type)
  | []                                (list constructor)
  | (->)                              (function constructor)
  | (,{,})                            (tupling constructors)
```

Just as *data values* are built using *data constructors*, *type values* are built from *type constructors*.

The 4 main forms of type expressions:

1. *Type variables*: their kind is determined by the context

2. *Type constructors*: 
  - Char, Int, Integer, Float, Double, Bool are type constants with kind `*`
  - Maybe and IO are unary type ctors, treated as types with kind `* -> *`
  - The declarations `data T …` or `newtype T …` add the type constructor `T` to the type vocabulary. The kind of `T` is determined by kind inference. Special syntax is provided for certain built-in type constructors: 
  - () is the trivial type and has kind `*`, denotes *nullary tuple* type, and has exactly one value, `()`
  - The function type is written as `(->)` and has kind `* → * → *`
  - The list type is written as `[]` and has kind `∗ → ∗`
  - The tuple types are written as `(,)`, `(,,)`, etc.

3. *Type application*: if `t₁` is a type of kind `κ₁ → κ₂` and `t₂` is a type of kind `κ₁`, then `t₁ t₂` is a type expression of kind `κ₂`.

4. *Parenthesized type*, having form `(t)`, is identical to the type `t`.


*Special syntax* is provided to allow certain type expressions to be written in a more traditional style:

* A function type has the form `t1 -> t2`, which is equivalent to the type `(->) t1 t2`. Function arrows associate to the right.
* A tuple type has the form `(t1, … , tk)` where k ≥ 2, which is equivalent to the type `(,…,) t1 … tk` where there are k−1 commas between the parenthesis. It denotes the type of k-tuples with the first component of type t1, the second component of type t2, and so on.
* A list type has the form [t], which is equivalent to the type `[] t`. It denotes the type of lists with elements of type `t`.

> These special syntactic forms always denote the built-in type constructors for functions, tuples, and lists, regardless of what is in scope. In a similar way, the prefix type constructors `(->)`, `[]`, `()`, `(,)`, and so on, always denote the built-in type constructors; they *cannot be qualified*, nor mentioned in import or export lists (hence the special production, `gtycon`).

Although the list and tuple types have special syntax, their semantics is the same as the equivalent user-defined algebraic data types.

Notice that expressions and types have a consistent syntax. If `ti` is the type of expression or pattern `ei`, then the expressions `(\ e1 -> e2)`, `[e1]`, and `(e1,e2)` have the types `(t1 -> t2)`, `[t1]`, and `(t1,t2)`, respectively.

With one exception (that of the *distinguished type variable in a class declaration*), the type variables in a Haskell type expression are all assumed to be universally quantified; there is no explicit syntax for universal quantification [4]. For example, the type expression a -> a denotes the type ∀ a. a  →  a. For clarity, however, we often write quantification explicitly when discussing the types of Haskell programs. When we write an explicitly quantified type, the scope of the ∀ extends as far to the right as possible; for example, ∀ a. a  →  a means ∀ a. (a  →  a).
