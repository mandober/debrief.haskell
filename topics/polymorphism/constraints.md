# 6.10 Constraints

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/constraints.html

<!-- TOC -->

- [Loosening restrictions on class contexts](#loosening-restrictions-on-class-contexts)
- [Equality constraints and Coercible constraint](#equality-constraints-and-coercible-constraint)
  - [Equality constraints](#equality-constraints)
  - [Heterogeneous equality](#heterogeneous-equality)
  - [Unlifted heterogeneous equality](#unlifted-heterogeneous-equality)
  - [The Coercible constraint](#the-coercible-constraint)
- [The Constraint kind](#the-constraint-kind)
- [Kind queries in GHCi](#kind-queries-in-ghci)

<!-- /TOC -->


## Loosening restrictions on class contexts

- `FlexibleContexts`
- Since: 6.8.1
- Remove the type-variable restriction on class contexts.

The FlexibleContexts extension lifts the Haskell 98 restriction that the type-class constraints (anywhere they appear) must have the form: 
`(class type-var) =>` or `(class (type-var type₁ type₂ … typeₙ)) =>`.

With FlexibleContexts these type signatures are perfectly okay:

```hs
g :: Eq [a] => ...
g :: Ord (T a ()) => ...
```

This extension does not affect equality constraints in an instance context; they are permitted by `TypeFamilies` or `GADTs`.

Note that `FlexibleContexts` affects the *contextual use* of class constraints (in type signatures and other contexts). In contrast, `FlexibleInstances` loosens a similar restriction in place when *declaring a new instance*.

## Equality constraints and Coercible constraint

### Equality constraints

A type context can include equality constraint of the form `t1 ~ t2`, which denotes that the types `t1` and `t2` must unify.

In the presence of type families, in general, *whether two types are equal cannot be decided locally*.

Hence, the contexts of function signatures may include equality constraints, as in the following example, where we require that the element type of `c1` and `c2` are the same.

```hs
sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2
```

In general, the types `t1` and `t2` of an equality constraint may be arbitrary monotypes; i.e. they may not contain any quantifiers, independent of whether higher-rank types are otherwise enabled.


Equality constraints can also appear in class and instance contexts.

The former enable a simple translation of programs using functional dependencies into programs using family synonyms instead. The general idea is to rewrite a class declaration of the form:

```hs
class C a b | a -> b

-- into

class (F a ~ b) => C a b where
  type F a
```

That is, we represent every functional dependency (FD) `a1 .. an -> b` by an FD type family `F a1 .. an` and a superclass context equality `F a1 .. an ~ b`, essentially giving a name to the functional dependency.

In class instances, we define the type instances of FD families in accordance with the class head. Method signatures are not affected by that process.

### Heterogeneous equality

GHC also supports *kind-heterogeneous equality*, which relates two types of potentially different kinds.

Heterogeneous equality is spelled `~~`.

Here are the kinds of `~` and `~~` to better understand their difference:

```hs
(~)  :: forall k. k -> k -> Constraint
(~~) :: forall k1 k2. k1 -> k2 -> Constraint
```

Users will most likely want `~`, but `~~` is available if GHC cannot know, a priori, that the two types of interest have the same kind.

Evidence that `(a :: k1) ~~ (b :: k2)` tells GHC both, that `k1` and `k2` are the same and that `a` and `b` are the same.

Because `~` is the more common equality relation, GHC prints out `~~` like `~` unless `-fprint-equality-relations` is set.


### Unlifted heterogeneous equality

Internal to GHC is yet a third equality relation, `~#`, which is heterogeneous, like `~~`, but only used internally.

It may appear in error messages and other output only when `-fprint-equality-relations` is enabled.


### The Coercible constraint

The constraint `Coercible t1 t2` is similar to `t1 ~ t2`, but denotes *representational equality* between `t1` and `t2` in the sense of *Roles*.

It is exported by `Data.Coerce`, which also contains the documentation. More details and discussion can be found in the paper "Safe Coercions":    

"Safe Zero-cost Coercions for Haskell" 2016    
https://www.microsoft.com/en-us/research/uploads/prod/2018/05/coercible-JFP.pdf


## The Constraint kind

- `ConstraintKinds`
- Since: 7.4.1
- Allow types of kind `Constraint` to be used in contexts.

Normally, constraints (which appear in types to the left of the `=>` arrow) have a very restricted syntax. They can only be:
* Class constraints, e.g. `Show a`
* Implicit parameter constraints, e.g. `?x::Int` (with the `ImplicitParams` extension)
* Equality constraints, e.g. `a ~ Int` (with the `TypeFamilies` or `GADTs` extensions)

Equality constraints: 
https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/equality_constraints.html#equality-constraints


With the `ConstraintKinds` extension, GHC becomes more liberal in what it accepts as constraints in your program. To be precise, with this flag any type of the new kind `Constraint` can be used as a constraint.

The following things have the kind `Constraint`:

* Anything that is already valid as a constraint (without the extensions):

  1. saturated applications to type classes
    `Show _ :: Constraint`

  2. implicit parameter (with ImplicitParams)
    `(?x :: Int) :: Constraint`

  3. equality constraints (with TypeFamilies or GADTs)
    `(_ ~ Int) :: Constraint`

  4. Constraint tuples, all of whose component types have kind `Constraint`
    `(Show _, Ord _) :: Constraint`

5. Anything whose form is not yet known, but the user has declared to have the kind `Constraint` (needs the `Data.Kind` import).

For example, type aliases, like `Curb` below, are allowed, as well as the examples involving type families.

```hs
-- user-declared Constraint kind
type Curb (f :: Type -> Constraint) = forall b. f b => b -> b

-- Constraint kind and type families
type family Typ a b :: Constraint
type instance Typ Int  b = Show b
type instance Typ Bool b = Num b
func :: (Typ a b) => a -> b -> b
```

Since constraints are handled as types of a particular kind, the `ConstraintKinds` extension allows *type constraint synonyms*:

```hs
type Stringy a = (Read a, Show a)

foo :: Stringy a => a -> (String, String -> a)
foo x = (show x, read)
```

Presently, only standard constraints, tuples and type synonyms for those two sorts of constraint are permitted in instance contexts and superclasses (without extra flags).

The reason is that permitting more general constraints can cause type checking to diverge, as it would with these two programs:

```hs
type family Clsish u a
type instance Clsish () a = Cls a
class Clsish () a => Cls a where
class OkCls a where

type family OkClsish u a
type instance OkClsish () a = OkCls a
instance OkClsish () a => OkCls a where
```

You may write programs that use exotic sorts of constraints in instance contexts and superclasses, but to do so you must use `UndecidableInstances` to signal that you don't mind if the type checker fails to terminate.


## Kind queries in GHCi

```hs
-- unsaturated application to a type class are
-- not the kind Constraint but (Type -> Constraint):
:k Show
Show :: * -> Constraint

-- saturated application to a type class
:k Show _
Show _ :: Constraint

:k Monad
Monad :: (* -> *) -> Constraint

:k Monad _
Monad _ :: Constraint

:k Monad Maybe
Monad Maybe :: Constraint

:k MonadState
MonadState :: * -> (* -> *) -> Constraint

:k MonadState _ _
MonadState _ _ :: Constraint

-- tuples of saturated applications to type classes
:k (Show _, Ord _)
(Show _, Ord _) :: Constraint

:k (Ord _, Num _) :: Constraint
(Ord _, Num _) :: Constraint :: Constraint

-- () is overloaded to mean the empty tuple of constraints
:k () :: Constraint
() :: Constraint :: Constraint


-- implicit parameter (needs ImplicitParams)
:k (?x :: Int)
(?x :: Int) :: Constraint


-- Equality constraints (with TypeFamilies or GADTs)
:k (_ ~ Int)
(_ ~ Int) :: Constraint
```

## Quantified constraints

- `QuantifiedConstraints`
- Since: 8.6.1
- Allow constraints to quantify over types

The extension QuantifiedConstraints introduces quantified constraints, which give a new level of expressiveness in constraints.

For example:

```hs
data Rose f a = Branch a (f (Rose f a))

instance (Eq a, ???) => Eq (Rose f a) where
  (Branch x1 c1) == (Branch x2 c2) = x1 == x1 && c1 == c2
```

From the `x1 == x2` we need `Eq a`, which is fine. 
From the `c1 == c2` we need `Eq (f (Rose f a))` which is not fine 
in Haskell 2010, we have no way to solve such a constraint.

QuantifiedConstraints lets us solve this:

```hs
instance ( Eq a, forall b. (Eq b) => Eq (f b) ) 
        => Eq (Rose f a)
  where
  (Branch x1 c1) == (Branch x2 c2) = x1 == x1 && c1 == c2
```

Here, the quantified constraint `forall b. (Eq b) => Eq (f b)` behaves a bit like a *local instance declaration*, and makes the instance typeable.

The paper `Quantified class constraints` (by Bottu, Karachalias, Schrijvers, Oliveira, Wadler, Haskell Symposium 2017) describes this feature in technical detail, with examples, and so is a primary reference source for this feature.

## Motivation

Introducing quantified constraints offers two main benefits:

1. Firstly, they enable terminating resolution where this was not possible before. Consider for instance the following instance declaration for the general `Rose` datatype:

```hs
data Rose f x = Rose x (f (Rose f x))

instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
  (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2
```

This extension allows us to write constraints of the form `forall b. Eq b => Eq (f b)`, which is needed to solve the `Eq (f (Rose f x))` constraint arising from the second usage of the (==) method.

2. Secondly, quantified constraints allow for more concise and precise specifications. As an example, consider the MTL type class for monad transformers:

```hs
class Trans t where
  lift :: Monad m => m a -> (t m) a
```

The developer knows that a monad transformer takes a monad `m` into a new monad `t m`. But this property is not formally specified in the above declaration. This omission becomes an issue when defining monad transformer composition:

```hs
newtype (t1 * t2) m a = C { runC :: t1 (t2 m) a }

instance (Trans t1, Trans t2) => Trans (t1 * t2) where
  lift = C . lift . lift
```

The goal here is to lift from monad `m` to `t2 m` and then lift this again into `t1 (t2 m)`. However, this second lift can only be accepted when `t2 m` is a monad and there is no way of establishing that this fact universally holds.

Quantified constraints enable this property to be made explicit in the `Trans` class declaration:

```hs
class (forall m. Monad m => Monad (t m)) => Trans t where
  lift :: Monad m => m a -> (t m) a
```

This idea is very old; see Section 7 of Derivable type classes:

https://www.microsoft.com/en-us/research/publication/derivable-type-classes/
