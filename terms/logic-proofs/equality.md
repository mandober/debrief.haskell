# Equality

The notion of equality is so tricky that there is not one but many aspects of equality; stretching, skewing and moving the focus allows for different concepts of equality.

Equality of functions is particularly difficult to establish. Traditionally, there is extensional and intensional equality, originating in set theory, and the modern treatment prefers to deal with isomorphisms. An isomorphism is equality up to some particular point.

Equality relation is reflexive, symmetric and transitive, which are the same 3 axioms of any equivalence relation.

In Haskell, there are also many form of equality. The simplest is equality between two primitives values, denoted by the `==` operator, which takes two term-level values and returns a `Bool`. However, it cannot be used on functions.

Determining whether two functions are equal must involve the application of both functions, each to its corresponding arguments, which, if nothing else, stands in complete opposition to the language semantics and ubiquitous laziness.


+ Eq
+ a == b
+ Eq#
+ a ==# b
+ a ≅ b
+ a ~ b, (a ~ b) ~ 'True
+ a ~~ b
+ a :~: b
+ a :~~: b
+ ~#
- eqPrimTyCon,      `ty1 ~# ty2`
- eqReprPrimTyCon,  `ty1 ~R# ty2` (at role Representational)
- eqPhantPrimTyCon, `ty1 ~P# ty2` (at role Phantom)



* Type comparison from `src/Type`
eqType
eqTypeX
eqTypes
nonDetCmpType
nonDetCmpTypes
nonDetCmpTypeX
nonDetCmpTypesX
nonDetCmpTc
eqVarBndrs

https://downloads.haskell.org/~ghc/8.2.1/docs/html/libraries/ghc-8.2.1/src/Type.html

* Equality and ordering
  - Eq(..)
  - Ord(..)
* Monomorphic equality operators
  $matching_overloaded_methods_in_rules
  - eqInt, neInt
  - eqWord, neWord
  - eqChar, neChar
  - eqFloat, eqDouble
* Monomorphic comparison operators
  - gtInt, geInt, leInt, ltInt, compareInt, compareInt#
  - gtWord, geWord, leWord, ltWord, compareWord, compareWord#



## The equality types

`Data.Type.Equality`

- The equality types
  - propositional equality
    - kind-heterogeneous propositional equality
    - kind-homogeneous propositional equality
    - lifted heterogeneous equality
- Working with equality
- Inferring equality from other types
- Boolean type-level equality


See Note *The equality types story* in `GHC.Builtin.Types.Prim`


The module `Data.Type.Equality` defines two equality types:

### Kind-homogeneous propositional equality `:~:`

Pattern-matching on a variable of type `a :~: b` produces a proof that `a ~ b`.

```hs
data a :~: b where
  Refl :: a :~: a
infix 4 :~:
```

If `a :~: b` is inhabited by some terminating value, then the type `a` is the same as the type `b`. To use this equality in practice, pattern-match on the `a :~: b` to get out the `Refl` constructor; in the body of the pattern-match, the compiler knows that `a ~ b`.


### Lifted heterogeneous equality

Lifted, heterogeneous equality. By lifted, we mean that it can be bogus (deferred type error). By heterogeneous, the two types a and b might have different kinds. Because ~~ can appear unexpectedly in error messages to users who do not care about the difference between heterogeneous equality ~~ and homogeneous equality ~, this is printed as ~ unless -fprint-equality-relations is set.

```hs
class a ~~ b => (a :: k) ~ (b :: k)

class a ~# b => (a :: k0) ~~ (b :: k1)
```

See Note: *The equality types story* in `TysPrim` module.

NB: All this class does is wrap its superclass, which is the "real", *inhomogeneous equality*; this is needed when we have a *Given* `(a ~ b)`, and we want to prove things from it.

NB: Not exported because (`~`) is magical syntax. That's also why there's no fixity.

It's tempting to put functional dependencies on (`~`), but it's not necessary because the functional-dependency coverage check looks through superclasses, and (`~#`) is handled in that check.


### Kind-heterogeneous propositional equality

Like `:~:`, `a :~~: b` is inhabited by a terminating value iff `a` is the same type as `b` and their types have the same kind.

```hs
data (a :: k1) :~~: (b :: k2) where
  HRefl :: a :~~: a
```



## The equality types story

GHC sports a veritable menagerie of equality types:

Type or Lifted? Hetero? Role Built in Defining module class? 

L/U TyCon ~ C L homo nominal eqTyCon GHC.Types :~: T L homo nominal (not built-in) Data.Type.Equality :~~: T L hetero nominal (not built-in) Data.Type.Equality ~R# T U hetero repr eqReprPrimTy GHC.Prim Coercible C L homo repr coercibleTyCon GHC.Types Coercion T L homo repr (not built-in) Data.Type.Coercion ~P# T U hetero phantom eqPhantPrimTyCon GHC.Prim

Recall that "hetero" means the equality can relate types of different kinds.

Knowing that (t1 ~# t2) or (t1 ~R# t2) or even that (t1 ~P# t2) also means that (k1 ~# k2), where (t1 :: k1) and (t2 :: k2).

To produce less confusion for end users, when not dumping and without `-fprint-equality-relations`, each of these groups is printed as the bottom-most listed equality. That is, (~#) and (~~) are both rendered as (~) in error messages, and (~R#) is rendered as Coercible.


Let's take these one at a time:

This is The Type Of Equality in GHC. It classifies nominal coercions. This type is used in the solver for recording equality constraints. It responds "yes" to Type.isEqPrimPred and classifies as an EqPred in Type.classifyPredType.

All wanted constraints of this type are built with coercion holes.
(See Note *Coercion holes* in TyCoRep and the note *Deferred errors for coercion holes* in TcErrors to see how equality constraints are deferred).

* Within GHC, `~#` is called `eqPrimTyCon`, and it is defined in `TysPrim`.

This is (almost) an ordinary class, defined as if by

```hs
class a ~# b => a ~~ b
instance a ~# b => a ~~ b
```

Here's what's unusual about it:

- We can't actually declare it that way because we don't have syntax for `~#`. And `~#` isn't a constraint, so even if we could write it, it wouldn't kind check.
- Users cannot write instances of it.
- It is "naturally coherent". This means that the solver won't hesitate to solve a goal of type (a ~~ b) even if there is, say (Int ~~ c) in the context. (Normally, it waits to learn more, just in case the given influences what happens next). See the note *Naturally coherent classes* in TcInteract.
- It always terminates. That is, in the `UndecidableInstances` checks, we don't worry if a (~~) constraint is too big, as we know that solving equality terminates.

On the other hand, this behaves just like any class wrt eager superclass unpacking in the solver. So a lifted equality 'givens' quickly becomes an unlifted equality 'givens'. This is good, because the solver knows all about unlifted equalities. There is some special-casing in TcInteract.matchClassInst to pretend that there is an instance of this class, as we can't write the instance in Haskell.


* Within GHC, `~~` is called `heqTyCon`, and it is defined in `TysWiredIn`.

This is exactly like (`~~`), except with a homogeneous kind. It is an almost-ordinary class defined as if by

```hs
class a ~# b => (a :: k) ~ (b :: k)
instance a ~# b => a ~ b
```

- All the bullets for (~~) apply
- In addition (~) is magical syntax, as ~ is a reserved symbol. It cannot be exported or imported.
- Within GHC, ~ is called eqTyCon, and it is defined in TysWiredIn.

Historical note: prior to July 18 (~) was defined as a more-ordinary class with (~~) as a superclass. But that made it special in different ways; and the extra superclass selections to get from (~) to (~#) via (~~) were tiresome. Now it's defined uniformly with (~~) and Coercible; much nicer.

```hs
(:~:)  :: forall k.     k  -> k  -> *
(:~~:) :: forall k1 k2. k1 -> k2 -> *
```

These are perfectly ordinary GADTs, wrapping (~) and (~~) resp. They are not defined within GHC at all.

The is the representational analogue of ~#. This is the type of representational equalities that the solver works on. All 'wanted' constraints of this type are built with coercion holes.

* Within GHC, `~R#` is called `eqReprPrimTyCon`, and it is defined in TysPrim.

This is quite like (~~) in the way it's defined and treated within GHC, but it's homogeneous. Homogeneity helps with type inference (as GHC can solve one kind from the other) and, in my (Richard's) estimation, will be more intuitive for users.

An alternative design included HCoercible (like (~~)) and Coercible (like (~)). One annoyance was that we want coerce :: Coercible a b => a -> b, and we need the type of coerce to be fully wired-in. So the HCoercible/Coercible split required that both types be fully wired-in. Instead of doing this, I just got rid of HCoercible, as I'm not sure who would use it, anyway.

* Within GHC, Coercible is called coercibleTyCon, and it is defined in TysWiredIn.

This is a perfectly ordinary GADT, wrapping `Coercible`. 
It is not defined within GHC at all.

This is the phantom analogue of `~#` and it is barely used at all. 
(the solver has no idea about this one). 
Here is the motivation:

```hs
data Phant a = MkPhant type role Phant phantom

Phant <Int, Bool>_P :: Phant Int ~P# Phant Bool
```

We just need to have something to put on that last line. You probably don't need to worry about it.

`State#` is the primitive, unlifted type of states. It has one type parameter, thus `State# RealWorld` or `State# s`, where s is a type variable. The only purpose of the type parameter is to keep different state threads separate. It is represented by nothing at all.

The type parameter to `State#` is intended to keep separate threads separate. Even though this parameter is not used in the definition of `State#`, it is given role Nominal to enforce its intended use.


> from: https://ghc-compiler-notes.readthedocs.io/en/latest/notes/compiler/prelude/TysPrim.hs.html
4
