# Functional dependencies

* Functional dependencies
https://wiki.haskell.org/Functional_dependencies

* GHC: Functional dependencies
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#functional-dependencies


* *Type classes* allow programmers to define functions that can be used on a set of different types, with a potentially different implementation in each case. It is allowed to define new classes or to extend existing classes to include new, user-defined datatypes.

* Each class represents a set of types, and is associated with a particular set of *member functions*.

For example, the type class `Eq` represents the set of all equality types, which is precisely the set of types on which the (==) operator can be used. The use of type classes is reflected by allowing types to include predicates. The type of the equality operator is:

```hs
(==) :: Eq a => a -> a -> Bool

-- explicitly
(==) :: Eq a => ∀a. a -> a -> Bool
```

The type variable `a` used here represents an arbitrary type (bound by an implicit universal quantifier), but the predicate `Eq a` then restricts the possible choices for `a` to types that are in `Eq`.

More generally, functions in Haskell have types of the form `P ⇒ τ`, where `P` is some list of predicates (constraints) and `τ` is a *monotype*. If `P` is empty, then we usually just write `τ`. The presence of a predicate in a function's type indicates that an implicit parameter will be added to pass some appropriate evidence for that predicate at run-time. For example, we might use an implementation of equality on values of type `a` as evidence for a predicate of the form `Eq a`.

* In a predicate such as `Eq a`, we refer to `Eq` as the *class name*, and to `a` as the *class parameter*.

Haskell's syntax for constraints, which looks more like a curried function application, suggests that it might be possible to allow classes to have more than one parameter. Natural interpretation of a predicate of the form `R a b`, where two parameters `a` and `b` have been provided, is to interpret `R` as a two-place *relation between types*, and to read `R a b` as the assertion that `a` and `b` are `R`-related. This is a natural generalization of the one parameter case because sets are just one-place relations. More generally, we can interpret an n parameter class by an n-place relation on types.
