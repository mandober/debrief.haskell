# 4.1.4 Semantics of Types and Classes

https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-670004.1.4

In this section, we provide informal details of the type system.

For more details see:
- [Jones][8] (1989) discusses constructor classes
- [Wadler and Blott][13] (1995) discuss type classes

Type system attributes a type to each expression in the program.

* In general, a type is of the form `∀ ū. cx => t`, 
  where `ū` is a set of type variables `u₁, …, uₙ`.

* In any such type, any of the universally-quantified type vars `uᵢ` 
  that are free in `cx` must also be free in `t`.

* Furthermore, the context `cx` must be of the form 
  as stated in the previous section (Section 4.1.3.).

Examples of valid types:

```hs
t1 :: Eq a => a -> a

t2 :: (Eq a, Show a, Eq b) => [a] -> [b] -> String

t3 :: (Eq (f a), Functor f) => (a -> b) -> f a -> f b -> Bool
```

In the last example, the constraint `Eq (f a)` cannot be made simpler because `f` is universally quantified.

The type of an expression e depends on a type environment that gives types for the free variables in e, and a class environment that declares which types are instances of which classes (a type becomes an instance of a class only via the presence of an instance declaration or a deriving clause).

Types are related by a generalization preorder (specified below); the most general type, up to the equivalence induced by the generalization preorder, that can be assigned to a particular expression (in a given environment) is called its principal type. Haskell's extended Hindley-Milner type system can infer the principal type of all expressions, including the proper use of overloaded class methods (although certain ambiguous overloadings could arise, as described in Section 4.3.4). Therefore, explicit typings (called type signatures) are usually optional (see Sections 3.16 and 4.4.1).

The type ∀ u. cx1  ⇒  t1 is more general than the type ∀ w. cx2  ⇒  t2 if and only if there is a substitution S whose domain is u such that:

    t2 is identical to S(t1)

Whenever cx2 holds in the class environment, S(cx1) also holds.
A value of type ∀ u. cx  ⇒  t, may be instantiated at types s if and only if the context cx[s/u] holds. For example, consider the function double:

    double x = x + x

The most general type of double is ∀ a. Num a  ⇒  a  →  a. double may be applied to values of type Int (instantiating a to Int), since Num Int holds, because Int is an instance of the class Num. However, double may not normally be applied to values of type Char, because Char is not normally an instance of class Num. The user may choose to declare such an instance, in which case double may indeed be applied to a Char.


[8]: M. P. Jones, `A system of constructor classes: overloading and implicit higher-order polymorphism`, 1995

[13]: P. Wadler and S. Blott, `How to make ad hoc polymorphism less ad hoc`, 1989
