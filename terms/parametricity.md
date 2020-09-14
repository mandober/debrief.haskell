# Parametricity

*Write down the definition of a polymorphic function on a piece of paper. Tell me its type, but be careful not to let me see the function’s definition. I will tell you a theorem that the function satisfies.*    
-- Philip Wadler, "Theorems for free"

Things that break parametricity:
- null
- exceptions
- Type-casing (isInstanceOf)
- Type-casting (asInstanceOf)
- Side-effects
- equals/toString/hashCode
- notify/wait
- classOf/.getClass
- General recursion


Polymorphism, and polymorphic code, has many kinds of which the best known are ad hoc, subtyping, and parametric polymorphism.

In Haskell, parametric polymorphism

CSE 505: Programming Languages - Lecture 15 — The Curry-Howard Isomorphism

Haskell uses at least two kinds of polymorphism, **ad hoc polymorphism** and **parametric polymorphism**.

This means that polymorphic functions must work uniformly across any input type, and it's this property that turned out to have some interesting implications for both authors and clients of such functions.

Parametric polymorphism is reified in function signatures by the presence of *type parameters*; they are usually represented by a single letter (`a`, `b`, `s`), which is not obligatory, but the small caps are. A type variable like `a` can stand for any type at all; it is said that it is unconstrained. It can be *instatiated* by any concrete type, e.g. `Either Bool String -> [Maybe [Int]]`. Any (non-function) type and any function type are conformant.

Type variables can be constrained so that they are more useful. They may be constrained just by placing them in a broader signature, e.g. `a -> a`, which is a type that describes the `id` function. In relation to the `id` function, or better said, the `id` function as is implemented, type checks if it has exactly this signature. On the other hand, this signature may be "attached" to different functions -- but whether it type checks depends on what the function does with the value (a value of some type `a`).

For example, the second function below won't type check:

```hs
-- this is ok
id :: a -> a
id x = x

-- NOT K
dui :: a -> a
dui x = x && x
{-
Couldn't match expected type ‘Bool’ with actual type ‘a’
‘a’ is a rigid type variable bound by the type signature for:
    dui :: forall a. a -> a
    • In the first argument of ‘(&&)’, namely ‘x’
      In the expression: x && x
      In an equation for ‘dui’: dui x = x && x
    • Relevant bindings include
        x :: a
        dui :: a -> a
-}
```

It doesn't work because the client gets to choose the type, and he is free to choose any type, most of which don't make sense here.

(and now the punchline)
A signature, like `a -> a`, actually guarantees that the function will work for any and all types, no matter what type the client chooses.

> A signature is a guarantee that a function will work for any and all types.


Parametric polymorphism in Haskell also disallows the so-called **type-casing** (case-analysis on a type), which means performing different things depending on the input type (often done in JS as a way to overload a function).

Such function can be written in JS or Java but not in Haskell. Haskell does not have anything like theirs `instanceof` operator: it is not possible to ask what type something is and decide what to do based on the answer.

One reason is that Haskell's types are *erased* by the compiler after being checked: at runtime, there is no type information around to query! (but there are other good reasons too).

This style of polymorphism is known as parametric polymorphism. We say that a function like `f :: a -> a -> a` is parametric in the type `a`. Here "parametric" just means "works uniformly for any type chosen by the caller" (in Java, this style of polymorphism is provided by generics).

Then what are the possible function impl that could have this signature? In fact, there are only two such implementations:

```hs
t :: a -> a -> a
t x y = x

f :: a -> a -> a
f x y = y


t1 :: a -> b -> a
t1 x y = x


f1 :: a -> b -> b
f1 x y = y
```

Functions cannot touch polymorphic content, so all they can do is return first or the second input value - hence only two such fns.


## Type classes

If a polymorphic type is a promise that the function will work for whatever type the caller chooses, a type class polymorphic function is a restricted promise that the function will work for any type the caller chooses, as long as the chosen type is an instance of the prescibed type class.

The important thing to note is that when (==) (or any type class method) is used, the compiler uses type inference to figure out which implementation of (==) should be chosen, based on the inferred types of its arguments. In other words, it is something like using an overloaded method in a language like Java.


Type classes are quite similar to Java interfaces. Both define a set of types/classes which implement a specified list of operations. However, there are a couple of important ways in which type classes are more general than Java interfaces:
* When a Java class is defined, any interfaces it implements must be stated right then and there. Type class instances, on the other hand, are declared separately from the declaration of the corresponding types, even in a separate module.
* The types that can be specified for type class methods are more general and flexible than the signatures that can be given for Java interface methods, especially with multi-parameter type classes.
* Type classes may define default implementation which the instances can overide with more efficient computations, if needed. Type classes may define many methods but only require a minimal set of them to be implemented, and the other ones will be defined in terms of those that are defined.
* Haskell type classes can also easily handle n-ary methods - there is no nice way to do this in Java: for one thing, one of the two arguments would have to be the "privileged", the one off of which plus method is called. This asymmetry makes n-ary methods awkward. Furthermore, because of Java's subtyping, two args of a certain interface type does not guarantee that their type-equality, which makes implementing binary operators cumersome.

Multi-parameter type classes can be enabled in GHC with a language pragma. For example, this hypothetical type class amounts to doing multiple dispatch - which implementation the compiler chooses depends on both `a` and `b`.

```hs
class Mutl a b where
  mult :: a -> b -> Bool
```
