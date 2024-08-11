# Dependent types

Haskell is not (still) a dependently-typed language, but some aspects of dependent types can be modeled (primarily using singleton types).

At compile-time, types are even more important then values, but at run-time, values are what is important, and types are erased by then anyway. Dependent types may be exemplified as functions from values to types, so, in Haskell, this would be like time travel - going from run-time back to compile-time. This is why, in Haskell, we have to reify types, so they are available at runtime as values. This can only be done with singletons - types with a single value - since that is the only way we can infer the value from a type.

In truly dependently-typed languages, types can be selectively erased, and the distinction between values and types is too blurred there anyway. For one, it suffices to define e.g. the type of natural numbers once, at the term-level, to use naturals (as values) at the type-level. This is not possible in Haskell - we have to (re)define naturals at type-level, which are completely divorced from term-level naturals; each and every operation on naturals as well, amounting to a lot of duplication.

## Categories of functions

Categories of functions:
1. from terms to terms (vanilla)
2. from types to types (type ctors)
3. from types to terms (polymorphism)
4. from terms to types (dependent types)

### Functions from terms to terms

Functions from terms to terms are monomorphic functions, operating only over concrete types. Values are taken as arguments and values are returned. Functions in other categories have fancier ways of abstraction, but it all starts with (monomorphic) functions here, that abstract over values. After all, that's what functions are - abstractions over values. They allow us to operate over a range of values instead of only working with the constants.

odd : Nat -> Bool


### Functions from types to types

Type ctors.

A : Type

List : A -> List A
Int ⟼ List Int

Maybe : A -> Maybe A
Char ⟼ Maybe Char

Either : (A × B) -> Either A B
(String, Bool) ⟼ Either String Bool

(->) : (A × B) -> (A -> B)
(Nat, Bool) ⟼ (λ(n:Nat). odd n) : Nat -> Bool


### Functions from terms to types

Polymorphic functions.

Abstracting the types gives rise to polymorphism (generics) where functions take both types and terms as arguments.

```
id : ∀(A:T). A -> A
id Nat 1 = 1
```

Polymorphic functions take arguments that are types and values. A unary polymorphic function like `id`, first takes a type that then restricts the second, value, argument. Haskell allows us to skip supplying the type argument since it can be inferred from the value (most of the time).

```hs
id :: forall a. a -> a
id @Int 1 = 1

-- specializing polymorphic to monomorphic function
idInt = id @Int
-- now idInt is a monomorphic functions from category (1)
```


### Functions from types to terms

Dependent types.
