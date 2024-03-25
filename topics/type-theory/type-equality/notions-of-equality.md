# Notions of equality in Haskell

## Equality of values

At the term level, equality more-less works as expected - equality of flat data types (like `Int`, `Double`, `Char`, etc.) is straightforward - two integers are equal if they signify the same number. The equality is tested with the Boolean-valued function (predicate) `==`, which is one of the two methods of the `Eq` class. In fact, the two methods, `==` and `/=`, are each other's inverses, so it's enough to define just one of them - the other will be defined by applying `not` to it, e.g. `==` is `not (/=)`.

Equality of compound data types depends on the equality of their undelying data types, so e.g. the equality of the list, Set, Maybe, etc., is

```hs
instance Eq a => Eq [a]
instance Eq a => Eq (Set a)
instance Eq a => Eq (Maybe a)

-- equality of products
instance (Eq a, Eq b) => Eq (a, b)

-- equality of sums
instance (Eq a, Eq b) => Eq (Either a b)
```

For two compound values to be equal, they must 1) first have the same type, 2) and be nominally equal. Because Haskell has nominal typing (as opposed to structural typing), two compound values meet the first condition only if their type ctors have the same name. For example, two records may have the matching types of all their fields, but in nominal typing they are still considered different if their type ctors are not the same (if they don't have the same name). In structural typing, such records would perhaps be equal, allowing for the record polymorphism.

The well-known type whose terms cannot be tested for equality is the function type. Product and sum types admit equality, but exponential types do not.



## Equality of types

In GHC, the equality relation on types has many flavors, some of which have opposite polarities: lifted vs unlifted, homogeneous vs heterogeneous.

"Lifted" means that the equality may be bogus (e.g. originating from a deferred type error). *Homogeneous equality* means that two types must also have the same kind to be equal. *Heterogeneous equality* relates types as equal even if their kinds are different.

## Coding the equality of types

We can use a GADT to express equality of types.

```hs
-- The (≡) data type
data (≡) a b where
  Refl :: (≡) a a

-- or, with (≡) in infix position
data a ≡ b where
  Refl :: a ≡ a
```

This data type has a binary type ctor `≡`, that accepts two type arguments, `a` and `b`. It has a single nullary data ctor `Refl` (for reflexivity). We must use a GADT so that we can vary the type arguments of the result type.

The full type of the `Refl` data ctor is:

```hs
Refl :: forall a b. (a ~ b) => a ≡ b
```

where `~` denotes the *type equality constraint*.

Type equality via a GADT: the only way to get the `Refl` is if the two types are equal, i.e. if both `a` and `b` are equal, so that both are in fact `a`.



Expanding the type equality data type to admit symmetry and transitivity along with reflexivity [but I'm not quite sure how to reason about it]:

```hs
data (≐) a b where
  Ref    ::                       (≐) a a
  Sym    :: (a ~ b)            => (≐) b a
  Trans  :: (a ~ b, b ~ c)     => (≐) a c

  Sym1   :: (≐) a b            -> (≐) b a
  Trans1 :: (≐) a b -> (≐) b c -> (≐) a c
```




## Homogeneous type equality

The lifted, homogeneous equality on types is denoted by `~`. 
It is actually a multiparameter class named `~`, and defined as:

```hs
-- Defined in GHC.Types
type (~) :: forall k. k -> k -> Constraint
class ((a :: k) ~ (b :: k)) => a ~ b
```

That is, the equality of two types `a` and `b` is restricted by them having the same kind. The type (param) `a` has the kind `k`, (a :: k), which must be equal to (b :: k), i.e. `(a :: k) ~ (b :: k)`.


Homogeneous type equality can be seen occasionally as a constraint. GADTs have this sort of equality "built in", such that a pattern match against a GADT data ctor also pulls into the scope the information about the specific type associated with it (as if the constraint, e.g. `(a ~ Int)` is given).

```hs
-- type equality as a constraint
add :: (i ~ Int, d ~ Double) => i -> d -> d
add i d = (fromIntegral i) + d
```



Haskell has many symbols and notions of equality. It has different notions of equality equivalently on the term as well as on the type level.


- (~)
- (~~)
- (:~:)
- (:~~:)


## The equality types in Haskell

https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Type-Equality.html




## Definitional equality

The equals sign (=) is a reserved symbol in Haskell, meaning "as defined by", so perhaps it could be understood as the *definitional equality*, at both term and type level. The equals sign is used to introduce definitions, that is, to bind expressions (values, functions) to identifiers, allowing you to reference them by name.

```hs
-- term level definitional equality
cons :: a -> [a] -> [a]
cons x xs = x : xs
--        ^ here

-- type level definitional equality
type (+) :: Nat -> Nat -> Nat
type family m + n where
    Z   + n = n
    S m + n = S (m + n)
--          ^ here
```






Term-level equality:
* equational equality, (=)
* Boolean equality,    (==)

Type-level equality:
* Boolean equality,              (===)  official (==)
* propositional homo   equality, (≡)    official (:~:)
* propositional hetero equality, (~)    official (:~~:)

Symbols for equality:
    (=)
    (==)
    (~)    (~~)
    (:~:)  (:~~:)




## Extensional and intensional equality

In mathematics, equality of two objects may be considered from the aspects of intensionality and extensionality.

*Extensionality* or *extensional equality* refers to principles that judge two objects as being equal if they share the same *external properties*.

*Intensionality* or *intensional equality* refers to principles that judge two objects as being equal if they share the same *internal properties*, which usually amounts to them having the same definition.



## Equality of functions

In mathematics, two functions are *extensionally equal* if they produce the same output given the same input. However, they may still be intensionally distinct if their definitions are different.

Two functions are *intensionally equal* if they have the same definition.




For infinite functions there is no way to compare them for equality using a computer. Computability theory and particularly *Rice's theorem* explains why:

  For any non-trivial property of partial functions, there is no general and effective method to decide whether a Turing machine computes a partial function with that property.

The formal proof involves showing that a method that can compare any two functions for equality can be used to build a method that decides whether any Turing machine will halt. Given that the Halting problem is undecidable, we know this is not possible with our computers that are equal to Turing machines in terms of theoretical computing power.

>Perhaps surprisingly, it is actually possible to compute *extensional equality* of total functions on compact spaces, but in general (e.g. functions on the integers with possible non-termination) it is not.


## Equality of type-level functions

Pattern matching on types works by unification, and unifying two types is a decidable procedure. Types can be compared structurally and then nominally to decide if they are equal. Value level functions, on the other hand, have no procedure for deducing if they are equal (except comparing all inputs and outputs, which is unpractical).

https://stackoverflow.com/questions/34543660/pattern-match-on-functions-on-the-type-level-is-possible-but-not-on-the-value-l?noredirect=1&lq=1

(correction) Briefly put, there is no pattern matching on type-level function values, but only on their name. In Haskell, as in many other languages, types are distinguished by their name, even if their representation is identical.

```hs
data A x = A Int x
data B x = B Int x
-- or
-- newtype A' x = A' (Int, x)
-- newtype B' x = B' (Int, x)
```

Above, `A` and `B` are two different type constructors, even if they describe the same type-level function, in pseudo-code `\x -> (Int, x)`, roughly. In a sense, these 2 identical type-level functions have a different name (identity).

This is different from

```hs
type C x = (Int, x)
type D x = (Int, x)
```

`C` and `D` both describe the same type-level function as `A` and `B`, but C and D don't introduce two new type names. They are only synonyms - they denote a function, but don't have their own distinct identity.

This is why one can add a class instance for `A x` or `B x`, but not for `C x` or `D x`: attempting to do the latter would add an instance to the type `(Int, x)` instead, relating the instance to the type names `(,)`, `Int` instead.

At the value level, the situation is not so much different. Indeed, there we have data constructors, which are special functions with a name/identity, and regular functions without an actual identity. We can pattern match against a data constructor, but not against much else (well …).

```hs
case expOfTypeA   of A n t -> ... -- ok
case someFunction of succ -> ...  -- no
```

Note that at the type level we can't do pattern matching that easily. Haskell allows to do that exploiting type classes, only. This is done to preserve some theoretical properties (parametricity), and to allow an efficient implementation as well (allowing type erasure - we don't have to tag every value with its type at runtime). These features come at the price of *restricting type-level pattern matching to type classes* - this does put some burden on the programmer, but the benefits outweigh the drawbacks.




## Intensional equality

In logic, extensionality, or extensional equality, refers to principles that judge objects to be equal if they have the same external properties.


## Extensional equality

http://foldoc.org/extensional+equality

In logic, intensionality, or intensional equality, refers to principles that judge objects to be equal if they have the same internal definition.

Two relations (functions) are said to be equal if they have the same extensions. In set-theoretical foundations of mathematics, it is common to identify relations and functions by their extension, so that it is impossible for two relations (or functions) with the same extension to be distinguished.

In set theory, the Axiom of Extensionality states that two sets are equal iff they contain exactly the same elements.


## The univalence axiom
