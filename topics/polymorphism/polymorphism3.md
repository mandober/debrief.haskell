## Polymorphism

Polymorphism is a property of language entities that do not have one single fixed type; they have more than one type, and it can be said that they may be instantiated with different types (although the term "instantiation" is associated with exactly polymorphic entities, that is, monomorphic entities are not instantiated, they just are of some fixed type.)

Numeric literals are polymorphic values. Consider this example:

```hs
one :: forall a. (Num a) => a
one = 1
```

The variable `one` binds the number but also "a number". Since number literals are polymorphic, the numeral "1" can stand for many different types of numbers, it doesn't identify a unique number, hence the lack of the determiner "the". It is strage how, although it has a flexible type, its value remains fixed. It is as if the numeral itself acts as a variable that ranges over the set of number types (Integer, Float, Double, Ratio, Int, Word, etc.).

On the other hand, it cannot be typed as just `a`. Or to simplify things, by switching 1 with 'c', which is uniquely typed as a `Char`: the expression beneath would then mean that 'c' is a member of any type at all.

More precisely: let `a` be any type at all; then, the value 'c' is a member of the type `a`, and hence of any type. This is why the only value that would type check without changing the signature, is `undefined`. It is because only `undefined` has the required type, `undefined :: a`.    
*Only "undefined" is a sufficiently polymorphic value*.

```hs
all :: forall a. (a :: *)
all = 'a'         -- type error: 'a' is a Char, not any type at all

any :: forall a. (a :: *)
any = undefined   -- ok: undefined is indeed any type at all

one :: forall a. (Num a) => (a :: *)
one = 1           -- ok: 1 is indeed any numeric type at all
```

> `'c' ∈ Char` and `Char ∈ Type`, but `'c' ∉ Type`

```hs
1 ∈ Int, 1 ∈ Float
Int ∈ Num, Float ∈ Num
Int ∈ Type, Float ∈ Type
Num ∉ Constraint, Num ∈ (Type -> Constraint)
Constraint ∈ Type

:k Int
Int :: *
:k Num
Num :: * -> Constraint
:k Num Int
Num Int :: Constraint
```
