# Quantification

https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types


## Conjunction vs disjunction

- Universal quantification:   ∀xPx := Px₀ ∧ Px₁ ∧ … ∧ Pxₙ
- Existential quantification: ∃xPx := Px₀ ∨ Px₁ ∨ … ∨ Pxₙ
where `xᵢ ∈ D` (D is the domain of discourse)

In Haskell, the polymorphic type `forall a. a` represents the broadest possible type. The type variable `a` ranges across all Haskell types, and it is universally quantified, so the type means "every type", i.e. *everything* in the domain of discourse (DOD), (and members of the DOD are types).

~~In logic, this polymorphic type corresponds to the formula `∀x.x`, but this formula is ill-formed [*verification needed*], it should instead be `∀xPx`, where the predicate `P` could be interpreted as `x` is a type, `x : Type`. Then this formula means: for all `x`, `x` is a type.~~

Anyway, back to Haskell: if we need to produce (provide, pass in) a value of the type `forall a. a`, we are left holding our balls because there is no value (no reasonable value, at least) of this type.

The `forall` quantifier here universally quantifies the type varaible `a`, 
so the type `forall a. a` represents a value at the **intersection of all types**. And there is no (sensible) value at the intersection of all types, i.e. there is no value that belongs to any and all types.

In fact, one of the important ways in which type theory differs from set theory is that (with sets corresponding to types) an object (term, element) can belong to multiple sets, but a term (value, element, object) belongs exclusively to a single type (set). A type may correspond to a set, but a type is more than just a set of values.

However, in Haskell, there actually is a value at the intersection of all types, called *bottom* and denoted `⊥`. It is a term (value) that belongs to any and all types (~~which kinda defeats the difference between type and set theory we've just established~~). The bottom term was explictly added to each type in order to endow it with the capability to signal divergent computations.

It would be great is all functions were always *total*, accepting any element (term, value) from their specified domain, and always returning a legal element (term, value) from their specified codomain.

For example, a function like `division :: Int -> Int -> Int` is pure and dandy but only *most of the time*. Alas, when the second argument is zero, all it can do is blow up, returning `⊥` to signal an error.

```hs
division :: Int -> Int -> Int
division n 0 = error "Division by zero"
division n m = n / m
```

What it actually "returns" is the exception raised by calling the `error` function with an appropriate messsage.

```hs
>>> division 1 0
*** exception "Division by zero"
```

>Bottom can be explicitly denoted in Haskell using the term (value) `undefined` which inhabits every type.

The signature of `undefined` is exaclty the polymorphic type we started this discussion with:

```hs
undefined :: forall a. a
```

it confirms that `⊥` really does belong to any and all types; that the only value at the intersection of all types is bottom.

The implementation of the `error` function is intrinsic to the compiler, but its signature seems friendly enough:

```hs
error :: forall a. String -> a
error msg = -- ***raise exception, print msg, die.
```

There goes the infamous type `forall a. a` again, this time with a `String` argument, making `a` the type of the returned value.

This signature is just a little off from another well-known signature:

```hs
id :: forall a. a -> a
id x = x
```

However, there is huge difference between the functions `id` and `error`. The `id` is the friendly neighbour function everyone adores, while `error` nobody likes to mention until absolutely necessary.

```hs
ident :: forall a. a      -> a
error :: forall a. String -> a
```





## Relation between ∀, ∧ and ∩

- ∀xPx := Px₀ ∧ Px₁ ∧ … ∧ Pxₙ
- ∀tPt := Pt₀ ∧ Pt₁ ∧ … ∧ Ptₙ = ⋀{tᵢ}
- ∀tPt := Pt₀ ∩ Pt₁ ∩ … ∩ Ptₙ = ⋂{tᵢ}

∀xPx := Px₀ ∧ Px₁ ∧ … ∧ Pxₙ



## Relation between implication and conjunction

- `∀x(Px → Qx)` but
- `∃x(Px ∧ Qx)`



## Relation between ∧, →, ⇔ and equality

  10 xx pattern
- 10 00 conjunction, `∧`
- 10 01 biconditional, `⇔`
- 10 10 Q itself
- 10 11 implication, `→`

all agree in the upper part of the truth table - all have `10xx` - i.e. all are true if P and Q are both true, and all are false if P is true but Q is false. BTW, the remaining (fourth) possibility compling to the pattern `10xx` is the Q itself (1010).

```
P Q | ∧ → ⇔ | Q
-----------------
1 1 | 1 1 1  | 1
1 0 | 0 0 0  | 0
-----------------
0 1 | 0 1 0  | 1
0 0 | 0 1 1  | 0
```

This agreement of `∧, →, ⇔` also persists in the equations where these connectives are sometimes used interchangibly, along with the equality. 

Equality is sometimes expressed as implication or biconditional.

*Leibniz equality* says that two things are equal if they have all the same properties, which could be stated by the formula

`∀F.∀ab. (F a = F b) ⇒ (a = b)`

With "things" interpreted as types, and properties as predicates expressed as Haskell's unary type ctors, `f, g :: Type -> Type`, the Leibniz equality can be expressed in Haskell as the function

```hs
leibniz :: forall (f :: Type -> Type) a b. (f a ~ f b) => a -> b
```

The equality, `a = b`, has turned into the implication, `a -> b`. The other equality, `f a = f b`, did not turn into implication since the operator `~` in the context `(f a ~ f b)` actually does express equality on types.
