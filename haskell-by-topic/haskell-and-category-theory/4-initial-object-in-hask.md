# Haskell :: Haskell and CT :: Initial object

## Initial object

>**The initial object** in a category is one which has a unique arrow to any other object in the category, including to itself via identity arrow.

It is not required that a category has initial object, some categories have one, other do not. Both `Set` and `Hask` have the initial object.

Initial objects
- in Set: `∅`
- in Hask: `Void` (canonical)
- in logic: `⊥`
- denoted: `0`, `𝟘`

In the category `Hask`, the initial object is any uninhabited (empty) type, similarly to how the empty set is the initial object in `Set`. However, while there is only one, unique empty set, denoted `∅`, among all sets, there are many types (objects) in Haskell that are empty - and all of them are candidates for the role of the terminal object.




Canonically, that role is assigned to the type `Void` from the module `Data.Void`. Any two uninhabited types are isomorphic - any could play the role of the initial object - and it would make no difference form the perspective of any other type. Since all uninhabited types are isomorphic, we usually talk about "the" initial object in Hask, as well as in other categories; in `Set`, however, the empty set is the initial object and it is already unique.

```hs
-- Void is an uninhabited type (invoke loose reasoning)
data Void

-- Void has a SINGLE UNIQUE arrow to each type
absurd :: forall a. Void -> a

vacuous :: forall f a. Functor f => f Void -> f a
```

(invoke: loose reasoning) The `Void` type is uninhabited - thus, there is nothing on the right-hand side. In fact, Haskell has plenty of empty types, but `Void` is the canonical one. Other empty types include any unsaturated type ctor, e.g. `Maybe` is uninhabited, and so is `Either String`. However, `Void` is the only *saturated uninhabited type*, i.e. its kind is `Type`, unlike the kind of unsaturated type ctors that is a kind function, like `Type -> Type`.

The `Void` type has a *single unique arrow* to each type in Hask, manifest by the function `absurd :: forall a. Void -> a`, which, unlike loose reasoning, can never be invoked. There is an infinite number of functions from `Void` since there is an infinite number of types in Haskell. However, for each type `a`, there is only one function `Void -> a`, made evident by considering that the cardinality of the exponential type `Void -> a` is `a⁰ = 1`.

The function `Void -> Void` is the single unique identity arrow on the `Void` object/type, justified by 0⁰ = 1, and defined as `id`, like all other identities.

```hs
-- Identity arrow at Void, id_𝟘: 𝟘 → 𝟘
idVoid :: Void -> Void
idVoid = id @Void

-- Identity arrow at Unit, id_𝟙: 𝟙 → 𝟙
idUnit :: () -> ()
idUnit = id @()
```

Functions in the other direction, `a -> Void`, are possible but almost never seen. A famous one of those is `negate :: forall a. a -> Void`, which is how negation is defined in inuitionistic logic, `¬P := P → ⊥`. In the light of CHI, negating a proposition (proving that it does not hold) is equal to proving that this function exists for that proposition - in IL, `⊥` is the uninhabited proposition (type). And a proposition `P` holds if `P` (as a type) is inhabited. The unit type, `⊤`, is inhabited, and thus always holds (and is always available; it represents 'true', 'truth'). So a category of logic, `⊥` is the initial object and `⊤` is the terminal object.
