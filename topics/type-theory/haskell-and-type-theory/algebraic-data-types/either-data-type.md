# Either data type

Haskell's `Either` data type is a sum type (a coproduct) holding a value of one of the two possible types. However, it doesn't completely correspond to the OR connective in logic. A logical disjunction `A ∨ B` is true if either `A` or `B` is true, but also if both are. Sum types do correspond to disjunction, but when we have a value of the `Either` data type, we know that it may only hold one of the two variants, not both (and not neither). This way, it actually corresponds more to the exclusive disjunction (XOR) that to the inclusive disjunction (OR).

Also, Haskell taints the Either type to only be used for computation that can possibly fail, but unlike with the `Maybe` type, we also want access to the failure message. By convention, the `Left` variant is used to signal error, while the `Right` variant holds the payload. Although the names of these ctors are completely arbitrary, now that they are established it would be strange for `Right` to be th error, but using the GADTs syntax they may have as well been named `Up` and `Down`. Anyway, to represent a true coproduct, it is better to define another sum type than to use `Either` that comes with the semantic baggage.

```hs
data Product a b where
  -- intro
  Product :: a -> b -> Product a b

-- elims
π₁ :: Product a b -> a
π₂ :: Product a b -> b

data Coproduct a b where
  -- intro
  In₁ :: a -> Coproduct a b
  In₂ :: b -> Coproduct a b

-- elim
coproduct :: forall c. (a -> c) -> (b -> c) -> Coproduct a b -> c
```
