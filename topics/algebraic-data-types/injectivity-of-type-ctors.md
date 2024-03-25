# Injectivity of Haskell's type ctors
(from "Higher-order Type-level Programming in Haskell")

- injectivity of type ctors
- generativity of type ctors
- matchability of type ctors
- type families must be saturated (no partial application)



All Haskell's type ctors are **injective** and **generative**.

```hs
injMaybe :: forall a b. (Maybe a ~ Maybe b) => a -> b
injMaybe = id

injective :: forall (f :: Type -> Type) a b. (f a ~ f b) => a -> b
injective = id

>>> :type injective @Maybe
injective @Maybe :: t -> t

generative :: f a ~ g a => f Int -> g Int
generative = id
```

Application decomposition implies that type ctors are injective and generative.
* **Injectivity** : `f` is injective iff            `f a ~ f b -> a ~ b`
* **Generativity**: `f` and `g` are generative iff  `f a ~ g b -> f ~ g`
* **Matchability**: `f` is matchable iff f is injective and generative    
(the concept of matchability due to Eisenberg 2016)

∀f  ∀a∀b.   `(f a = f b) -> (a = b)`   <=>   f injective
∀f∀g∀a∀b.   `(f a = g b) -> (f = g)`   <=>   f,g generative


## Desc

<details><summary>Desc</summary>

  what that means is we have this function
  here where the context that says for all
  and B if we know that maybe a equals
  maybe B then we can write a function a
  to b and implement it as the identity
  function which is only possible if we
  somehow in the process manage to learn
  this a must be equal to B because the
  identity function doesn't change the type.
  `Maybe` is injective and actually every type constructor in Haskell is injective so we can even abstract over over maybe from the
  previous example and should say that `f` in `f a` of this current type to type is injective.
  and this still compiles as we
  expected and indeed if we say we look at
  the type of injective instantiated with
  with maybe four F.
  so we you can use
  visible type applications to provide
  maybe as the as the argument F.
  so just see when Indy tell us that oh yes I
  learned that it must be that the a and B
  must be the same.
  so all type of structures injective and all type
  restarts are also generated which is the
  other property of that is implied by
  application decomposition.
  so when we saw that F of a equals may be boo
  we learned that equals boo which is the
  injectivity property of being able to
  learn the ethical may be is the
  generativity property.
  so in this case when we know that F of a equals G of B
  we can again write the identity function
  I type it at F event - G event because
  GAC learned that F must be equal to G ok.

</details>


## Type families

Some type families are not injective

```hs
-- injective
Maybe a ~ Maybe b -> a ~ b

-- non-injective
DB a ~ DB b --/-> a ~ b

-- such that DB is some database related type
-- making the type ctors inherently unequal
```

Because type families are non-injective they have a big restriction -- *type families must be saturated*.
