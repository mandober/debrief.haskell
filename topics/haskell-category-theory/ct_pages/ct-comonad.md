# Comonads in Category theory and Haskell

Defining a comonad in category theory is a straightforward exercise in duality. As with the monad, we start with an endofunctor `T`. The two natural transformations, η and μ, that define the monad are simply reversed for the comonad:

```js
ε :: T -> I
δ :: T -> T2
```

The components of these transformations correspond to `extract` and `duplicate`. Comonad laws are the mirror image of monad laws. No big surprise here.

Then there is the derivation of the monad from an adjunction. Duality reverses an adjunction: the left adjoint becomes the right adjoint and vice versa. And, since the composition `R ∘ L` defines a monad, `L ∘ R` must define a comonad. The counit of the adjunction:

```hs
ε :: L ∘ R -> I
```

is indeed the same ε that we see in the definition of the comonad - or, in components, as Haskell's `extract`. We can also use the unit of the adjunction:

```hs
η :: I -> R ∘ L
```

to insert an `R ∘ L` in the middle of `L ∘ R` and produce `L ∘ R ∘ L ∘ R`. Making `T2` from `T` defines the δ, and that completes the definition of the comonad.

We've also seen that the monad is a monoid. The dual of this statement would require the use of a comonoid, so what's a comonoid? The original definition of a monoid as a single-object category doesn't dualize to anything interesting. When you reverse the direction of all endomorphisms, you get another monoid. Recall, however, that in our approach to a monad, we used a more general definition of a monoid as an object in a monoidal category. The construction was based on two morphisms:

```hs
μ :: m ⊗ m -> m
η :: i -> m
```

The reversal of these morphisms produces a *comonoid in a monoidal category*:

```hs
δ :: m -> m ⊗ m
ε :: m -> i
```


One can write a definition of a comonoid in Haskell, but it's trivial.

```hs
class Comonoid m where
  split   :: m -> (m, m)
  destroy :: m -> ()


-- Obviously destroy ignores its arg:
destroy \_ = ()

-- split is just a pair of functions:
split x = (f x, g x)
```


Now consider comonoid laws that are dual to the monoid unit laws.

```js
lambda . bimap destroy id . split = id
rho . bimap id destroy . split = id
```

Here, `lambda` and `rho` are the *left and right unitors*, respectively (see the definition of [monoidal categories](https://bartoszmilewski.com/2016/12/27/monads-categorically/)).

Plugging in the definitions, we get:

```js
  lambda (bimap destroy id (split x))
= lambda (bimap destroy id (f x, g x))
= lambda ((), g x)
= g x
```

which proves that `g = id`. Similarly, the second law expands to `f = id`. In conclusion:

```hs
split x = (x, x)
```

which shows that in Haskell (and, in general, in the category **Set**) every object is a trivial comonoid.


Fortunately there are other more interesting monoidal categories in which to define comonoids. One of them is the category of endofunctors. And it turns out that, just like the monad is a monoid in the category of endofunctors,

> The comonad is a comonoid in the category of endofunctors.



Challenge: Implement the Conway's Game of Life using the `Store` comonad. Hint: What type do you pick for `s`?



Next: [F-Algebras](https://bartoszmilewski.com/2017/02/28/f-algebras/)
