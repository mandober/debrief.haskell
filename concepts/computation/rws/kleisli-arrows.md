# Kleisli arrows

A category `C` with a monad `(T, η, μ)` gives rise to a Kleisli category based on the monad, denoted `Cᵀ`, that consists of
- the same objects, Ob(Cᵀ) = Ob(C)
- but weird morphisms, Cᵀ(c, d) = C(c, T d)
  `f : c ⇸ d` in Cᵀ corresponds to `f : c -> T d` in C

The identity morphism `Idᶜ : c ⇸ c` in the category `Cᵀ` is given by the unit (return) NT, `η : c -> T c` in the original category `C`.

In `Cᵀ`, the composition of arrows `f : a ⇸ b` and `g : b ⇸ c` should yield an arrow `a ⇸ c`. In the original cat C, this corresponds to arrows `f : a -> T b` and `g : b -> T c` and should yield an arrow `a -> T c`.


In Haskell, the Kleisli composition is achieved with the "fish" operator:

```hs
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
amb >=> bmc = \a ->
  let
    mb :: m b -> (b -> m c) -> m c  -- (>>=) :: m a -> (a -> m b) -> m b
    mb = amb a
  in  
```





Just like every category has an opposite category, it also gives rise to a Kleisli category based on it. In a category `C`, arrows are the regular arrows,like `a -> b`, but in a Kleisli category based on it, `K(C)`, those arrows correspond to "embellished" arrows of the form `a -> m b`.

A Kleisli arrow is an embellished function, generically typed as `a -> m b`. That is, the return value of a regular function like `a -> b` is "embellished" with an additional context `m`. The type param `m` stands for a monad, and each monad brings some specific capability to the table.

```hs
function :: a -> b
kleisli  :: a -> m b

maybe    :: a -> Maybe b
either   :: a -> Either e b
list     :: a -> [b]
pair     :: a -> (b, r)
io       :: a -> IO b
```

For example, the Maybe monad models a possibly failing computation, so instead of such computation being represented with the function `a -> b`, it gets the signature `a -> Maybe b`. If the reason of failure is important, then the signature becomes `a -> Either e b`, where `e` represents the error type. 

If the Maybe monad represents the possibility of having zero or one result, then we can model a more general situation of having any number of possible results by using the list monad, and the signature then becomes `a -> [b]`.

A computation that needs to return an extra value (besides the original payload) can do so by returning a pair, thus `a -> (b, w)`, where `w` stands for that extra data.

Side effects are managed within the IO monad, so the signature of computations, i.e. of arrows becomes `a -> IO b` .

>All these situations are instances of a general Kleisli arrow, with `m` representing different kinds of embellishments - different effects and capabilities.
