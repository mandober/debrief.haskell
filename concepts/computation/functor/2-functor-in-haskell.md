# Functors in Haskell

In Haskell, a functor is a unary type ctor `f :: Type -> Type` that maps types (objects) to types, as well as mapping functions to functions (from the source category to target category, e.g. within Hask). For each type `a`, the type ctor `f` produces a type `f a`.


In Haskell, functors are realized as a type class with the `fmap` method that converts `a`'s to `b`'s in a functor by applying (lifting) a function `a -> b`.

```hs
fmap :: Functor f => (a -> b) -> (f a -> f b)

(<$>) :: Functor f => (a -> b) -> (f a -> f b)

($>) :: Functor f => b -> (f a -> f b)
```

>update vs overwrite

`<$>` updates the contents of `f a` by applying the function `a -> b` to each item of `f a` (each item has the type `a`).

`$>` overwrites the contents of `f a` just be replacing each item with `b` (but it doesn't even look at the items of type `a`).




## Covariant functors

Covariant Functor or just Functor, as opposed to a Contravariant Functor, or just Contravariant.

In CT, functors are structure-preserving mappings between categories. Given a source category `𝒞` and a target category `𝒟`, a functor `F : 𝒞 -> 𝒟` maps:
- objects to objects
  - it maps objects in `𝒞` to objects in `𝒟`
  - `∀ a ∈ Ob(𝒞). ∃ F a ∈ Ob(𝒟)`
- arrows to arrows
  - it maps arrows in `𝒞` to arrows in `𝒟`
  - `∀ a b ∈ Ob(𝒞). ∃ (f : a → b) ∈ Ar(a,b). ∃ H f ∈ Ar(F a, F b)`
    - it maps source objects of arrows in 𝒞 to source objects of arrows in 𝒟
      - `Ar(a,-)` then `Ar(F a, -)`
    - it maps target objects of arrows in 𝒞 to target objects of arrows in 𝒟
      - `Ar(-,b)` then `Ar(-, F b)`
    
    
    - such that
      - `Ar(a,b)` then `Ar(F a, F b)`
      - `f : a → b ∈ Ar(a,b)` then `H f ∈ Ar(F a, F b)`
      - `f ∈ Ar(𝒞) ⟼ H f ∈ Ar(𝒟)`

  - arrows in 𝒞 to arrows in 𝒟
  - functors preserve composition and id:
    - id arrows in 𝒞 to id arrows in 𝒟 such that 1ᵃ ⟼ 1ꜰₐ = F 1ᵃ
    - g ∘ f ⟼ F (g ∘ f) = F g ∘ F f

  In Haskell, a functor is type ctor `f` (arrow as a type function) along with the `fmap` method. It is a structure-preserving mapping from Hask to Hask, that is, all Haskell functors are endofunctors.
    H : Haskᶜ -> Haskᴰ
    ∀  a  b  c ∈ Ob(Haskᶜ)
    ∀ Ha Hb Hc ∈ Ob(Haskᴰ)

    ∀ f : a -> b ∈ Ar(Haskᶜ)
    ∀ g : b -> c ∈ Ar(Haskᶜ)

    a ⟼ H a
    b ⟼ H b
    c ⟼ H c

    1ᵃ ⟼ H 1ᵃ = 1ʜᵃ
    1ᵇ ⟼ H 1ᵇ = 1ʜᵇ
    1ᶜ ⟼ H 1ᶜ = 1ʜᶜ

    f ⟼ H f
    g ⟼ H g
    g ∘ f ⟼ H (g ∘ f) = H g ∘ H f
