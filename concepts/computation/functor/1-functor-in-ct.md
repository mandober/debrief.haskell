# Haskell :: Concepts :: Functors

Functors are one of the central concepts in category theory, where a functor is a structure-preserving map between categories, that maps objects and arrows in the source category `𝒞`, to objects and arrows in the target category `𝒟`. Thus, this functor is denoted by `F ∶ 𝒞 -> 𝒟`. A functor preserves structure by preserving composition of arrows and the identity arrows.

## Definition

- A functor `F ∶ 𝒞 -> 𝒟` is a mapping between categories `𝒞` and `𝒟`
- `F` maps objects and arrows in the source category `𝒞`
  to objects and arrows in the target category `𝒟`
- `F` maps each object `X` in `𝒞` 
  to an object `F(X)` in `𝒟`, 
- `F` maps each morphism `f : X -> Y` in `𝒞` 
  to a morphism `F(f) : F(X) -> F(Y)` in `𝒟` 
  such that these two conditions hold: 
  1. ∀X ∈ 𝒞. `F(1ₓ) = 1ꜰₓ`
  `F` preserves identity morphisms 
  by mapping each object's (`m`) id arrow (`1ₘ`), `1ₘ ⟼ 1ꜰₘ`
  2. ∀f,g ∈ 𝒞. `f : X → Y` and `g : Y -> Z`, `F (g ∘ f) = F(g) ∘ F(f)` in 𝒟
  `F` preserves composition of morphisms 
  by mapping compositions of arrows in C to compositions of arrows in D.


## Element involved

```
categories
  C, D
functor F
  F : C -> D
objects in C
  a, b, c ∈ Ob(C)
arrows in C
  f, g ∈ Ar(C)
  1ₘ ∈ Ar(C)
objects in D
  F a, F b, F c ∈ Ob(D)
arrows in D
  F f, F g ∈ Ar(D)
  F 1ₘ = 1ꜰₘ ∈ Ar(D)
-----------------------
∀m ∈ Ob(𝒞). m ⟼ F m
∀m ∈ Ob(𝒞). 1ₘ ⟼ 1ꜰₘ
∀f,g ∈ Ar(𝒞). f ⟼ F f, g ⟼ F g 
-----------------------
∀ f g ∈ Ar(𝒞).
  f : a -> b
       g : b -> c
-----------------------
∃ F f, F g ∈ Ar(𝒟).
  F f : F a -> F b
         F g : F b -> F c
-----------------------
  F (g ∘ f) = F g ∘ F f


               𝒞 ------F----> 𝒟
∀m ∈ Ob(𝒞)     m -----------> F m
∀m ∈ Ob(𝒞)    1ₘ -----------> 1ꜰₘ
```

## Tactic Metaprogramming in Haskell

A New Kind of Programming: Tactic Metaprogramming in Haskell 
by Sandy Maguire, @ZuriHac21, Oct 2021
https://www.youtube.com/watch?v=BuEn1J90bKg

When we define a new data type in Haskell, we sort of "have a gut feeling" whether it can be made an instance of the Functor class, that is, whether it could really obey the functor laws.

```hs
data Foo s a
  = Foo s a (s -> a)
  | Bar [(Int, a)]

instance Functor (Foo s) where
  fmap :: (a -> b) -> Foo s a -> Foo s b
  fmap fab (Foo s a fsa) = Foo s (fab a) (fmap fab fsa)
--fmap fab (Foo s a fsa) = Foo s (fab a) (fab . fsa)
  fmap fab (Bar x)       = Bar           (fmap (fmap fab) x)
```

So, the question is how do we really know this is, in fact, the law-obeying functor instance?

## The Functor laws

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b

fmap id === id                    -- preserves identity
fmap g . fmap f === fmap (g . f)  -- preserves composition


Law 1: F 1ₘ = 1ₘ
Law 2: F (f ∘ g) = F f ∘ F g

LawHs 1: fmap id = id
LawHs 2: fmap (f ∘ g) = fmap f ∘ fmap g
```



If you're into academia, you might mention something about theorems-for-free, or how this is the only reasonable definition that type-checks. And those that are more categorically inclined, might reason about the naturality conditions and check if this instance satisfies them. Sandy suspects that nobody really looks at this and immediately imagines a commuting diagram, proceeding to chase the arrows to ensure that everything checks out. (oh no, oh why. Sandy… suspend the disbelief this instance!).

To make our intuition about what a correct functor instance look like, let's remind ourselves that a functor takes a function `fab : b -> g` and some kind of structure, `f b`, mapping all the bees to geez, producing a functorful of jizz, `f g`. It must not do anything else - that's the structure-preserving part, in the example reflected by the fact that a lhs data ctor is mirrored on the rhs of equations (`Foo` as a pattern on the lhs deconstructs the value, but then `Foo`, the data ctor, builds it back up on the rhs; same for `Bar`).

```hs
fmap :: Functor f => (b -> g) -> f b -> f g
```

Therefore, this should become one of the things to verify to make sure you have a legal Functor instance:

> If the LHS deconstructs a value, the RHS must reconstruct it back up.

Mirroring of the same data ctor around the equal sign in an equation, but only in case the lhs uses a pattern to deconstruct a value (as opposed to using the accessor function on the rhs).

A functor is structure-preserving because it only touches (maps) the `a`'s and must not touch anything else. Another consequence of "not doing anything else" is that it must not change the "current" data constructors.
If I'm `f`-mapping over something, then if I'm only allowed to change the `a`'s, then a data constructor is not n a, so I can't change it. Therefore I must map Foo to Foo, and Bar to Bar.

Another thing that i'm not allowed to do is to shuffle the arguments around.
that means if I unpack arguments I have to repack them in the same order.

so it goes s a f s a and then s a f s a
and x zero again x zero


for every hole:
- (1) if it doesn't mention `a`, it must be left alone
- (2) if a hole is `a`, then it may (must) be directly mapped (by `fab`)
- (3) if a hole mentions `a`, i.e. `a` is nested within some subexp within, then we use `fmap` (some number of times) to lift the function `fab`, in order to reach that `a` and map it.


The first equation from the example shows all the 3 cases:

```hs
fmap fab (Foo s  a fsa) = Foo s  (fab a) (fmap fab fsa )
                              ↑       ↑   ↑
                             (1)     (2) (3)

fmap fab (Bar x)        = Bar (fmap (fmap fab) x)
                               ↑     ↑
                              (3) lifting fab twice
```

The `Bar` case shows how we must use `fmap` twice to lift `fab` so it gets at the `a` that is the second component of a pair inside a list. Recall that a pair `(r, a)` is a Functor as `(,) r`, so its first component is fixed, while the second element varies. Thus, e.g.

```hs
p1 = (3, 'c')
x1 = length p1   -- 1 (not 2!)
x2 = fmap ord p1 -- 99
```

Again, if `a` is the current exp, then we apply `fmap` zero times - that is, we don't use it since we can just apply the function directly to the value of type `a`. But, if `a` is nested within a data structure, then we use `fmap` once for each nesting structure that is in our way, until we reach the `a` value. `fmap` sort of opens the doors for us, granting us access through all the data structures that enclose `a`.
