# Higher-rank type

- rank-2 type, rank-n type, higher-rank type (HRT)
- Higher-rank types are functions that take polymorphic functions as their args
- Complete type inference is known to be undecidable for HRT (impredicative) type systems



```hs
-- (show a-type-of-thing) ++ (show b-type-of-thing)
combine :: ∀a b. (Show a, Show b) => a -> b -> String
combine x y = show x ++ show y

-- now, we'd like to not hardcode `show` as the conversion fn (a -> String)
-- but allow users to specify such function `f`
-- (f a-type-of-thing) ++ (f b-type-of-thing)

-- Instead of writing it the proper way, with `b` type replaced with `a`
-- coz a fn cannot take a and b type of input arg, it's a xor situation
combain :: ∀a. (Show a) => (a -> String) -> a -> a -> String
combain f a b = f a ++ " " ++ f b

-- but what is wrong in writing it like this?
customCombine :: (Show a, Show b, Show c) => (c -> String) -> a -> b -> String
customCombine f x y = f x ++ f y

-- the type of fn (c -> String) surely is general enough to instantiate
-- both (a -> String) and (b -> String)
-- Hindley-Milner doesn't allow functions which take polymorphic functions
```

> The problem is that by using different (rigid) type variables for different args, the type checker refuses to unify them (i.e. to treat them as the same concrete type).

In particular, if we've specified in the type signature that f is of type c -> String and x is of type a, the type checker won't allow us to apply f to x, because it can't prove that c represents the same type as a.


## Practical type inference for arbitrary-rank types
Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields, 2007

The variance of a type `T a` with respect to its type variable `a` is fully specified by the position of `a`:
* if `a` appears in positive position it is *covariant*
* if `a` appears in negative position it is *contravariant*
* if `a` appears in both positions at once it is *invariant*

The only negative position is if it appears before an arrow (gets -1 pts), all other positions are positive (+1 pts). The overall position of a type param is the product of its points. If that product is negative, a type param is contravariant, else covariant.

- `∀a. a -> a` the `a` is *invariant* (it's in both positions at once)
- `(a, Bool) -> Int`, the `a` is *contravariant* (-1 * 1 = -1)
- `a -> b`, the `a` is *contravariant*, the `b` is *covariant*

A type's variance also has a more concrete interpretation: variables in positive position are produced (owned), while those in negative position are consumed.

> The **rank of a type** describes **the depth** at which universal quantifiers appear contravariantly (before an arrow).

* Monotypes τ, σ⁰ ::= a | τ₁ → τ₂
* Polytypes σⁿᐩ¹  ::= σⁿ | σⁿ → σⁿᐩ¹ | ∀a.σⁿᐩ¹

Examples:
- `Int -> Int`         Rank 0 (no type params)
- `∀a.a -> a`          Rank 1 (depth 1, `∀a` is before 1 arrow)
- `Int -> (∀a.a -> a)` Rank 1 (depth 1, `∀a` is before 1 arrow)
- `(∀a.a -> a) -> Int` Rank 2 (depth 2, since `∀a` is before 2 arrows)


## Type annotations

Haskell and ML are both based on the classic *Damas-Milner type system*, which has the remarkable property that a compiler can infer the principal type for a polymorphic function, without any help from the programmer. Furthermore, the type inference algorithm is not unduly complicated. But Damas-Milner is brittle: almost any extension of the type system either destroys this unaided-type-inference property, or greatly complicates the type-inference algorithm.

The Damas-Milner type system permits ∀ quantifiers only at the outermost level of a type scheme; the type inference becomes difficult or intractable if one permits richer, higher-ranked types.

```hs
f :: (forall a. [a] -> [a]) -> ([Bool], [Char])
f x = (x [True, False], x ['a','b'])
```

The type signature for `f` makes the type of `x` clear, without explicitly annotating the latter. Here, annotating `x` directly wouldn't be too bad:

```hs
f (x :: forall a. [a]->[a]) = (x [True, False], x ['a','b'])
```

But one would not want to annotate `x` and provide a separate type signature;and if `f` had multiple clauses one would tiresomely have to repeat the annotation. 

The idea of propagating type information around the program, to avoid redundant type annotations, is called **local type inference**.

## Subsumption

Suppose that we have variables bound with the following types:

```hs
k  :: ∀a b. a -> b -> b
f1 :: (Int -> Int -> Int) -> Int
f2 :: (∀a. a -> a -> a) -> Int
```

The application `f1 k` is well typed: `a ~ b ~ Int`. But what about the application `f2 k`? Even though `k`'s type is not identical to that of `f2`'s arg, this application too should be accepted because `k` is more polymorphic than `f2` requires. The `k` is independently polymorphic in `a` and `b` (more general, as both arg types may be different), while `f2` is polymorphic just in `a` (less general, as both arg types must be the same).

There is some sort of subtyping going on: a type arg is acceptable to a function if it's more polymorphic than what the function expects. The term **subsumption** is used to describe this *"more-polymorphic-than" relation*.

When extended to arbitrary rank, the usual co/contra-variance phenomenon occurs; that is, 
`σ₁ -> Int` is more polymorphic than 
`σ₂ -> Int` 
if `σ₁` is less polymorphic 
than `σ₂`. For example:

```hs
g  :: ((∀b.  [b] -> [b]  ) -> Int) -> Int
k1 ::  (∀a.    a -> a    )         -> Int
k2 ::     ([Int] -> [Int])         -> Int

g k1 -- ill-typed
g k2 -- well-typed
```

Since `∀a.  a  ->  a` is more polymorphic 
than  `∀b. [b] -> [b]`, it follows that 
`(∀a.  a  ->  a ) -> Int` is less polymorphic than 
`(∀b. [b] -> [b]) -> Int`.

Hence the application `g k1` is ill-typed. In effect, `k1` requires to be given an argument of type `∀a.a → a`, whereas `g` only promises to pass it a (less polymorphic) argument of type `∀b.[b] → [b]`. On the other hand, the application `g k2` is well-typed.

## Predicativity

Once one allows polytypes nested inside function types, it is natural to ask whether one can also call a polymorphic function at a polytype. For example, consider the following two functions:

```hs
(&) :: forall a b. a -> (a -> b) -> b
(&) x f = f x

poly :: (forall a. a -> a) -> (Int, Bool)
poly f = (f 3, f True)
```

Is the application `(\x -> x) & poly` legal? It requires us to instantiate the type variable `a` from (&)'s type with the polytype `∀a. a -> a`.

The function `fixMT` is a more practical example. It is a specialised instance of the "ordinary" fix function. However, using `fix` in place of `fixMT` would mean instantiating fix at the polymorphic type `MapT`.

```hs
-- the "ordinary" fix function
fix :: (a -> a) -> a
fix f = f (fix f)

type MapT = forall a b. (a -> b) -> Term a -> Term b

fixMT :: (MapT -> MapT) -> MapT
fixMT f = f (fixMT f)

mapT :: MapT
mapT = fixMT $ \mt -> \f t -> case t of
  Var x     -> Var (f x)
  App t1 t2 -> App (mt f t1) (mt f t2)
  Lam t     -> Lam (mt (mapI f) t)
```

The same issue arises in the context of data structures. Suppose we have this tree data type:

```hs
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

Is it legal to have the type `Tree (∀a. a -> a)`, i.e. a `Tree` whose leaves hold polymorphic functions? Doing so would require us to instantiate the `Leaf` ctor at a polymorphic type.

> A **predicative type system** only allows a polymorphic function to be instantiated with a *monotype*.

> An **impredicative type system** allows a polymorphic function to be instantiated with a *polytype*.

The Damas-Milner type system is predicative. Type inference is much easier in a predicative type system. Remarkably, it is possible to support both type inference and impredicativity, as MLᶠ shows, but doing so adds significant new complications to both the type system and the implementation.

(D.Botlan, D.Remy: *MLF - Raising ML to the power of System F*, in ICFP 2003)

# Higher-kinded types

Haskell allows abstraction over HKT. For example, our Monad type was defined like this:

```hs
data Monad m = Mon
  { return :: a -> m a
  , bind   :: m a -> (a -> m b) -> m b
  }
```

Here, the type variable `m` ranges over type ctors. GHC will infer that `m` has kind `∗ -> ∗` i.e. `m` maps types to types.

The question of type inference for *higher-kinded types* is an interesting one. It turns out that the solution adopted by Haskell for higher kinds extends smoothly to work in the presence of *higher-rank types*, as we know from our experience of implementing both in GHC. They are almost entirely orthogonal.

## Type systems for higher-rank types

- Type contexts:      Γ := Γ, x : σ | ϵ
- Type variables:     a | b | ...
- Monotypes:          τ := Int | τ₁ -> τ₂ | a
- Rho-types:          ρ := τ | σ -> σ
- Polytypes:          σ := ∀ā. ρ (`ā` is a sequence of 0 or more type vars)

Rho-types:
- Rank 1: ρ := τ
- Rank N: ρ := τ | σ -> σ


Syntax of the source language:

```
Term variables: x,y,z
Integers:       i

Terms t,u
  := i                    Literal
  |  x                    Variable
  |  \x.t                 Abstraction
  |  \(x::σ).t            Typed abstraction (σ closed)
  |  t u                  Application
  |  let x = u in t       Local binding
  |  t::σ                 Type annotation (σ closed)
```

- type annotations are closed, that is, they have no free type vars (as is the case in Haskell 98)



## The non-syntax-directed Damas-Milner type system

The type checking rules for the Damas-Milner type system.

In this system, polytypes have Rank-1 only, so a `ρ`-type is simply a monotype `τ`, and hence a polytype `σ` takes the form `∀a.τ`. The main judgement takes the form: `Γ ⊢ t : σ` which means that the term `t` has type `σ` in the env `Γ`.

However, several judgements have the form `Γ ⊢ t : ρ` (conclusion of APP rule), and this is just shorthand for the `σ`-type `∀. ρ`, i.e. a type without a quantifier.

In the Damas-Milner system we omit the type-annotated lambda `\(x::σ).t`, because a Damas-Milner lambda can only abstract over a monotype, and that is adequately dealt with by the un-annotated lambda.

The INST rule makes a very important point: the system is predicative, so type variables may only range over monotypes. We can see this from the fact that the type variables in INST are instantiated by `τ` types, and not by `σ` types. Efficient type inference crucially depends on this restriction.

```
Rho-types ρ := τ

Γ |- t : σ


----------- INT
Γ |- i : Int


------------------- VAR
Γ, (x : σ) |- x : σ


Γ, (x : τ) ⊢ m : ρ
--------------------- ABS
Γ |- (λx.m) : (τ → ρ)


Γ ⊢ m : τ → ρ
Γ ⊢ u : τ
-------------- APP
Γ |- m u : ρ


Γ        ⊢ u : σ
Γ, x : σ ⊢ m : ρ
----------------------- LET
Γ |- 𝒍𝒆𝒕 x = u 𝒊𝒏 m : ρ


Γ |-  t     : σ
----------------- ANNOT
Γ |- (t::σ) : σ


ā ∉ 𝚏𝚝𝚟 (Γ)
Γ ⊢ t : ρ
------------- GEN
Γ |- t : ∀ā.ρ


Γ |- t : ∀ā.    ρ
----------____----- INST
Γ |- t : [ā->τ] ρ
```

- in the definition of polytypes, we quantify over a vector of zero or more type vars, `ā`, rather than quantifying one variable at a time with a recursive definition. These quantifiers are not required to bind all the free type vars of `ρ`; i.e. a polytype `σ` can have free type vars, else it wouldn't be possible to write HRT, such as `∀a.(∀b.(a, b) → (b, a)) → [a] → [a]`

- Hereto, we assume we have a list, `[τ]`, and pair types, `(τ1, τ2)`, but we will not introduce any terms with these types.

- In our syntax, a `σ`-type always has a `∀`, even if there are no bound vars, but we will sometimes abbreviate the degenerate case `∀.ρ` as simply `ρ`.

- The same figure also shows type contexts, `Γ`, which convey the typings of in-scope vars. `Γ` binds a term var, `x`, to its type `σ`.

- We define `ftv(σ)` to be the set of the free type vars of `σ`, and extend the function to type contexts: `ftv (Γ) = S { ftv (σ) | (x : σ) ∈ Γ }`

- We use the notation `[ā -> τ] ρ` (in INST) to mean the capture-avoiding substitution of type variables `ā = a₁...aₙ` by monotypes `τ` in the type `ρ`.





<!-- #region Refs -->
## Refs

https://wiki.haskell.org/Rank-N_types

Higher-Ranked Types
https://8thlight.com/blog/mark-grant/2013/09/13/higher-ranked-types-part-1.html


Higher-rank and higher-kinded types
https://www.stephanboyer.com/post/115/higher-rank-and-higher-kinded-types

Implement With Types (Haskell)
https://reasonablypolymorphic.com/blog/typeholes/index.html

Generalizing Hindley-Milner Type Inference Algorithms
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf

Practical type inference for arbitrary-rank types
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf

The surprising rigidness of higher-rank kinds
https://ryanglscott.github.io/2019/07/10/the-surprising-rigidness-of-higher-rank-kinds/

Higher-Ranked Types
https://8thlight.com/blog/mark-grant/2013/09/13/higher-ranked-types-part-1.html

The Curry-Howard Correspondence in Haskell
https://web.archive.org/web/20150325180015/http://www.thenewsh.com/~newsham/formal/curryhoward/

SPJ pub
https://www.microsoft.com/en-us/research/people/simonpj/publications/

Tutorial Papers in Functional Programming
https://web.archive.org/web/20090924024043/http://www.md.chalmers.se/~rjmh/tutorials.html

Vector, Vector Tutorial
http://hackage.haskell.org/package/vector
https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial

<!-- #endregion -->
