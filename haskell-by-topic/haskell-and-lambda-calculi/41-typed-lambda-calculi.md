# Lambda calculus unboxed

Lambda calculus unboxed
https://www.youtube.com/playlist?list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4

"Lambda Cube Unboxed" is a video series on the basic lambda calculi which form the basis of many important concepts of functional programming, logic, mathematics and type theory. In these 13 videos, we explore and expose the basics of un/typed λ-calculi including higher-order parametric polymorphism and dependent types. We introduce the systems and analyze their properties working towards a uniform definition in the context of the lambda cube.

## Souces

- `Type Theory and Formal Proof`, 2014, R. Nederpelt, H. Geuvers
- `Lambda Calculi with Types`, 1992, by H. P. Barendregt
- `Propositions as Types`, 2015, P. Wadler
- `Proofs and Types` (Vol 7), 1989, J. Y. Girard, P. Taylor, Y. Lafont
- `Programming in Higher-order Typed Lambda Calculi`, 1989, 
   B. Pierce, S. Dietzen, S. Michaylov

## Terms

```hs
add = λmnfx.mf(nfx)  ~ fᵐ(fⁿ x)
mul = λmnfx.m(nf)x   ~ (fⁿ)ᵐ x = fⁿ (fⁿ (fⁿ (…m times…))) = n + n + …ᵐ + n = nm
pow = λmn.nm
pow = λmnfx.nmfx     ~ f(ⁿᵐ) = fᵐⁿ

TRUE  = K  = λxy.x
FALSE = KI = λxy.y

0 = λsz.z
S = λnsz.s(nsz)

isZero = λn.n(λz.(λxy.y))(λxy.x)
isZero = λn.    n (λz. FALSE) TRUE

       =        0 (λz. FALSE) TRUE
       = TRUE

       =        1 (λz. FALSE) TRUE :
       = (λsz.sz) (λz. FALSE) TRUE
       = (λz. FALSE) TRUE
       = FALSE

       =        2 (λz. FALSE) TRUE :
       = (λsz.s(sz)) (λz. FALSE) TRUE
       = (λz. FALSE) [(λz. FALSE) TRUE]
       = FALSE
```


>**α-conversion**: two lambda exp `M` and `N` are α-convertable, `M ≡ N`, iff they only differ in the names of bound vars.

**Barendregt convention**: all vars used in an exp should be pairwise different and distinct from free vars.

## β-reduction

Computation with *the 1-step β-reduction*, `⟶ᵦ`
- Basis: `(λx.M)N ⟶ᵦ [x:=N]M`
- Compatibility: if `M ⟶ᵦ N` then
  - `M` L  ⟶ᵦ `N` L
  -  L `M` ⟶ᵦ  L `N`
  - λx.`M` ⟶ᵦ  λx.`N`

Compatibility ensures that the 1-step β-reduction also holds in subexpessions.

**Redex** is a reducable expression. **Contractum** is the reduced expression.

(λx.M)N  ⟶ᵦ   [x:=N]M    
redex    ⟶ᵦ  contractum

`M ⟶ᵦ N` read as: `M` is 1-step reducable to `N`.

In the *1-step β-reduction*, `⟶ᵦ`, exactly one redex is replaced by its contractum at the time.

For example,

      (λx.x(xy))N ⟶ᵦ [x:=N](x(xy)) ≡ N(Ny)

NOTE: `⟶ᵦ` and `≡` come in pairs: β-reduction is followed by syntactic equality, i.e. an exp M is beta reduced to exp N (expressed using subst notation) which is equal to exp N expressed in the std lambda notation.

More examples:

This term can be reduced in two ways:
`(λx. (λy. x y) z) v`
1.  (λx. (λy. x y) z     ) v         (innermost rightmost redex first)
⟶ᵦ (λx. {[y:=z](x y)} ) v
  ≡ (λx. x z) v
⟶ᵦ [x:=v](x z)
  ≡ v z

2.  (λx. (λy. x y) z) v              (outermost leftmost redex first)
⟶ᵦ [x:=v](λy. x y)
  ≡ (λy. v y) z
⟶ᵦ [y:=z](v y)
  ≡ v z


### Many-steps β-reduction

In the many-steps β-reduction, `⟶ᵦ⋆`, a number of 1-step reductions are abbreviated.

**Many-steps β-reduction**: `M ⟶ᵦ⋆ N` if there is `n≥0` and terms `M₀, …, Mₙ`, such that `M₀ ≡ M`, `Mₙ ≡ N`, and `Mᵢ ⟶ᵦ Mᵢ﹢₁`, for all `1≤i≤n`.

Basically, such that we can express many-step reduction as a sequence of 1-step reductions.

Curiously, `M` is many-step reducable to itself (since `n` can be 0), even though `M` is not 1-step reducable to itself!    
`M ⟶ᵦ⋆ M` but `M -/->ᵦ M`

## Undoing computation

So far, we are able to reduce and substitute in terms, but we still cannot actually computing anything!

What we have so far is the ability to say, e.g. `f(5) = 5²+1`, which can be reduced to 25+1 = 26.

But we have no *symmetric equality* sign, and therefore, we cannot add anything which is not needed for computing, and we cannot undo computations. To see why this is preferable, consider the equation: `a² + 2ab + b² = (a + b)²`. We can go from either side to the other side, as it suits us. Thus we needed the capability to undo a computation in order to simplify expressions better.

For example, thanks to this identity we can simplify the following expression:

```
a² + 2ab + b²    (a + b)²
------------- = --------- = a + b
    a + b         a + b
```

Therefore, we need a way to also undo β-reduction in order to simplify lambda expressions better.

This 2-way computation is an extension of β-reduction and is called *β-equivalence* or *β-conversion*, and denoted by `=ᵦ`.

**β-equivalence**: `M =ᵦ N` if there is `n≥0` and terms `M₀, …, Mₙ`, such that `M₀ ≡ M`, `Mₙ ≡ N`, and for all `1≤i≤n` either `Mᵢ ⟶ᵦ Mᵢ﹢₁` or `Mᵢ ᵦ⟵ Mᵢ﹢₁`.

>Every reduction chain is also a conversion chain but not the other way around.

For example, these two terms are β-convertable but not β-reducable:

    (λy.y v) z =ᵦ (λx.z x) v
    (λa.a v) z =ᵦ (λa.z a) v
    (λz.z v) z =ᵦ (λv.z v) v

The β-conversion holds because they can both be reduced to `z v`.

    (λy.y v) z  ⟶ᵦ  z v  ᵦ⟵  (λx.z x) v


>*β-convertibility* extends *β-reduction*: whenever a term is β-reducible to another, they are also β-convertible.

    (λx.x(xy))N  =ᵦ=  N(Ny)  =ᵦ=  (λz.N(Nz))y



β-conversion is an equivalence relation.

So far, we have several relations:
- `≡`    for syntactical equivalence, α-convertibility, substitution
- `⟶ᵦ`  for 1-step β reduction
- `⟶ᵦ⋆` for many-step β reduction
- `=ᵦ`   for β-conversion

Since β-conversion is an equivalence relation there should be a representative for equivalence classes.

https://youtu.be/R1dmeFEJyqI?list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&t=666


- β-normal form
- normalization
  - weakly normalizing
  - stronkly normalizing (terminating)
  - not normalizing at all
  - Church-Rosser theorem
  - confluence
