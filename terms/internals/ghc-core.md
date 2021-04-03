# Core Language


## GHC intermediate lang

Notes on the FC-based intermediate language
https://gitlab.haskell.org/ghc/ghc/-/wikis/intermediate-types

These notes describe the new intermediate language for GHC. The intermediate language is based on System F with algebraic datatypes and explicit type coercions (hereafter FC). See the paper:
http://research.microsoft.com/~simonpj/papers/ext-f/fc-popl.pdf

This note mostly focuses on the type system and also discuss how some source level features are represented in the intermediate language. Most of the system is fairly standard, with the exception of coercions.

A coercion `c`, is a type-level term, with a kind of the form `T1 :=: T2`. (`c :: T1 :=: T2`) is a proof that a term of type `T1` can be coerced to type `T2`. It is used as argument of a cast expression; if `t :: T1 then (t cast c) :: T2`.

---

# Haskell's Core Language

(*Back to Core* - S.P.Jones)

Haskell is a massive language with dozen of data types and 100+ constructors. This is what the type checker works on. After it's done, the desugaring phase brings the massive Haskell into the Core, which is a typed intermediate language (IL). Although the Core has just 3 types and 15 constructors, any Haskell feature can be translated into Core's scarce syntax. While Haskell itself is implicitly typed, the Core is explicitly typed and every binder has a type annotation, e.g. `(\x: Bool -> x)`. This makes the type inference dramatically faster compared to full-blown Haskell code where specifying just which programs are well-typed is complicated because of many ad-hoc constraints that make type inference feasible.

Small IL means that analysis, optimisation, and code generation, handle only a small language. Type checker for the Core language is basically a lint that relies on straightforward syntactic checks. It also acts as a very powerful internal consistency checker, by verifying that desugared code still type-checks. Also it helps maintain consistency by ensuring that optimisation passes transform well-typed Core into (still) well-typed Core code.

The design of the Core is a powerful sanity-check on crazy type system extensions currently available in GHC or the ones that are still being experimented with - if they desugar correctly into Core, the typing must be sound.

"GHC is the only production compiler that remorselessly pursues this idea of a strongly-typed IL. The design of Core is probably GHC's single most substantial technical achievement." -- S.P.Jones


## Lambda Calculi

The design of the Core starts with the Lambda Calculus. However LC is untyped and we need a typed calculus so we can sprinkle some type annotations atop, without getting buried in them. And besides, types change with optimizations.

```hs
compose :: (b->c) -> (a->b) -> a -> c
compose = λ(f: b->c) . λ(g: a->b) . λ(x:a)
  let tmp: b = g x
  in f tmp
```

The idea is to put type annotations on each binder (lambda, let), but nowhere else; but then, where are `a`,`b`,`c` bound?

```hs
compose :: (b->c) -> (a->b) -> a -> c
compose = λ(f: b->c) . λ(g: a->b) . λ(x:a)
  let tmp: b = g x
  in f tmp

neg :: Int -> Int
isPos :: Int -> Bool

-- unstable under transformation
compose isPos neg
  -- (inline compose: f=isPos, g=neg)
  λx:a. let tmp: b = neg x
        in isPos tmp
```

Another problem is that it is unstable under transformation, breaking well-typedness. oh😱noes. εὕρηκα! 😮 Get me Girard and Reynolds!



## System F

System F, or Girard–Reynolds polymorphic lambda calculus, is a typed LC that has introduced the mechanism of universal quantification over types.

System F formalizes the notion of parametric polymorphism, serving as the theoretical base for FP languages such as Haskell and ML.

Whereas simply typed lambda calculus has variables ranging over functions, and binders for them, System F adds variables that range over types and a location to bind them.

For example, the polymorphic identity function (it can have any type of the form $$A\to A$$) would be formalized in System F as the judgment:

$$
\vdash \Lambda \tau . \lambda x^{\tau}.x : \forall \tau . \tau \to \tau
$$

where:
- $$\tau$$ is a type variable
- the explicit or big lambda, $$\Lambda$$, binds the type variable $$\tau$$
- the superscripted $$\tau$$ in $$x^{\tau}$$ marks the type of the bound $$x$$
- the expression after the colon is the type of the lambda abstraction

$$\Lambda$$ is traditionally used to denote type level functions, while $$\lambda$$ denotes value level functions.

The Boolean type, $$\scriptstyle \mathsf{Bool}$$, is defined as: 
$$\forall \alpha\ .\alpha \to \alpha \to \alpha$$     
that is, $$\scriptstyle \mathsf{Bool}$$ is the type of all functions which take as input:
* a type $$\alpha$$ (the input type arg that matches the declared type param)
* an expression of type $$\alpha$$ (the first value arg)
* another expression of type $$\alpha$$ (the second value arg)
* and produce an expression of type $$\alpha$$ as output (the returned value)



## Back to Core

So the exemplary composition function is annotated like this

```hs
-- function abstraction
compose :: ∀abc . (b->c) -> (a->b) -> a -> c
compose = Λ abc . (λf: b->c) . (λg: a->b) . (λx: a)
        let tmp: b = g x
        in f tmp

-- application: big lambdas applied to types, small lambdas to values.
-- here @ denotes passing the types to the corresponding type params.
compose @Int @Int @Bool isPos neg
  -- = (inline compose: a=Int, b=Int, c=Bool, f=isPos, g=neg)
  λx:Int . let tmp: Int = neg x
           in isPos tmp
```
and now the expression remains well-typed.


The Core also has
- Algebraic Data Type declarations
- Data constructors in terms
- Case expressions
- Let expressions

```hs
-- ADT declarations
data Maybe a = Nothing | Just a

-- data ctors in terms
λx:Int. Just (Just x)

-- case expressions
case x of { Nothing -> 0; Just x -> x+1 }

-- let expressions
let x:Int = 4 in x+x
```
