# Haskell by Topic :: Haskell and Lambda Calculi

## Design points and Trade-offs

Lambda Calculus is not a single language but a family of languages with even more subfamilies as determined by their choice to many points of contrition:
- type system
  - untyped
  - typed
    - λ→
    - λ2
    - λω
    - λΠ
    - λ2ω
    - λ2Π
    - λωΠ
    - λ2ωΠ
  - polymorphism (_2)
  - type ctors (_ω)
  - dependent types (_Π)
  - unification
  - typing rules
  - type erasure
- evaluation and reduction
  - reduction strategy, redex, reducable exp
  - evaluation strategy
  - strict or non-strict evaluation
  - reduction order: normal, applicative
  - whether to reduce under a lambda
  - call-by-value vs call-by-name vs call-by-need
  - domain values, domain of discourse
  - small-step semantics
  - big-step semantics
  - normal form, WHNF, exp, value, neutral value, ...
- representing variables
  - named variables
  - deBruijn indices
  - named variables and deBruijn indices
  - managing names of pretty-printed variables
- representing a lambda abstraction
  - First-order Abstract Syntax (plain data type for abs)
  - Higher-order Abstract Syntax, *HOAS* (use host lambdas for abs)
  - Parametric Higher-order Abstract Syntax, *PHOAS* (use host lambdas)
- substitution
  - capture-avoiding substitution
  - direct replacement
  - using environment
  - explicit substitution
  - director strings


Typed lambda calculi add types to untyped lambda calculus, but even with types there are too many choices to pick from, primarily those dealing with particular reduction strategy and substitution.



### Calculus of constructions

>The Curry-Howard isomorphism associates a term in the STLC with each natural-deduction proof in intuitionistic propositional logic.

The Calculus of Constructions can be considered an extension of the CHI. The CoC extends this isomorphism to proofs in the full intuitionistic predicate calculus, which includes proofs of quantified statements (sometimes also call "propositions").

CoC has very few basic operators: the only logical operator for forming propositions is `∀`. However, this is sufficient to define all others:
- A → B  ≡ ∀x : A . B                  (x ∉ B)
- A ∧ B  ≡ ∀C : P . (A ⇒ B ⇒ C) ⇒ C
- A ∨ B  ≡ ∀C : P . (A ⇒ C) ⇒ (B ⇒ C) ⇒ C
- ¬A     ≡ ∀C : P . (A ⇒ C)
- ∃x:A.B ≡ ∀C : P . (∀ x : A . (B ⇒ C)) ⇒ C

The basic data types used in computer science can be defined within the calculus of constructions:
- Booleans ∀A:P.A ⇒ A ⇒ A
- Naturals ∀A:P.(A ⇒ A) ⇒ A ⇒ A
- Product A×B: A ∧ B
- Disjoint union A + B: A ∨ B

Note that Booleans and Naturals are defined in the same way as in Church encoding. However, additional problems arise from propositional extensionality and proof irrelevance.

### FOAS

In the First-order Abstract Syntax (FOAS), abstractions are not especially represented - a lambda is `Abs String Exp`, i.e. the data ctor `Abs` supplied with a variable (vars are strings) and a lambda xp for the body.

Lambda abstractions then look like this:

```hs
i = Abs "x" (Var "x")            -- λx.x
k = Abs "x" (Abs "y" (Var "x"))  -- λxy.x
```

And the substitution proceeds as usual: 
e[v/x] means `x` is replaced by `v` in exp `e`, 
i.e. what we canonically denote by [x⟼v]e.

```hs
subst e x v = e[v/x] -- v must be a closed exp

subst (Var y)     x v = if x == y then v else Var y
subst (Abs y e)   x v = if x == y then (Abs y e) else (Abs y (subst e x v))
subst (App e1 e2) x v = App (subst e1 x v) (subst e2 x v) 
```

### HOAS

Higher-order abstract syntax (HOAS) uses functions of the host language to represent lambda abstractions. This representation hides the innards of abstractions from us since we cannot inspect Haskell functions, but it greatly simplifies substitution (in fact it's a gas).

## Representing variables

Lambda abstractions will also use strings for their formal parameters:

    Abs "x" (Var "x")

which is not ideal since both `x`'s represent the same variable, but the first `x` is just a string `"x"`, while the second is a proper variable `Var "x"`. On the other hand, making them both proper variables, e.g.

    Abs (Var "x") (Var "x")

would complicate things since we'd have to define a new data type for variables, but at least then all vars are really vars (and not a combo of strings and vars).

Yes, there are infinite choices right off the start merely in deciding on a right representation. Worse of all, whatever we settle on will probably have to be modified later when we implement other lambda calculi. Deciding from the start on the ideal representation that can be cleanly extended seems futile, so we proceed unburdoned and just wing it, taking it as it comes.

```hs
newtype Var = Var String

data Exp
  = Par Var
  | Abs Var Exp
  | App Exp Exp
```

Examples of lambda expressions:

```hs
-- x
vx :: Var
vx = Var "x"

-- λx.x
i :: Exp
i = Abs (Var "x") (Par (Var "x"))
```

...and now we have 2 incompatible syntactic forms, i.e. variables are not lmabda terms! Variables have type `Var` and lambda terms type `Exp`! Shite. Since `Var "x"` is different from `Par (Var "x")`, we might as well return to the previous situation where the string `"x"` was different from `Var "x"`, even though both represent variables.

### Variables

The occurrence of `x` in `λx.M` (where `M` is some arbitrary lambda exp) is called its **binding occurrence**. The second occurrence of `x` in `λx.x` is called its **applied occurrence**; these are all the occurrences of some variable in the body of a lambda, e.g. `λx.λy.xyx` has two applied occurrences of the variable `x`, along with a (single) binding occurrence of `x` (the `x` right next to the lambda binder, `λx`).

Variables are also distinguished by being bound or free. In `λx.λy.xyx`, both variables `x` and `y` are bound - each has its binder in the scope. But being free or bound is a relative state of a variable: focusing on just the subexp `xyx`, both vars are free. That is, these two attributes depend on a particular scope under consideration.

A lambda exp consists of a number of subexp. Even a simple exp like `λx.x` has two subexp: `x` (i.e. only the lambda's body) is one, and the other is the verall exp, `λx.x`, which is also counted as a subexp.

In the expression, `λx.yx`, the var `x` has a *binding occurrence* (`λx`), and an *applied occurrence* in the lambda body, `yx`. In this exp, the variable `x` is *bound* (since its binder is in scope when the entire exp is considered). However, if we only consider the subexp `yx`, then `x` is free there. On the other hand, the var `y` is free no matter if we consider the whole exp or any of its subexps (in fact, `λx.yx` better be a subexp of some bigger exp, otherwise `y` is undefined).

A lambda expression without free variables is called a **closed expression** or a **combinator**. Otherwise the exp is called open.


### Free variables

It is often needed to get the **set of free variables**, `FV`

```
FV(x)    = {x}
FV(λx.M) = FV(M) ∖ {x}
FV(M N)  = FV(M) ⋃ FV(N)
```

FV(x λx.x) = ??? 
In this App, `x` is both free and bound! 
FV(x λx.x) 
= FV(x) ⋃ FV(λx.x) 
= {x} ⋃ (FV(x) ∖ {x}) 
= {x} ⋃ ({x} ∖ {x}) 
= {x} ⋃ ∅ 
= {x}
so this result is incorrect! 
We must perform α-equivalence to rename the binding and bound `x` to `xʹ` in `λx.x`. So we get a renamed term and then FV(x λxʹ.xʹ) = {x} is correct. Well, in this case we get the same answer, but the problem still lurks.

https://www.youtube.com/watch?v=R1dmeFEJyqI&list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&index=3





A *variable* alone is free, so `FV(x) = {x}`, i.e. just the singleton set `{x}`.

For *application* of two lambda exps, `M N`, we recursively check for FV in each, unionizing the resulting sets, `FV(M N) = FV(M) ⋃ FV(N)`.

When getting FV of an *abstraction*, `λx.M`, we skip the binding occurrence of the var `x` and procced to collect FV in the lambda's body `M`. However, the binder `λx` suggest it is likely that this `x` appears in `M`. This `x` may be bound in the overall exp, but it is free in `M`, so the recursive call `FV(M)` is gonna collect it. Thus, we need to remove `x` from the FV set - hence the set difference operator, `FV(M) ∖ {x}`.

This case seems to present opportunity to screw things up if there should be another, distinct variable `x` in `M`, name-shadowing the outer `x` variable. If that inner `x` is bound, then no problem, it all amounts to the same. But if that inner `x` is free, then we shouldn't remove `x` from the FV set (although we'd have a hard time distinguishing between two variables when the identifier "x" is just a string). But is it even possible that another `x` appears free in `M`? It seems not: if there were any free `x`'s in `M`, they would be bound by the outermost binder, `λx`, right? Right-o, because, you know, reasoning.

Examples of FV
- FV λx.x = ∅
- FV λx.y = {y}
- FV λx.λy.xyx = ∅
- FV λx.λf.f(λz.zyx) = {y}
- FV λx.λy(y(λx.x)) = ∅
- FV (λx.λf.(x(λx.fxx))(q(λx.fxx))) (λz.zw) = {q} ⋃ {w} = {q,w}

We have restored the initial data type, so now we implement the `fv` function that collects free vars in a given lambda exp.

```hs
data Exp
  = Var String
  | Abs String Exp
  | App Exp Exp

fv :: Exp -> Set String
fv e = case e of
  Var s   -> S.singleton s
  Abs s e -> fv e `S.difference` S.singleton s
  App m n -> fv m `S.union` fv n
```

## Values

Lambda expressions get evaluated and reduced to **values**, and in LC the only values are lambda expressions again; well, reduced lambda expressions. An exp that can be reduced is called a **redex**, and an exp that cannot be reduced anymore is in a **normal form**. Usually, normal forms are values. So we need to tell when an exp is in normal form.

Under the evaluation strategy that also reduces under lambdas, normal form is really a normalized exp - an exp that cannot be reduced further. However, in the strategies that do not reduce under a lambda, expressions may end up still having redexes and they are said to be in the **weak-head normal form**. The "head" here is the lambda binder of an abstraction.

Evaluation proceeds by reducing an exp until it either gets reduced down to a value, or it gets **stuck** because there are no evaluation rules to follow.
