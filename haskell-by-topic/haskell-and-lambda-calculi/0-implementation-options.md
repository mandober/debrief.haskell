# Haskell by Topic :: Haskell and Lambda Calculi

## Implementation factors

Fundamental to all functional languages is the most atomic notion of composition - function abstraction of a single variable. Lambda calculus consists of 3 terms, and all valid recursive combinations thereof:
- Var, variable
- App, application
- Abs, lambda abstraction

A lambda abstraction is said to *bind* its variable, so `λx.E`, is an anonymous function with a *formal parameter* named `x` that, when applied, will return some arbitrary lambda expression, here denoted by a metavariable `E`. When applied to an argument, the parametr `x` will bind that argument. The exp `E` makes the *body* of the lambda abstraction, and represents the return value of the abs. After an abs is applied, the body exp will need to undergo further evaluation before reaching a *normal form* (something that can be called a value). The body usually mentions (uses) the bound variable, so here, the body `E` may mention the bound variable `x` none, one, or multiple times. For example, the abs `λxy.x` ignores its second arg, `λa.a` mentions its sole arg once, while `λgfx.gx(fx)` mentions its third arg twice.

```
E := x       (Var)
   | λx.E    (Lam)
   | E E     (App)
```

- abstraction associates to the right, λx.λy.λz.E = λx.(λy.(λz.E))
- application associates to the left, M N E = (M N) E
- application extends as far to the right as is syntactically meaningful
- abs has precedence over app, λx.x y = λx.(x y) = λx.xy ≠ (λx.x)y
- parentheses are used for disambiguation
- each lambda abstraction binds a single variable
- the body of abs may be another abs
- binders may be contracted, λxyz.E = λx.λy.λz.E


An actual implementation of LC admits several degrees of freedom in how abs are represented. The most notable is the choice of identifiers for the binding variables. A variable is said to be *bound* if it is contained in an exp of the same variable binding. Conversely a variable is *free* if it is not bound. A term with free variables is called an *open term*, while a term without free variables is called a *closed term*. A closed abstraction is called a *combinator* (combinators are extremely rarely applications).

Multiple abs may bind the same variable (name). Each occurrence of a variable is then bound by the nearest enclosing binder. An inner var with the same name is said to *shadow* an outer var.

## Substitution

A single substitution step is called a *reduction*. β-reduction of a lambda exp (application) `(λx.E) A` proceeds by performing substitution: all free occurrences of the variable `x` in the body `E` are replaced by the argument `A`, which is denoted `[x:=A]E`; although var `x` is bound in the abs, `λx.E`, it occurs free (possibly) in the exp `E`. Being free or bound are relative terms that depend on the scope (subexp) under consideration.

    (λx.E) A -->> [x := A] E      if x ∉ FV(A)


Definition of substitution:

```
[x := A] x      = A
[x := A] y      = y                         if x ≠ y
[x := A] (M N)  = ([x := a] M) ([x := a] N)
[x := A] (λx.E) = λx.E
[x := A] (λy.E) = λy.[x := A] E               if x ≠ y and y ∉ FV(A)
```

where `FV(E)` denotes the set of free variables in exp `E`.

A substitution metavariable will be written as [s].

*Capture-avoiding substitution* only proceeds if the bound variable (`y`) is not in the set of free variables of the arg `A`; if it is, then the bound variable `y` must first be renamed: its binding occurrence, `λy`, and all its occurrencies in `E`; we do this by genereting a fresh name for it. This is justified by the fact that variable names have no meaning other than to mark positions of variables, e.g. `λx.x = λy.y = λz.z` are said to be all *equal up to renaming (of bound vars)*; they are *α-equivalent*.

    λx.x =α= λy.y

*α-equivalnce* or renaming may also be denoted using the same notation as for the substitution: `[x := xʹ] E`, except here both `x` and `xʹ` can only be vars, plus the meaning of symbols is changed: `x` is the original var name, but `xʹ` is a fresh name, and `E` signifies the exp inside which this renaming is performed.

For example, the redex `(λx.λy.yx)(λz.zy)` cannot proceed with β-reduction because the variable `y` in the argument `λz.zy` occurs free. If we naively substituted `x` with the arg `λz.zy` in the body `λy.yx`, we get `λy.y(λz.zy)`, which is incorrect since the previously free var `y` is now captured (and thus bound) by the leftmost binder `λy`, thus changing the meaning of the exp.

So before we can reduce the exp `(λx.λy.yx)(λz.zy)`, we must resolve this conflict. But how? What should we rename? The rule is to always rename a bound variable - we rename its binding occurrence (`λy`) and all of its applied occurrences (in the scope of that binder).

```hs
binding occurrence of y
     ↑
     | applied/bound occurrence of y
     | ↑
     | |       free occurrence of y (distinct var with the same name)
     | |       ↑
(λx.λy.yx)(λz.zy)
     ↓ ↓     -- is renamed into
(λx.λw.wx)(λz.zy)
```

i.e. the binding occurrence of `y` in `λy`, as well as the (one) applied occurrence of `y` are renamed to a fresh name `w`. In exp `(λx.λy.yx)(λz.zy)`, this renaming is denoted by [y := w](λy.yx). Note that the exp in which this renaming is done is not the entire exp, nor the first applicative (λx.λy.yx),but the subexp `λy.yx` that starts with the binder  (`λy`) that binds the var that is to be renamed.

Renaming var `y` with a fresh name `yʹ` in the subexp `S`:

    [y := yʹ] S


```hs
    (λx.λy.yx)(λz.zy)
    [x := λz.zy](λy.yx)
!=ᵦ λy.y(λz.zy)         -- y in (λz.zy) gets captured by the outer λy!
    (λx.λy.yx)(λz.zy)   -- first rename the bound y in (λx.λy.yx) to w
    [y := w](λx.λy.yx)   -- then proceed with β-reduction
=α= (λx.λw.wx)(λz.zy)
    [x := λz.zy](λw.wx)
==ᵦ λw.w(λz.zy)
```

## Conversion and Equivalences

### Alpha equivalence

    (λx.E) =α= λy.[x:=y]E

Alpha equivalence is the property (when using *named binders* instead of deBruijn indices) that changing the variable on the binder and throughout the body of an expression should not change the fundamental meaning of the exp.

For example, the following exp are alpha-equivalent, `λxy.xy =α= λab.ab`.

Symbolically, α-equivalence, aka renaming, may be denoted similar to substitution, except that `xʹ` is a *fresh for* `E`.

    [x := xʹ] E


### Beta-reduction

Beta reduction is simply a single substitution step, replacing a variable bound by a lambda expression with the argument to the lambda throughout the body of the expression.

    (λx.E)A -->ᵦ [x:=A]E

e.g.

(λx.z)(λy.y) -->ᵦ [x:=λy.y]z -->ᵦ z

### Eta transformation

    λx.Fx =η= F      if x ∉ FV(F)

η-reduction is justified by the fact that if we apply both sides to a term, one step of beta reduction turns the left side to the right side:

```hs
(λx.Fx)A -->ᵦ F A   -- provided x ∉ FV(F)
```

The opposite of *η-reduction* is *η-expansion*, which takes a function that is not saturated and makes all variables explicitly bound in a lambda.

## Reduction

Evaluation of a lambda expression proceeds by beta reduction. Variables bound by a lambda abstraction are substituted across the body of the abs.

There are several degrees of freedom in the design space on how to do this, and in which order an expression should be evaluated.

For instance, we could evaluate under a lambda and then substitute variables into it, or instead, evaluate the arguments and then substitute them, reducing the expression (more on this in Evaluation models).

## Recursion

Probably the most famous combinator is Curry's `Y` combinator. Within untyped lambda calculus, `Y` can be used to allow an expression to contain a reference to itself and reduce to itself (plus some extra exp) permitting recursion and looping logic. The `Y` combinator is one of many of the so-called *fixed point combinators*.

```hs
Y := λf.(λx.f(xx))
        (λx.f(xx))

Y := λf.(λx.f(xx))(λx.f(xx))

-- example
Y F
= (λf.(λx.f(xx)) (λx.f(xx))) F
=     (λx.F(  x          x)) (λx.F(xx))
=         F ( (λx.F(xx)) (λx.F(xx)) )
= F ( F ( (λx.F(xx)) (λx.F(xx))) )
= F ( F ( F ( … ) ) )
≡ F ( Y F )

-- Y = λR. (λx.R(xx)) (λx.R(xx))
-- Y F = (λf.(λx.f(xx)(λx.f(xx)))) F
--     = (λx.(F(xx))λx.(F(xx)))
--     = F (λx.(F(xx))λx.(F(xx)))
--     = F(YF)
```

In untyped lambda calculus without explicit fixpoint or recursive let bindings, the `Y` combinator can be used to create both of these constructs out of nothing but lambda abstractions. However it is more common to just add either an atomic fixpoint operator or a recursive let as a fundamental construct in the term syntax.

```hs
e := x
   | e e
   | λx.e
   | fix e
```

Where `fix` has the evaluation rule:

    fix v -->> v (fix v)


Together with the fixpoint (or the Y combinator) we can create let bindings which contain a reference to itself within the body of the bound expression. These are *recursive let bindings*, written as `let rec` in ML dialects. We can also implement them by wrapping a fixpoint around a lambda binding by the following equivalence.

    let rec x = M in N   ===    let x = fix (λx.M) in N


To show both styles, `factorial` is written with *let rec* and `fibonacci` using *explicit fix*:

```hs
-- factorial with explicit `fix`
let fac = fix (\fac -> \n ->
    if (n == 0)
    then 1
    else (n * (fac (n - 1)))
  )
-- or
let fac = fix ( \ f n -> if n == 0 then 1 else n * f (n - 1) )

-- fibonacci with let rec
let rec fib n =
  if n == 0
  then 0
  else if n == 1
       then 1
       else fib (n - 1) + fib (n - 2)
```

## Omega Combinator

An important degenerate case useful for testing is the omega combinator which applies a single argument to itself:

    ω = λx.xx

    Ω = ωω = (λx.xx)(λx.xx)

When we apply the `ω` combinator to itself we get `Ω`, which results in an infinitely long repeating chain of reductions. A sequence of reductions that has no normal form (i.e. it reduces indefinitely) is said to *diverge*.

    Ω = ωω = (λx.xx)(λx.xx) -->> (λx.xx)(λx.xx) -->> (λx.xx)(λx.xx) …

The `Ω` combinator is the canonical looping term in lambda calculus.
