# Haskell by Topic :: Haskell and Lambda Calculi

## Evaluation

While lambda calculus is exceedingly simple, there is a great deal of variety in the ways to evaluate and implement the reduction of lambda expressions. Different models of evaluation are called **evaluation strategies**.

There is a bifurcation between two points in the design space: 
strict vs non-strict evaluation.

An evaluation strategy is *strict* if the arguments to a lambda are necessarily evaluated before the lambda is reduced.

A language in which the arguments are not necessarily evaluated before the lambda is reduced is called *non-strict*.

Alternatively expressed, diverging terms are represented (up to equivalence) by bottom value, `⊥`.
>A function `f` is *strict* if `f ⊥ = ⊥`
>A function `f` is *non-strict* if `f ⊥ ̸= ⊥`.

In a strict language, should bottom occur anywhere in the exp, the execution will panic, *always*. In a non-strict language, the panic may or may not occur, depending on whether the term with the bottom is forced. For example

```hs
x = fst (1, undefined) -- ok
y = snd (1, undefined) -- ***Exception: Prelude.undefined

z = let x = 3
    in let y = undefined
       in  x + 5       -- ok

foldr (\ x z -> x) (error "headon") [1,2,3]      -- 1
foldr (\ x _ -> x) (error "headoff") []          -- ***Exception: headoff
foldr (\ x _ -> x) (error "ohi") [1,undefined]   -- 1

foldl (\ z x -> x) (error "björk") [1,2,3]       -- 3
foldl (\ _ x -> x) (error "bjerk") []            -- ***Exception: bjerk
foldl (\_ _ -> ()) (error "bjerk") []            -- ***Exception: bjerk
foldl (\_ _ -> ()) (error "bjerk") [1]           -- ()
foldl (error "oh") (error "bjerk") []            -- ***Exception: bjerk
foldl (error "oh") (error "bjerk") [1]           -- ***Exception: oh
foldl (\ _ x -> x) (error "bjerk") [undefined,2] -- 2
```

## Evaluation Models

There are many evaluation models and their variants. The 3 dominant models are
- call-by-value: args are evaluated before a function is entered.
- call-by-name: args are passed into function unevaluated.
- call-by-need: args are passed into function unevaluated, but each arg is only evaluated once when needed, and then its value is propagated to all referants.

Given an app `M N`, the reduction in these evaluation models proceeds differently, as follows:

Call-by-value:
1. Evaluate arg `N` down to a value `V`
2. Evaluate `M` to a lambda `λx.E`
3. Evaluate (substitute) [x:=V]E

Call-by-name:
1. Evaluate `M` to a lambda `λx.E`
2. Evaluate (substitute) [x:=N]E

Call-by-need:
1. Allocate a thunk `T` for arg `N`
2. Evaluate `M` to a lambda `λx.E`
3. Evaluate (substitute) [x:=T]E

Terms that have a *normal form* in one model, may or may not have a normal form in another. Terms that diverge under call-by-value may not diverge under call-by-need and call-by-name. Since diverging terms are not necessarily evaluated before entry, more exp will have a normal form under these two models, then under call-by-value.

### Call-by-value

Call-by-value is an extremely common evaluation model. Many programming languages both imperative and functional use this evaluation strategy.

The essence of call-by-value is that there are 2 categories of expressions: terms and values. *Values* are lambda expressions in normal form that cannot be reduced further. *Terms* are expressions that are not fully reduced, but that cannot be further reduced under the rules of the reduction strategy (e.g. no reduction under lamnda). These are often in the *weak-heaad normal form*.

All arguments to a function will be reduced to normal form before they are bound inside the lambda, and reduction of the lambda only proceeds once all the arguments are reduced.

Call-by-value model has 2 rules for evaluation of applications:

```hs
  F → F′
----------- (E-App1)
F A → F′ A


  A → A′
----------- (E-App2)
v A → v A′

-------------------- (E-AppLam)
(λx.Aʹ)v → [x:=v]Aʹ
```

These rules should be interpreted like this:

The rule *E-App1* says:   
to evaluate an app `F A` 
(stated below the line on the left), 
first evaluate exp `F`, obtaining reduced `Fʹ` 
(i.e. `F → Fʹ`, which is stated above the line); 
so we end up with `Fʹ` (stated below the line on the right). 

Thus, the rules should be read clock-wise, starting with the bottom left exp.

The reduction `F → Fʹ` means a single step of evaluation is taken. This rule will fire until the exp is reduced all the way down to a value. Values are denoted by `v`. So, basically, evalute `F` across many intermediate exps, `Fʹ`, until it gets reduced to a value, `v` (usually, a closure value).

The rule *E-App2* deals with the evaluation of the argument exp `A` in the app `F A`. When the first rule is fired enough times so that `F` is reduced to `v`, this rules kicks in. Now the arg exp `A` is kept being evaluated until it gets reduced as much as possible, obtaining the arg `Aʹ`. The initial exp `A` might also reduce to a value - basically, values are lambda exp that cannot be reduced further anywhere, but some remain exp for whatever reason even though they can be reduced further; and some exp diverge. Here, it is not clear when to stop evaluating `A`. We should probably stop once the resulting reduct stabilizes (stops changing).

The last rule *E-AppLam* kicks in the last, and it states that reduction then proceeds by substitution. This is the last part of the application `F A`: even before both exps `F` and `A` have been reduced as much as possible, `F` better have been revealed as a lambda, say `λx.E`. So, after both `F` and `A` in the app `F A` are fully reduced, `F` will be a value, i.e. a closure, `λx.v`. Note that the full reduction of the lambda abs has resulted i n the body of the lambda also being reduced to `v`. That is, the whole lambda abs is a value (`v`), and its body is a value (`v`). The arg `A` is reduced to, say, `Aʹ`. So then we have exp `(λx.v)Aʹ`, which proceeds by substitution. We replace all occurences of `x` with `Aʹ` in the body `v`. This is what this rule denotes as `(λx.v)Aʹ -->> [x:=Aʹ]v`.

---

For lambda calculus, the call-by-value interpreter is quite simple. Part of the runtime evaluation of exps involves the creation of closures, which are maps (environment) that record the local variables in scope.

In our implementation, there are two possible values which reduction may converge on: `VInt` (for numbers) and `VClosure` (for functions).
