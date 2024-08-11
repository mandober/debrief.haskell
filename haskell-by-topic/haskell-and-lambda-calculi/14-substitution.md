# Substitution

## TL/DR

Substitution is a nightmare, operationally and notationaly. For example, a well-known source like Wikipedia presents the substitution procedure that turns out to be incorrect. There seems to be an infinite supply of edge cases for free variables to wrack havoc. The latest procedure deemed to be correct is taken form "Type Theory and Formal Proof - An Introduction" by Rob Nederpelt and Herman Geuvers (2014).

## Substitution procedure


1.      x[x:=N] ≡ N                   since x = x
1.      y[x:=N] ≡ y                      if x ≠ y
2.  (P Q)[x:=N] ≡ (P[x:=N]) (Q[x:=N])
3. (λy.P)[x:=N] ≡ λz.(P[y⟼z][x:=N])  if `λz.P[y⟼z]` is an α-variant    
                                      of `λy.P` such that `z ∉ FV(N)`


## Substitution procedure details


## Around the time substitution takes place

Substitution is a part of β-reduction of a lambda exp `M` to exp `N`. In `M N`, there comes a point when `M` is revealed an abs `λx.B` and `N` is evaluated (or not) down to exp `A` (possibly a NF), so the app `M N` becomes `(λx.B)A`. At that point we perform the substitution, denoted by [x:=A]B.

So, again, an inital lambda exp `L` is revealed (in Haskell, via pattern matching) to be an app `M N`, which means `M` better be a function (also revealed via pattern matching in Haskell) of the form `λx.B`. The exp `N` is its argument, but according to the evaluation strategy `N` may not be evaluated at all or it may be evaluated down to a normal form (is one exists) before the lambda `λx.B` is applied to it. In any case, let's say `N` has become `A` ('A' as in argument) when the function is applied, `(λx.B)A`.

L >>> M N >>> (λx.B)A ⟶ᵦ [x:=A]B

Here, and perhaps in further text, `>>>` stands for evaluation performed in Haskell, usually via pattern macthing, while `⟶ᵦ` is a single-step β-reduction, and `->>ᵦ` is a multi-step β-reduction performed in LC.





## Substitution, notationaly

Substitution notation is fucked across the board. People use all possible permutations of var, arg and exp. Fortunatelly, the substitution brackets are pretty standardized, so the subject expression in which the substitution takes place is never ambiguous, but the parameter and argument are. Some authors prefer the form `[x/y]e`, others `[y/x]e`, even both forms signify `[x:=y]e`. Using a directional symbol for binding should be the law: `:=` or `⟼`, or any other symbol as long as it's obviously *directional*.

`[x:=a]e`

The placement of the subject exp is less of an issue with simple substitutions, but can impair readability with *parallel substitutions*. It seems more natural to place the **subject on the left of the brackets**, then a parallel subst is denoted by:

((λy.xy)[x:=C])[y:=D]

vs when the subject on the right of brackets: [y:=D]([x:=C](λy.xy)). Granted, other, more complex, substitution acrobatics may prove otherwise.

## Substitution, operationally

`F A` ~~> `(λx.E) A` ⟶ᵦ [x:=A]E



In an application, `F A`, we first make sure that `F` is an abs, `λx.E`, before we proceeds with β-reduction.


β-reduction proceeds by substitution, denoted `[x:=A]E`, which depends on what `E` is, so we must scrutinize it:
1. `Var y`: if `E` is a var, we check if `x == y`:
  - (1.1) if `x == y`, it means we had `(λx.x)A`, and it reduces to arg, `A`.
  - (1.2) if `x /= y`, it means we had `(λx.y)A`, and it reduces to body, `y`.

2. `App M N`: in case `E` is app, we preform the substitution in each operand:

F A -->> λx.E -->> [x:=A]E -->> case E of 
  [x:=A](M N) -->> ([x:=A]M) ([x:=A]N)

3. `Abs y B`: if `E` is abs, `λy.B`, then we had `(λx.λy.B)A`. The subst proceeds by checking if `x == y`:

  - (3.1) if `x = y` then we have *(λx.λx.B)A*, which reduces to `λx.B`
    `F A` -->> 
    `(λx.E) A` -->> `(λx.λy.B) A` -->> (x=y) -->> `(λx.λx.B) A` -->> `λx.B`

  - (3.2) Here we know that `x != y` so we have *(λx.λy.B) A*.

    We check whether `y ∈ FV(A)?`

    - (3.2.1) if `y ∉ FV(A)`, it means `y` is not free in the arg `A` - it may be bound by some inner local binder. So we have *(λx.λy.B)A* which reduces to `λx.B` (which is the original lambda's body `E`), like above in (3.1).
    `F A` -->> 
    `(λx.E) A` -->> `(λx.λy.B) A` -->> (x≠y) -->> `(λx.λy.B) A` -->> `λx.B`

    - (3.2.2) else we know that `y ∈ FV(A)`, i.e. `y` is free in the arg `A`.
  
    This requires performing renaming to reduce correctly. We rename the binding occurrence of `y` as well as the free occurrences of that `y` in the scope of that binder, `y ->> yʹ`, which may be expressed as a subst: [y:=yʹ]B. After renaming, the new subst looks like: `[x:=A](λyʹ.B), the reduction of which is the result of eval.

```
      F A
-->> (λx.E) A
-->> (λx.λy.B) A
                    x≠y
-->> (λx.λy.B) A
                    y ∈ FV A
     =α= [y:=yʹ]B
-->> (λx.λyʹ.B) A
     =ᵦ= [x:=A](λyʹ.B)
```


```hs
-- (1) Var 1.1 (λx.x)A → A
App (Abs "x" (Var "x")) ARG -->> ARG -- whatever exp ARG is

-- (1) Var 1.2 (λx.y)A → y
App (Abs "x" (Var "y")) ARG -->> Var "y"
```



 means replacing all occurrences of var `x` by `A` in exp `E`.

Substitution depends on what `E` is, so we scrutinize it:

If `E` is:
- `Var y`: then if `x == y` then we have λ
                
