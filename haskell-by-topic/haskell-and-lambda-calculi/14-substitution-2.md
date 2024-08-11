# Substitution

## Substitution procedure

1.      x[x:=N] ≡ N                   since x = x
1.      y[x:=N] ≡ y                      if x ≠ y
2.  (P Q)[x:=N] ≡ (P[x:=N]) (Q[x:=N])
3. (λy.P)[x:=N] ≡ λz.(P[y⟼z][x:=N])  if `λz.P[y⟼z]` is an α-variant    
                                      of `λy.P` such that `z ∉ FV(N)`

## Substitution procedure details

A β-reduction step can be performed anywhere a function meets an argument. A redex is an application `M N` where the left operand `M` is an abs and the right operand `N` is an arg.

(λx.e)a ⟶ᵦ e[x:=a]

Where the `e[x:=a]` notation means that all free occurrences of variable `x` are replaced by an arg `a` in the expression `e`.

`[x:=a]e` aka `subst x a e`

```hs
--       name      arg    body   result-exp
subst :: String -> Exp -> Exp -> Exp
```

There are many degrees of evaluation to choose from: Weak Head Normal Form, Head Normal Form, Normal Form, etc. They differ in exactly where there might be reducible expression lingering.

We start with an easy evaluation strategy, *normal order to WHNF*.

To get WHNF we only need to make sure that there is no redex along the *spine* of the expression, i.e. starting from the root and following the left branch in applications.

```hs
((λxy.y) (λa.a)) (λb.b)

       @   
     /   \ 
    @    λb
  /   \   |
λx    λa  b
 |     |   
λy     a   
 |         
 x         
```


## Normal order reduction

Doing *normal order reduction* means we do not evaluate the argument of a β-redex. This is sometimes called lazy evaluation, but the term "lazy evaluation" can also signify an implementation strategy for normal order reduction. Not reducing the argument is also sometimes called *call-by-name*.

Weak head normal form (WHNF) means we do not reduce under lambdas.

We implement normal order WHNF by walking down the *spine* of a lambda expression collecting the args (i.e. right branches of applications) in a list until we reach an abs or variable.

If we encounter a variable we already have a WHNF, so we just reconstitute the applications again.

If we encounter an abs, we perform a β-reduction. We have an app `M @ N`, so we need to reveal that the left operand M is indeed an abs, in which case the exp is `(Abs x B) @ A`. We do not evaluate the arg `A` but immediately proceed with substitution: replacing all free occurrences of the variable `x` inside the lambda body `B` by the argument `A`, i.e. `[x:=A]B`, the job of the 'subst' fn.

```hs
whnf :: Expr -> Expr
whnf ee = spine ee []
  where
  spine (App f a) as = spine f (a:as)
  spine (Lam s e) (a:as) = spine (subst s a e) as
  spine f as = foldl App f as
```
