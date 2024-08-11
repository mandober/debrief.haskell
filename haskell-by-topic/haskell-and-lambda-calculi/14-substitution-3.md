# Substitution

## Substitution procedure

1.      x[x:=N] ≡ N                   since x = x
1.      y[x:=N] ≡ y                      if x ≠ y
2.  (P Q)[x:=N] ≡ (P[x:=N]) (Q[x:=N])
3. (λy.P)[x:=N] ≡ λz.(P[y⟼z][x:=N])  if `λz.P[y⟼z]` is an α-variant    
                                      of `λy.P` such that `z ∉ FV(N)`

## Substitution procedure details

Substitution takes place when, during the evaluation, we encounter an application `M N`. We'll call this application the *initial application* in the text below; it prompts β-reduction, which, at some point, performs substitution. Before that, β-reduction, depending on the strategy, may reduce the argument exp `N` into exp `A`. Regardless of whether the strategy demands the reduction of the arg `N` or not, we'll now denoted that arg as `A`.

Next, we need to make sure `M` is really an abstraction; if it's not, the initial application is not a redex so there's no β-reduction, and at this point we can just return/leave the exp as is, or report an error. For example, the app `x(λy.y)` cannot be reduced. If `M` is indeed an abstraction then the initial application is of the form `(λx.B)A` and we proceed with substitution.

    M N ~~> (λx.B) A ~~> [x:=A]B

The substitution means replacing all *free occurrences* of the *binding var* `x` in the lambda body `B` with the argument `A`, denoted by `[x:=A]B`.

Substitution proceeds by casing the lambda body `B`, which produces 3 cases, each corresponding to `B` being one of the 3 lambda terms: Var, App or Abs.

## Var case

The lambda body `B` is a variable `y`, meaning the initial app was `(λx.B)y`, so the substitution has the form `[x:=y]B`.

```
        M      N
    ~~> (λx.B) A
            ↓
    ~~> (λx.y) A ~~> [x:=y]B
```

In the initial application `M N`, the arg `N` was cooked for a bit (or not) becoming the arg `A`, and exp `M` was revealed to be an abs `λx.B`. The substitution at this point looked like `[x:=A]B`, but then `B` was revealed to be a variable `y`. So now we actually have the substitution `[x:=A]y`.

>The crucial question at this point is whether the meta-vars `x` and `y` are actually the same variable, i.e. whether they have the same name?

This has 2 subcases:
1. x = y
   in this case the current exp is really `(λx.x)A`, 
   so the substitution `[x:=A]x` gives `A`.
2. x ≠ y
   in this case the current exp is really `(λx.y)A`,
   so the substitution `[x:=A]y` gives `y`.
   Since the binding var `x` does not occur in the lambda body (the body is a var `y`), the argument is discarded and the body, i.e. var `y`, is returned.

Operationally, in Haskell, the substitution is the function `subst`, which so far loos like this:

```hs
-- | (λx.B)A ~~> [x:=A]B
subst :: Name -> Exp -> Exp -> Exp
subst x arg body = case body of
  Var y | x == y    -> arg
        | otherwise -> Var y
```

## App case

In case the body is an app, we have:

    (λx.B)A ~~>
    [x:=A]B ~~>
    [x:=A](M N)

The subs `[x:=A](M N)` is done by performing the same substitutiont for each operand: 
- replace all free vars `x` in `M` with arg `A`
- replace all free vars `x` in `N` with arg `A`
- reconstruct the application

    [x:=A](M @ N) = [x:=A]M @ [x:=A]N

Operationally:

```hs
-- | (λx.B)A ~~> [x:=A]B
subst :: Name -> Exp -> Exp -> Exp
subst x arg body = case body of
  -- 1. (λx.y)A ~~> [x:=A]y
  Var y
      -- 1.1 (λx.x)A ~~> [x:=A]x = A    (if x = y)
    | x == y    -> arg
      -- 1.2 (λx.y)A ~~> [x:=A]y = y
    | otherwise -> Var y
  -- 2. (λx.M N)A ~~> [x:=A](M N) = ([x:=A]M) ([x:=A]N)
  App m n = App (subst x arg m)
                (subst x arg n)
```

## Abs case

If the body exp `B` is another abstraction, then the complications begin.

    (λx.B)    A  →  [x:=A]B
    (λx.λy.E) A  →  [x:=A](λy.E)

We now have an app whose left operand consists of 2 abstractions: 
- `λx.`  : outer abs with binder `x` and body which turns out to be...
- ...`λy.E` : inner (nested) abs with binder `y` and body `E`
- applied to an arg `A`

The conditions:
- y ∉ FV(A)
- x ∉ FV(E)


    (λx.λy.E) A  →  [x:=A](λy.E) → 
    if x = y:
              (λx.λy.E) A  →  [x:=A](λy.E) = λy.E
                   ↓
              (λx.λx.E) A  →  [x:=A](λx.E) = λx.E
    if x ≠ y:
      if y ∈ FV(A):
              (λx.λy.E) A  →  

      if y ∉ FV(A):
              (λx. λy.E) A          → then rename binding/bound `y` in `E`:
                    ↓
              (λx. λyʹ.E)           → occurrences of `y` in `E` are now `yʹ`
              (λx. λyʹ.[y:=yʹ]E) A  →
              [x:=A](λyʹ.[y:=yʹ]E)
