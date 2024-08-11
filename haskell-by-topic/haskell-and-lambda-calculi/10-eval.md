# Eval

Initially, we have an unknown lambda exp `M` as the sole top-level exp. Plain LC, considered as a PL, does not have imports, declarations, external bindings, or any such feature, so any "program" always consists of a single top-level exp.

```hs
data Term
  = Var Name
  | Abs Name Term
  | App Term Term

type Name = String
```

Evaluation proceeds by scrutinizing `M` to reveal its data ctor:
1. Variable,    `Var x`
2. Abstraction, `Abs x b`
3. Application, `App m n`

1. `Var`: if `M` is a variable `Var "x"`, then it has the logical form `x`, so there is nothing to do. The exp evaluates to itself, i.e. remains as a var `x`. In fact, a sole variable like that is *free* and thus undefined (and makes little sense as a complete program).

Variables are constructed using the data ctor `Var` that takes a `String`, so unlike abs and app, vars are *flat* lambda terms, representing the leaves of an exp tree. As flat lambda terms, encountering a `Var "x"` terminates the evalution.

Variables are peculiar because they are used both as formal parameters (to bind args) and as applied parameters (in the lambda body). However, as a formal param, a var is not `Var "x"`, but just the string `"x"` - it has the form `Var "x"` only as an applied param. Yet the string `"x"` and `Var "x"` are obviously connected, both representing the same logical variable `x`.

2. `Abs`: if `M` is an abstraction `Abs x b`, then its logical form is `λx.B`, where `x` is a string signifying the name of the formal param (i.e. variable's binding occurrence), and `b` is the lambda body, an unknown exp.

Finding a lambda at the top level make sense because abstractions are *values*, but they are assigned meaning outside the language. Many lambdas have an interpretation, representing certain values, according to an *encoding schema*; e.g. `λxy.x` is almost always interpreted to mean the Boolean constant 'true', the meaning assigned to it under the *Church Booleans* encoding schema.

The revealed abstraction, `λx.B`, may contain redexes in `B`. However, whether the evaluation goes under the lambda is dictated by a reduction strategy.
- If the reduction strategy *does not evalute under lambdas*, then the evaluation is over, and the exp remains as is. 
`λx.B ->> λx.B`.
- If the reduction strategy allows *evaluting under lambdas*, then 
`λx.B ->> λx.(eval B)`.

Evaluation is based on pattern matching which reveals the data ctor of the next (nested) exp, until an application `M A` is encountered. Then the "real" evaluation takes place. First we make sure (via pattern matching) that `M` is an abstraction, `λx.B`. Under the call-by-value, we reduce the arg exp `A` as much as possible. We also cook `B` to see what is it. If it is another abs, `λy.E`, then we need to check if `x == y`:
- x == y
- x /= y
- y ∉ FV(A)
- y ∈ FV(A) includes renaming (α-equivalence) of y to yʹ in B

Then we perform β-reduction: the var `x` binds the arg `A`, and proceeds by substitution. We substitute all free occurrences of `x` in the lambda's body `B` with the (cooked) arg `A`.

oh, god...



### Evaluting under lambdas

So we have top exp `λx.B` and work under the reduction strategy that allows (and insists on) evaluating under lambdas. The next thing we should do is check whether the var `x` occurs in `B`.

- 2.1 If `B` *doesn't contain* `x` at all, we proceed to evaluate `B` carelessly.

λx.B ∧ x ∉ B ->> λx.(eval B)

- 2.2 If `B` contains *only free occurrences* of `x`, then all these occurrences of `x` are in fact bound by the outmost binder.

- 2.3 If `B` contains *only bound occurrences* of `x`, then these `x`'s in `B` are *distinct vars* from the outer `x`; they (there may be many distinct vars `x`) have their own binders inside `B`. The outer `x` is shadowed in `B`.

- 2.4 If `B` contains *both bound and free occurrences* of `x`, then the bound occurrences are distinct vars `x`, and free are bound by the outer binder.
