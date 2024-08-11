# Implementation notes

## Top-level lambda expression

There can be only a single lambda expression at the top-level, i.e. the entire lambda calculus program is made of a single lambda expression `T`.

The top level expression `T` cannot reasonably be a variable bacause such variable occurs *free* and is thus undefined. In fact, the entire exp better be *closed*, otherwise encountering a free variable anywhere would be an error, at least in a strict setting. Loosely, we may acommodate free variables, stopping the evaluation when we encounter one, but such an approach serves no purpose; in almost all PLs (save for bash or JS, which at least have a reserved value - namely, null and 'undefined', respectively - to initialize the variable with in such cases), referencing an undefined variable is a hard error.

It is the most reasonable that the top-level expression is an *application*. Since LC does not support top-level bindings (unless we extend it ourself by introducing top-level *let-bindings*), `T` should be an application `M N`, where `M` is a lambda abstraction with as many formal parameters as there are "standard library" functions we want to have easily available. `N` should then be a sequence of arguments, each defining one lib function. Each formal parameter should be named according to the function it binds.


```hs
((((((
  λ I .
    λ K .
      λ S .
        λ B .
          λ C .
            {- lambda body: main exp ... -}
  )
(λx.x))
  (λxy.x))
    (λgfx.gx(fx)))
      (λgfx.g(fx)))
        (λgxy.gyx))
```

Yes, this is a mess so people more often go with the top-level let-bindings.

```hs
let I = λx.x
let K = λxy.x
-- ...
let C = λgxy.gyx

-- main exp
-- ...
```

## Evaluating a program

The evaluation of a lambda program begins by scrutinizing the top exp `T` to reveal its data ctor.
- if `Var _`, we are either done, returning the same exp, or we emit an error.
- if `App M N` is the most common case
- if `Abs x B` is usually fine as well

The abstraction as the top level exp is dubious especially under the evaluation strategies that do not reduce under a lambda. Under such a strategy, we'd be done; otherwise, we'd proceed with to reduce under the lambda, i.e. to reduce its body, the exp `B`.

The end values of a program's evaluation are usually abstractions that stand for encoded values. So, having an abstraction immediately, as the top level exp, may not be so strange after all.

## Things to consider

- canonical forms
- normal forms
- what are values
- "\\" or "\955" vs "λ"
- "->" vs "→" vs "."
- compacting the binders
- show vs pretty-print
- scheme for generating fresh names, y ⟼ yʹ, y″, y‴ or y₀, y₁, y₂
  - mapping ints to subscript ints, {0-9} ⟼ {₀˗₉}
- relations
  - `~~>`      Haskell evaluation (e.g. in pattern matching)
  - `≡`        syntactic equality
  - `⟶ᵦ`       one-step β-reduction,  `ᵦ⟵`
  - `⟶ᵦ⋆`      many-step β-reduction, `ᵦ⋆⟵`
  - `=ᵦ=`      β-conversion, `=ᵦ`, `=β=`
  - `=α=`      α-equivalence, α-conversion
  - `=η=`      η-conversion: η-contraction/η-reduction, η-expansion
  - [x:=A] B    Substitution
  - [x:=xʹ] E   Renaming
  - `Abs x M`   Abstraction
  - `App M N`   Application


T ~~> F A ~~> (λx.E) A ⟶ᵦ [x:=A]E

where `~~>` is pattern matching in Haskell done to reveal the data ctor.

e.g.

`T` ~~> `F A` ~~> `(λx.x) (λy.y)` ⟶ᵦ `[x:=λy.y]x` ≡ `λy.y`
     ↑         ↑                   ↑               ↑
pattern matching                β-reduction      equality


## Substitution

```hs
-- subject is on the left of brackets
(λx.λy.x y) C D
= ((λy.x y)[x:=C])[y:=D]
= (λy.C y)[y:=D]
= C D
```
