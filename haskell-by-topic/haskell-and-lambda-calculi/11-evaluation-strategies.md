# Evaluation strategies

- evaluation strategy
- reduction strategy
- strict
  vs call-by-value
  vs applicative order
  vs rightmost-outermost order
- is applicative order ≡ rightmost-outermost order


## Eval strategy

- eager:           reduce args of applications
- lazy:     do not reduce args of applications
- deep:            reduce inside abstractions
- shallow:  do not reduce inside abstractions

We can combine these to get various reduction strategies:

EVAL    | Eager             | Lazy
Shallow | weak normal form  | weak head normal form
Deep    | normal form       | head normal form

In terms of programming language terminology, *weak normal form* corresponds approximately to *call by value* and the *weak head normal form* to *call by name*.



## Variety

There is a great deal of variety in ways to evaluate and implement the reduction of lambda expressions. The different models of evaluation are called evaluation strategies.

Applying an expression `M` to an expression `N`, `M N`, that is, applying a function (a lambda) to an argument may proceed in different ways.

There is a bifurcation between two points in the design space:
- strict evaluation
- non-strict evaluation

An evaluation strategy is *strict* if the arguments to a lambda abstraction are necessarily evaluated before the abstraction is reduced.

An evaluation strategy is *non-strict* if the arguments to a lambda abstraction are not necessarily evaluated before the abstraction is reduced.

In other words, *diverging terms* are represented *up to equivalence by the bottom value*, written as `⊥`.
- a function `f` is non-strict if `f ⊥ ≠ ⊥`
- a function `f` is strict if `f ⊥ = ⊥`

Strictness has more to do with how bottom is treated then with the *order of evaluation*.

**Evaluation order** specifies which redex is first reduced
- innermost leftmost
- outermost rightmost

When applying a lambda to an argument, evaluation order is also specified as
- normal order
- applicative order

In the **applicative order**, `M N`, the argument `N` is reduced (as much as possible). The lambda `M` is reduced (as much as possible), soon in the procces revealing it is indeed an abstraction, `λy.E`. So the application `M N` becomes `(λy.E) A` with both operands evaluated as much as possible.

```
  M → Mʹ             N → Nʹ
----------         ----------           -------------------
M N → Mʹ N         v N → v Nʹ           (λx.B)A ⟶ᵦ [x:=A]B
```

Then the argument `A` is bound to the lambda's formal parameter `x`, and the substitution, `[x:=A]B` is performed.

In the **normal order**, `M N`, the first operand `M` is evaluated just enough to reveal its data ctor, which should be an abstraction (revealing some other term could constitute an error since only an abstraction can be applied; later about this - it is another matter). At that point, the lambda is in the form called *weak-head normal form*. Basically, the data ctor is known to be an abstraction; implementationally, the data ctor `Abs` is revealed along with its first paramater which is a `Name` (actually a `String`) that represents (the name of a) variable - actually, in this case, it is the name of the formal parameter of the lambda abstraction. This means the form of the lambda is now `Abs x _` out of `Abs String Term`, where `x` is a formal parameter, like "x". Logically, this is `(λx.B)N`. This is enough information to proceed: we bind the arg `N` to the formal param `x` to perform the substitution, `[x:=A]B` (which is bound to require some additional evaluation).


<details><summary>About laziness</summary>

NOTE: Actually, I have no idea if these details are correct, especially the exact opportunities the evaluation is allowed to continue.

In general, the **lazy evaluation strategy** means that the evaluation of any bit of syntax is delayed until that piece is absolutely required. But what does "absolutely required" means in the context of evaluating a lambda exp; i.e. when can a piece of syntax be *referenced* albeit not being *done* vs the situations when it must be *forced*? Because referring to a piece of syntax can succeed despite that piece not being evaluated at all - like when the application of a lambda to an arg succeeds, even though the arg is unevaluated. So, when must a piece be evalujated and how much? And when does a piece of syntax needs to be forced (does *forced* mean the exp must be completely evaluated, or is it ok to leave it somewhat undercooked?). What are the possible stages (I guess "forms") an exp goes through during cooking, besides the wel-known ones like "raw" (completely untouched), WHNF, HNF, and NF.

>How is evaluation exactly interleved with other actions during the process of β-reduction? Or any other process, in general?
where the "actions" include: binding an arg to the formal parameter, doing the substitution, checking for free vars, renaming things and such.

It seems the evaluation goes though this process many times: it starts, cooking an exp for a while (for a step or two, usually just a single step), then stops (leaving the exp *undercooked*, or, at least, not *well-done*). This is then probably *interleaved* with other processes - surely the evaluation kicks in at times during the substitution process? But what is the exact order - that is what is unclear.

>Does an evaluation step necessarily reduces (or at least changes) an exp?
Because the evaluation as it pertains to LC is here accompanied with bits of evaluation done by Haskell. There is eval is LC which invloves reduction, but there is also the Haskell's notion of evaluation that may not necessarily involve reduction. For instance, pattern matching only reveales which data ctors we are dealing with. So, is a redex always a reducable expression?

No wonder, many implementors opt into strict or call-by-value or applicative order or whatchamacallit strategy...

</details>

Substitution itself has its own set of rules and egde cases.

## Evaluation Models

There are many different models, and various hybrids thereof, but the three dominant models:
- call-by-value: arguments are evaluated before a function is entered
- call-by-name: arguments are passed unevaluated
- call-by-need: arguments are passed unevaluated but an expression is only evaluated once and shared upon subsequent references

Given an expression fx the reduction in different evaluation models proceeds differently:

Call-by-value:
1. Evaluate x to v
2. Evaluate f to λy.e
3. Evaluate [y/v]e

Call-by-name:
1. Evaluate f to λy.e
2. Evaluate [y/x]e

Call-by-need:
1. Allocate a thunk v for x
2. Evaluate f to λy.e
3. Evaluate [y/v]e

Terms that have a normal form in one model, may or may not have a normal form in another. In call-by-need and call-by-name evaluation diverging terms are not necessarily evaluated before entry, so some terms that have a normal form in these models may diverge under call-by-value.

## Fixpoints

Y = SSK(S(K(SS(S(SSK))))K)
