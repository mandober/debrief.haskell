# Eta-conversion

## η-conversion as a consequence of App

`λf.λx.(fx)` is an abstraction expression: the outer lambda binds `f`, the inner binds `x`, but they both share the same body, which consists of a *faux application* of `f` to `x`; this is not a *proper application*, aka a *redex*, because we don't know what `f` os (or `x`). Intuitivelly, we expect this abstraction will be applied to two arguments (fargs or funargs; this is untyped LC), bind each appropriately, and only then will it be possible to perform this application.

>However, if the problem is not knowing what `f` is, does that mean we can try and reduce this application as soon as we recieve the first argument? Even though `x` would still be unknown? Does not knowing what `x` is, matters at all, at any point?

(spolier: no. at least for suitable, η-reducable expressions)

Note: we analyze lambda expressions from the point of view of procedures that consume them; that is, we focus on a single expression at the time, not knowing whether it represents the entire (toplevel) expression, or just one of its (deeply buried) subexpression (i.e. *we never know where we are*).

When we apply the abs to the first arg (let it be the `I` combinator although the concrete arg does not matter), we get a proper application:

      (λf.λx.fx)(λa.a)

whose reduction proceeds in various ways, according to the reduction strategy, but that's unimportant here as both left and right operands are fully evaluated. So the next step in any strategy is surely to perform β-reduction which yields the following contractum:

      λx.((λa.a)x) ≡ λx.(λa.a)x ≢ λx.λa.ax

Note: Careful now, application binds tighter then abstraction.

>It is at this point that it becomes possible to reduce the body of the original lambda, even if we still don't know what `x` is. Is this correct?

There are a few cases to analyze.

1. Had we performed this reduction immediately, we'd get the same (first) arg back modulo renaming:

λx.((λa.a)x) ⟶ λx.x

Passing in `λa.a`, but receiving `λx.x` back (so, same exp % renaming).

2. Instead, if we recognize the current exp as being suitable for η-reduction, after η-contraction, we'd (again) get the I combinator back ("modulo renaming" not necessary).

λx.((λa.a)x) ⟶ λa.a

These first two results are equivalent (modulo renaming).

3. Another option is to apply the current exp to another (second arg in terms of the initial lambda abstraction) argument (like things were supposed to unfold from the begining), say the K combinator.

(λx.(λa.a)x) K ⟶ (λa.a) K ⟶ K

then we'd get that arg back (here, K). 

It seems whichever way we proceed we get the same result.

And just to cover the case where the original lambda abstraction was initially immediately applied to two args, I and K:

(λf.λx.fx) I K ⟶ (λx.I x) K ⟶ I K ⟶ K

or

(λx.((λa.a)x)) (λs.λz.z) ⟶ (λa.a)(λs.λz.z) ⟶ λa.a

>Conclusion: a timely η-contraction can save us from performing a lot of useless reductions.

The original abstraction has a convenient shape such that applying it to any arg `M` yields a form ideal for η-reduction:

(λf.λx.fx)M ⟶ λx.Mx =η= M

Perppforming the η-reduction is much more satisfying then "waiting for" an additional arguemnt and then going down the substitution road:

(λf.λx.fx)M ⟶ (λx.Mx)N ⟶ [x:=N]Mx
