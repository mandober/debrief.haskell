# Renaming

Renaming of variables in a lambda expression is needed in order to avoid name capture, which is consequence of naive substitution. As a general rule: the vars that were free before substitution must remain free after it; or, more to the point: the meaning of an exp must not change.

Renaming only ever changes the names of bound variables, it never touches free variables because that would be a sure way to change the meaning of an exp.

Since renaming only deals with bound variables, it means we need to change the name of the binding occurrence of a variable - varible that is introduced by a lambda binder - as well as all occurrences (so-called applied occurrences) of that variable in the lambda's body (of which there may be none, one, or many).

The case where a var only appears in at the binding site, but does not occur in the lambda's body is particularly tricky because, if overlooked, that binder may become the nearset binder that captures a free var. Here's an example:

(λf.λx.fx)(λf.λx.fx) ≢ λx.xx ≡ λyx.yx

What is especially disconcerning is that this is a closed expression - all variables are bound, even though the Barendregt convention is not honored (having distinct names for variables). No, in fact, even if the Barendregt convention was honored (as in the expression below), we can still end up in a binder: exp `(λa.aa)(λf.λx.fx)` is a bona fide closed expression that respects the Barendregt convention, but after one step we are back to the original, problematic expression, `(λf.λx.fx)(λf.λx.fx)`.


(λa.aa)(λf.λx.fx) ⟶ᵦ (λf.λx.fx)(λf.λx.fx)




The procedure of renaming 

, `M[x :=ᵅ xʹ]`


The exp `M[x := xʹ]` denotes the result of replacing 
every free occurrence of `x` by `xʹ` in the exp `M`.

**Renaming relation**, `=α=`
`λx.M` =α= `λxʹ.M[x := xʹ]`, provided that
1. `xʹ ∉ FV(M)` and
2. `xʹ` is not a binding variable in `M`


`λx.M` =α= `λxʹ.M[x := xʹ]`
