# 3.3 Curried Applications and Lambda Abstractions

https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-260003.3

```js bnf
fexp ::= [fexp] aexp                  (function application)
lexp ::= \ apat₁ … apatₙ -> exp       (lambda abstraction, n > 0)
```

* Function application is written `e1 e2`
* Application associates to the left
* the parentheses may be omitted in `(f x) y` ≡ `f x y`
* `e1` could be a data ctor, so partial applications of data ctors are allowed
* Lambda abstractions are written `\ p1 … pn -> e`, where the `pᵢ` are patterns
* An expression, `\x:xs -> x` is syntactically incorrect; it may legally be written as `\(x:xs) -> x`.
* The set of patterns must be linear - each var must appear only once

Translation:

The following identity holds:

`\ p₁ … pₙ -> e` = `\ x₁ … xₙ -> case (x₁, …, xₙ) of (p₁, …, pₙ) -> e`

where the `xᵢ` are new identifiers.

Given this translation combined with the semantics of `case` expressions and pattern matching described in Section 3.17.3, if the pattern fails to match, then the result is `⟘`.
