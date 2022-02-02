# Operator Applications

3. Expressions ❱ 3.4 Operator Applications

https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-280003.4


```js bnf
infixexp ::= lexp qop infixexp
           | '-' infixexp                       (prefix negation)
           | lexp

qop      ::= qvarop
           | qconop                             (qualified operator)
```



The form `e₁ 𝙦𝙤𝙥 e₂` is the infix application of a **binary operator** `𝙦𝙤𝙥` to expressions `e₁` and `e₂`.

The special form `- e` denotes **prefix negation** (negation is the only prefix operator in Haskell) and is an alternate syntax for `negate e`.

- `Prelude.negate   :: Num a => a -> a`
- `Prelude.subtract :: Num a => a -> a -> a`
- `(Prelude.-)      :: Num a => a -> a -> a`


```hs
-- interpreted as `negate`
(- 5) :: Num a => a

-- interpreted as `subtract`
(5 -) :: Num a => a -> a

((+ 3) . (5 +)) :: Num a => a -> a
((- 3) . (5 -)) :: (Num a, Num (a -> b)) => a -> b
--                         ^^^^^^^^^^^^
--                      sign o' the error
```

The binary operator `-`, i.e. **minus**, doesn't necessarily refer to the definition of `(Prelude.-)` since it may be rebound. However, the unary operator `-`, i.e. prefix negation, will always refer to the `Prelude.negate` function. *There is no association between the local meaning of the `-` operator and the unary negation*.

Prefix negation has the same precedence as the infix operator `-` defined in the Prelude (infixl 6).

Because `e₁ - e₂` is parsed as an infix application of the binary operator `-`, one must write `e₁ (- e₂)` for the alternative parsing.

A section where any binary infix (symbolic) operator, `⊙`, is by itself in parens, as in the form `(⊙)`, is interpreted as the lambda `(\x y -> x ⊙ y)`. This also applies to the section `(-)` which is therefore interpreted as the lambda `(\x y -> x - y)`, not as `(\ x -> -x)`; you must use `negate` for that.

The following identities must hold:

```
e1 op e2 = (op) e1 e2
      -e = negate (e)
```
