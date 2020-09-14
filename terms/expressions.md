# Expressions


expression type signature
infix operator application
prefix negation
lambda abstraction
let expression
conditional
case expression
do expression
function application
variable
general constructor
literals
parenthesized expression
tuple
list
arithmetic sequence
list comprehension
left section
right section
labeled construction
labeled update


```
exp      := infixexp :: [context =>] type       (expression type signature)
          | infixexp

infixexp := lexp qop infixexp                   (infix operator application)
          | - infixexp                          (prefix negation)
          | lexp


lexp     := \ apat1 … apatn -> exp      (lambda abstraction, n ≥ 1)
          | let decls in exp      (let expression)
          | if exp [;] then exp [;] else exp      (conditional)
          | case exp of { alts }      (case expression)
          | do { stmts }      (do expression)
          | fexp

fexp     := [fexp] aexp      (function application)

aexp     := qvar      (variable)
          | gcon      (general constructor)
          | literal
          | ( exp )      (parenthesized expression)
          | ( exp1 , … , expk )      (tuple, k ≥ 2)
          | [ exp1 , … , expk ]      (list, k ≥ 1)
          | [ exp1 [, exp2] .. [exp3] ]      (arithmetic sequence)
          | [ exp | qual1 , … , qualn ]      (list comprehension, n ≥ 1)
          | ( infixexp qop )      (left section)
          | ( qop⟨-⟩ infixexp )      (right section)
          | qcon { fbind1 , … , fbindn }      (labeled construction, n ≥ 0)
          | aexp⟨qcon⟩ { fbind1 , … , fbindn }      (labeled update, n  ≥  1)
```
