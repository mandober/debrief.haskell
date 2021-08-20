# Haskell Terminology and Definitions


## Evaluation
Haskell terms (expressions and values, particularly data ctors) have different levels of evaluation layers and a value can be evaluated in passes with teach pass peeling off a layer until it is fully evaluated (NF form).

A value starts completely unevaluated (frequently as a thunk), and as it gets evaluated, it starts to shed layers; a significant point in evaluation is when a value reached WHNF because that form reveals a value's tag (data ctor name), thereby completing pattern matching.

## Functions, application
Application is normally associated with functions (in their various flavors and shapes). Applying a function means invoking a function, while at the same time, passing to it, at least one arg. Specifying only a bare function name without passing it any args, means referencing that function, e.g. to pass it off as an arg to a HOF (although this is still the case when some, but not all, args are supplied, which passes a new function obtained from that partial application).

## Functions, operationally
Operationally, it is the substitution operation in which each supplied argument is bound to the corresponding formal parameter, then all the formal parameter's application occurrences are consistently replaced with their binding occurrence.

## Functions, elements
Teminology of function elements:

- function (type) signature
- function head(er)
- function body
- nested function
- subfunction
- function un/currying
- formal parameters
  - type parameter
    - concrete
    - ∀
    - ∃
    - skolem
    - rigid
    - forbidden type application (surrounded in braces)
    - with accompanying kind signature/annotation
  - value parameter
- parameter occurrences
  - binding occurrence (single)
  - application occurrences (none to many)
- arguments
  - type args (type-level type applications/instantiations)
  - value args (term-level expressions)
- function application
  - type application (type-level)
  - type instantiation
  - application of (term-level) args
- input value/type
- output value/type


## Functions, levels
As should be expected, Haskell provides functions in all flavors, shapes, sizes, ranks, levels, planes, types, kinds, circumstances, in at least dozens of orthogonal dimensions. Term-level functions (named function, lambdas, segments, infixed, prefixed, symbolic operators, data ctors, field accessors, etc.), type-level functions (type ctors, explicit type applications, type families, type associations), kind-level functions (is this it?).


## Normal Form (NF)
A fully evaluated value; for example when we show a value it will eventually be fully evaluated.

## seq function
Roughly, the seq function returns its second argument and ties the evaluation of its first argument to the evaluation of the second. In `seq a b`, the value of `a` is always evaluated before `b`.

## Weak Head Normal Form (WHNF)
Evaluated up to the first data constructor; `seq` evaluates its argument to WHNF.
