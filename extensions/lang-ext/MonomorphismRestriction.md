# MonomorphismRestriction

Lang Pragma: `MonomorphismRestriction`


## The Monomorphism Restriction

Haskell places certain extra restrictions on the generalization step (beyond the standard Hindley-Milner restriction) which further reduces polymorphism in particular cases.

The monomorphism restriction depends on the binding syntax of a variable.

A variable is bound by either
- function binding
- pattern binding
  - *simple pattern binding* is a var binding by an *irrefutable pattern*


The following two rules define the monomorphism restriction:

1. A given declaration group is **unrestricted** iff
  a) every var in the group is bound by a function 
     binding or a simple pattern binding, and
  b) an explicit type signature is given for every variable
     in the group that is bound by simple pattern binding.

The usual Hindley-Milner restriction on polymorphism is that only type vars that don't occur free in the env may be generalized. In addition, the constrained type vars of a restricted declaration group may not be generalized in the generalization step for that group.

2. Any *monomorphic type vars* that remain when type inference
   for an entire module is complete, are considered *ambiguous*,
   and are resolved to particular types using the *defaulting rules*.
