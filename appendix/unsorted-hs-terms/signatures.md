# Signatures

Any function must have the function type ctor, otherwise it's not a function, so, there are no constant functions; those are merely values bound by variables.

A function must have exactly one input value and one output value, the types of which are both indicated in the signature, `function :: input -> output`.

Both of thse may be trivial values of trivial types, but they must be present. This form allows us to regard all functions as unary, thus curryied; e.g. a curryied binary function takes the first argument and returns another function that takes the second argument, computes and returns the result. Featuring auto-currying, Haskell especially blurs the distinctions between functions of different arities and those taking a tuple vs being curryied.

In any variant of a signature, the first type, i.e. the one on the LHS of (->), is always an input type; when all redundant parens (unnecessary due to the right associativity of function type ctor) are restored, it is evident that there is only one root (->) type ctor. And the type on its RHS is the return type. All signatures may be regarded as denoting a unary function (sometimes it is really unary and sometimes it is just curryied).
