# Haskell :: Haskell implementation

Things to coinsider for Haskell implementation:

- Implementation of functional languages
  - Specific functional features
    - first-class functions, functions-as-values
    - higher-order functions
    - anonymous functions
    - lambdas
    - nested functions, nested function definitions
    - function scope, variable shadowing
    - closures
    - auto-currying of function application
    - partial function application
  - Implementation of lazy functional languages
    - laziness
    - non-strict evaluation
    - call-by-need
  - Implementation of pure (lazy) functional languages [^¹]
  - Implementation of non-strict higher-order functional languages
    - STG machine
- Haskell implementation
- Haskell compilation


An implementation of Haskell has to deal with the things common to all functional programming languages, which primarily means the support for *first-class functions* (functions as first-class citizens). This approach treats functions as ordinary values, which implies certain features expected from such a language.

First of all, it means that functions can be passed into and can be returned from other functions; functions that expect function arguments (funargs) are called *higher-order functions*.

Secondly, this further implies that functions are actually *closures* - they have the ability to close over an environemnt, that is, to capture variables from the enclosing scopes. Returning a closure means it will maintain the bindings to the captured variables long after the scope in which it originated was destroyed.

Additional features, probably subsumed by these are the ability to *nest functions* (to define functions within functions), which further implies that each function creates *own scope*, from which it has access to the names of all the *enclosing scopes* (possibly capturing them).

Before, this has brought about the issue of how to deal with the function scope - whether to support dynamic or *lexical scoping* - but today almost all FPLs have settled on the second choice (Lisp had once supported dynamic scoping, bash still does, but Scheme has shown that lexical scoping is more inlined with the expectations of programmers). Another issue that is now settled is the problem of *variable shadowing* - when a nested function refers to some name, which happens to exists in more than one enclosing scope, then which of those names is actually referenced; today, it is almost always the name from the nearest enclosing scope, going outwards. Also the support for *anonymous functions* that are created on the spot (e.g. to be passed as a funarg in a function call); such functions are usually called *lambdas*.

Once, the big issue was the handling of funargs in terms of the stack, but this was resolved by using pointers to the heap objects that represent closures.

The support for *auto-currying of function application* and *partial function application* are features also expected from a modern FPL, which require further consideration about how to handle them in terms of their representation in memory.

The feature listed above are common to all FPLs, and now such languages fork into further categories depending on their take on: purity and mutability, laziness, non-strict evaluation strategy, call-by-{value, referenc, need, …} strategies, etc.


[^¹]: It seems impossible to implement a pure functional languages without it being lazy.
