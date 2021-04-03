# Evaluation strategy

https://en.wikipedia.org/wiki/Evaluation_strategy

Evaluation strategies
- Eager evaluation
- Strict evaluation
- Lazy evaluation
- Partial evaluation
- Remote evaluation
- Short-circuit evaluation

Procedure calls
- call-by-value
- call-by-reference
- call-by-name
- call-by-need
- call-by-sharing


The moving parts of a *function application*, i.e. *function call*, that is,applying a (previously defined) function to arguments, can make the whole ordeal incredibly complicated.

**Evaluation strategies** deal with many aspects of a function application:
- when to evaluate the arg expression (argument is an expression)
  - in advance: fully eval the args before executing a function's body
  - when needed: eval an arg just before it is actually used (in the body)
- what kind of construct to pass in
  - pass the fully evaluated argument expression (as value)
  - pass the argument unevaluated (as thunk)
  - pass in the partially evaluated arg, (as WHNF thunk)
- what kind of value to pass in
  - pass the arg in by copying its value (call-by-value)
  - pass in a reference to the value (call-by-reference)
  - pass in a pointer to the value (call-by-reference)
  - selective approach based on the type of arg
- argument caching
  - memoization (cache the arg once its evaluated)
  - evaluate the arg every time
- argument analysis
  - whether to analize args to avoid eval of duplicated arg expressions
  - what to do in case some args are duplicated: eval first/last or else
  - no analysis: eval each arg every time
  - deal with multiple (application) occurrences of the same arg (in the body)
- tail-call analysis
  - optimize the stack usage if the recursive function call is in tail position
  - translate non-tail recursive calls into iterative loops



In the straight up case of eager, call-by-value, evaluation strategy, each argument is always fully evaluated before it is passed into the function, regardless whether it's actually used or not. The value of the argument may be copied directly into the corresponding function parameter, or only its reference (e.g. the ability to look up the argument's current value, even the possibility to modify it, from within the function).

The notion of *reduction strategy* in lambda calculus is similar but distinct.

Many (OO) PLs have converged on the call-by-value and call-by-reference evaluation strategy for function calls. Commonly, values whose size is up to the word's (the size of a pointer), such as primitive values, are passed by copying, while bigger objects are passed by reference. With slight variations, this is a very common strategy in impure PLs because they don't know any better.

Purely FPLs like Haskell, that enjoy referential transparency, use call-by-need evaluation in which an expression can be in several stages of evaluation (e.g. WHNF), with it being fully evaluated to NF only when really needed, e.g., just before it is printed out. These actions that trigger such reduction are the ones that drive the evaluation - without them nothing would be evaluated; this is, the so-called, *lazy evaluation*.

Haskell uses *call-by-name* with *call-by-sharing* evaluation strategy, which means a function's argument is only evaluated when actually needed (used) and only once, after which it is memoized.

Evaluation strategy is not a function of any specific implementation but it is fixed by a PL's specification. However, Haskell offers to programmers the ability to force evaluation: an expression prefixed by `!` forces it to WHNF, while `!!` forces the full evaluation.



## Refs

Call-by-value, call-by-name and call-by-need
http://homepages.inf.ed.ac.uk/stg/NOTES/node72.html
