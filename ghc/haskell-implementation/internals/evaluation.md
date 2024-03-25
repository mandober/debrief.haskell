# Evaluation

Haskell uses call-by-need evaluation strategy in the non-strict (lazy) setting. Values( like args to a function) are not evaluated until actually needed, and once evaluated the result is cached for future reuse.

* In pattern matching (tipically, on the LHS of equation) the args are evaluated to their WHNF, that is, just until the constructor is revealed (which is crucial in order to select the appropriate RHS in a multi-piece equation). With literal patterns, the args are surely evaluated all the way.

* [Not entirely sure] what happens in a function that has two identical expressions vs moving these two expressions under a `where` clause? Does GHC recognize that the two separate expressions are the same (probably not, since it doesn't do equality of functions) or must we explicitly let it know by extracting them in the `where` clasue?

* `seq` function evaulates its first argument to WHNF before returning the second, `seq :: a -> b -> b` (seq :: Data a => a -> b -> b old version?)

* Non-determinism can express the so-called *parallel-or* function, which has the property that `or True ⊥` == `or True ⊥` = `True`

```hs
-- normal OR
(||) True undefined -- True
(||) undefined True -- EXCEPTION!

-- parallel OR
parallelOr True undefined -- True
parallelOr undefined True -- True
```
