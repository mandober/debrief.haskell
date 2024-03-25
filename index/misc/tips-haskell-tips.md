# Haskell :: Tips

* `map` can quickly produce a list of partially applied functions when given a function of arity > 1, instead of a unary function it expects.

* `Control.DeepSeq.deepseq` differs from `seq` as it traverses data structures deeply, for example, `seq` will evaluate only to the first constructor in the list: `seq [1,2,undefined] 3` will return 3. The `deepseq` will force evaluation of all the list elements: `deepseq [1,2,undefined] 3` will throw. The `Control.DeepSeq.force` variant of `deepseq` is useful in some situations: `force x = deepseq x x`. That is, `force x` fully evaluates `x`, and then returns it. Note that `force x` only performs evaluation when the value of `force x` itself is demanded, so essentially it turns shallow evaluation into deep evaluation.
