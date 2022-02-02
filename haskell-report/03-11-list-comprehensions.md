# 3.11 List Comprehensions

https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11

```js bnf
aexp ::= [ exp | qual₁ , … , qualₙ ]        (list comprehension, n ≥ 1)

qual ::= pat <- exp                         (generator)
       | let decls                          (local declaration)
       | exp                                (boolean guard)
```

A list comprehension has the form `[ e | q1, …, qn ]` for n ≥ 1, where the `qᵢ` qualifiers are either:
* *generators* of the form `p <- e`, where `p` is a pattern of type `t` and `e` is an expression of type `[t]`
* *local bindings* that provide new definitions for use in the generated expression `e` or subsequent boolean guards and generators
* *boolean guards* which are arbitrary expressions

A list comprehension returns the list of elements produced by evaluating `e` in the successive environments created by the nested, depth-first evaluation of the generators in the qualifier list.

Binding of variables occurs according to the normal pattern matching rules, and if a match fails then that element of the list is simply skipped over.

```hs
[ x | xs <- [[(1,2),(3,4)], [(5,4), (3,2)]], (3,x) <- xs ] -- [4,2]
--                   ↑ ↑             ↑ ↑
               matched returned
```

If a qualifier is a boolean guard, it must evaluate to `True` for the previous pattern match to succeed.

```hs
[ 4 | True]  -- 4
[ 4 | False] -- []
```

As usual, bindings in list comprehensions can shadow those in outer scopes:

```hs
  3   2    1  3    2
  ↓   ↓    ↓  ↓    ↓
[ x | x <- x, x <- x ] ≡
[ z | y <- x, z <- y ]
```
