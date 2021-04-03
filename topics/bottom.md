# Bottom

- bottom, formally denoted by `⊥`, represents the diverging computation
- bottom can be denoted by the value `undefined :: a` (reserved keyword)
- bottom is a consequence of the negative answer to the halting problem
- bottom repr the most undefined type, a diverging computation, infinite loop, the result of `error` function. In fact, there is no way to distinguish between these different circumstances that bottom out: all bottoms are the same.


```hs
-- Prelude.undefined
bottom = undefined
undefined = error "Prelude.undefined"

-- error
bottom = error "Non-terminating computation!"

-- in Gofer, bottom is defines as
bottom = undefined | False

-- bottom's type's arbitrary, defaulting to the most general type
undefined :: a
```


## Type inhabitation

The bottom is a member of each and every Haskell type, past, present or future, builtin or custom types. Even the trivial unit type has bottom, and so does any other nullary type.

The only exceptions are the unlifted types, whose type ctors are marked with a hash symbol (magic hash). Those are the unlifted, mostly unboxed types, e.g. `#Int` is the proper, machine integer, while `Int` is a lifted Haskell type that wraps it. 

The pair `(⊥, ⊥)` is different then `⊥` because the `test` fn below is defined by pattern matching, so `test ⊥ == ⊥`. On the other hand, `test (⊥,⊥) == True` since in the evaluation of `test p`, pattern matching is required to reduce `p` to the form `(x,y)`, but it is not required to determine the values of x and y. Since `⊥ ≠ True`, it follows that `(⊥,⊥) * ⊥`.

```hs
test :: (a, b) -> Bool
test (x,y) = True
```

More generally, if `a` is instantiated to a type with `m` ctors and `b` to a type with `n` ctors, then `(a,b)` will have `1 + (m + 1)(n + 1)` values total; i.e. `mn + m + n + 1`, instead of `mn` values if the bottoms are ignored.


Since bottom is an inhabitant of every type, and thus is a value of any type, it can be used wherever a value is expected, which may be useful in case the type is uninhabited or if any value of the type will do.

```hs
-- dispatching to a class instance: no need to give a specific value
print (sizeOf (undefined :: Int))

-- probing laziness: undefined shouldn't be evaluated
print $ head $ 1 : undefined
```

Some languages with dependent type systems, such as Epigram, can statically enforce termination, based on the type for particular programs, such as those using induction.


## Definedness ordering

Haskell has a **definedness relation** between types, that has a specific ordering. Definedness of types of expressions can be ordered with `⊥` as the least element, e.g. `⊥` ⊏ `f (f ⊥)` ⊏ `f (...(f ⊥)...)` or `⊥ ⊏ [⊥] ⊏ [3, ⊥]`

In the definedness ordering the things that are more-defined-then carry more information; e.g. the list [3,⊥] is more defined then the list [⊥] because we know more about the former then the latter. Fully defined terms are equal w/r to their definedness ordering. This means, e.g. `3 ⋢ 4 ∧ 4 ⋢ 3`, i.e. 3 and 4 are equally defined under the definedness relation.
