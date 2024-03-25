# Functions

In Haskell, functions are actually lambda expressions. A lambda function may be bound to an identifier, as a way of abbreviation, but the binding is not needed, not even to define recursive functions (the fact known from λ-calculus). Moreover, all functions are actually unary. For example, a binary function takes the first argument and returns a function that takes the second argument and returns the result. Similarly with other polyadic functions. And because all functions are automatically curryied, this appears seamless.

Currying is a conversion performed on a function that takes a pair of arguments, `f (x, y)` - wht is in other PLs called a binary function - and returns a curryied function, `f x y`. Unlike the uncurryied version which requires both arguments at once - it actually requires one arg that is a 2-tuple, the curryied version can take the args either at once or one at the time. In the latter case, with only the first argument supplied, the function is said to be *partially applied*, and the partially applied function is returned (which then expects the second arg).

Declared at the top-level functions are usually given (most of the time redundant) type signature.

```hs
f :: a -> (b -> c) ≅
f :: a -> b -> c   ≅
f :: (a, b) -> c   ≅
f :: (b, a) -> c   ≅
f :: b -> a -> c
```
