# Functions in Haskell

Haskell has several ways to introduce functions. The main way is by declaring a named function, but anonymous lambdas may be introduced at the spot when needed. In fact, the "named" declaration of a function is just the binding of a lambda expression to a variable.

## Data constructors are also functions

Besides these, explicit, ways, there are more language construct where functions sneak in. In a data type declaration, data constructors are actually also functions, however they may be considered a special types of functions because their application is trivial. Unlike "regular" functions whose application to an argument results in a reduction, an application of a data ctor is never a reduction - they just "tag" their argument instead.

```hs
data Nat = Z | S Nat
-- S :: Nat -> Nat

data Dist = Mi Int | Km Int
-- Km :: Int -> Dist
```

Here, the data ctor `S` is a unary function. It expects one argument of the type `Nat`. Given the argument `Z`, the application of `S` to `Z` results in the expression `S Z`. There is no reduction, the data ctor `S` just tags the argument by prepending itself to it. Similarly in the case of `Dist` type. The data ctors `Mi` and `Km` only tag their integer argument, producing expressions like `Km 5` and `Mi 8`. No reduction is possible, nevertheless these are still function applications; e.g. the data ctor `Km` takes an integer and returns a (value of) `Dist` type, only the `Km` function doesn't compute the value but produces one by tagging. And since it does produce the value of the `Dist` type it is called a data constructor - it does construct a value of this type.
