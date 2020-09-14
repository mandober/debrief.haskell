# Data.Bool

* data `Bool`
  - module: `Data.Bool`
  - type name: `Bool`
  - `Data.Bool.bool :: a -> a -> Bool -> a`
* data declaration: `data Bool = False | True`
  - type ctor : `Bool`
  - data ctors: `False`, `True`
* operators:
  - (||) :: Bool -> Bool -> Bool   *infixr 2*
  - (&&) :: Bool -> Bool -> Bool   *infixr 3*
* methods:
  - not :: Bool -> Bool
* members:
  - otherwise :: Bool


`otherwise` is defined as `True`; it is used to makes *guards* more readable as a catch-all, i.e. catch-remaining-cases, pattern:

```
f x | x < 0     = ...
    | otherwise = ...
```
