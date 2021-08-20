# Kind system

More-less, what types are to terms, kinds are to types.

Unknown terms (expression) are represented by a variable, like `x`. The `x` must have some type, even though that type may be unknown at the moment. It may be a base type like `Int`, or a constrained concrete type like `Num a => a`, `Maybe a` or `[a]`; or even a unconstrained type like `forall a. a`. Unknown types are repr by a type variable like `a`, which may have plenty of additional properties, most of which in the form of constraints.

But things can get very complicated: a type variable `a` can stand for any type, but `[a]` is necessarily a list of some type; similarly `Maybe b` is a maybe containing some type. The `[]` and `Maybe` thenselves are type ctors - they are neither term-level values, nor complete (saturated) types, and what's more, they can also be repr by a type variable, usually denoted using later letters in the alphabet like `t`, `f`, `m`. So, a compound type-level expression is conceivable as `t a` and it may repr `Maybe Int` or `[] Float`, or may others combinations of a unary type ctor and a concrete type, but it cannot eve be `Maybe []`, or `[] Maybe` i.e. `[Maybe]`. Eventually, it will have to be saturated, so a valid type like `[] Maybe Int` (that is `[Maybe Int]`) could be repr by type variables as, e.g. `x y z`.

So could a type `[] Maybe [] Int` be repr by `w x y z`? No! Type application is right-associative so we must add parens to form a valid type, 
`[] (Maybe ([] Int))` and thus, `w (x (y z))`. 

```hs
[] Maybe [] Int     -- invalid
[] Maybe ([] Int)   -- invalid
[] (Maybe ([] Int)) -- valid

-- these are all the same:
-- (since brackets subsume the role of parens as delimiters)

[] (Maybe ([] Int)) -- same as
[] (Maybe ([Int]))  -- same as
[] (Maybe [Int])    -- same as
[(Maybe [Int])]     -- same as
[Maybe [Int]]       -- most common form
```


Levels
- *Terms-level values*: `42`, or better yet, since it's a polymorphic literal, `42 :: Int`, i.e. a term (member) of the `Int` type (set). So `x :: Int` can be interpreted as forall values (terms) `x` such that `∀x. x ∈ Int`. But we may also have `42 :: Integer`. This mean we can keep our functions that work on `Int`s, but if we want then to also work on `Integer`, we'd have to duplicate the effort by also writing their versions that work with `Integer` (very much like in `C`).
- *Type-level values*. Polymorphism allows us to solve this more practically. Parametric polymorphism is unrestricted and works for any and all types. On the other hand, ad hoc polymorphism allow us to introduce constraints that shrink the set of types to just those chosen by us.

```hs
i1 :: Int
i1 = 42

i2 :: Integer
i2 = 42

-- ad hoc polymorphism
i3 :: Integral a => a
i3 = 42

i4 :: Floating a => a
i4 = 42

i5 :: Num a => a
i4 = 42

-- Parametric polymorphism
id :: a -> a
id a = a
```
