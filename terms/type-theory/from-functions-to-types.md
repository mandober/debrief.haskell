# From Functions to Types and vice versa


A function `const` is `λab.a` in LC, and if partially applied, it can hold onto a single value. However, since functions may be difficult to handle, at least as type classes are considered, we can "reify" a function by making a newtype to represent it, then we can make that type a class instance.

In general, data type declaration that define a new type, whether with `data` or `newtype` keywords, are nothing but a pack of related functions, usually one or more constructor functions that introduce the type, plus some functions to eliminate it (possibly in the form of accessor functions).

The elimination functions play a similar role to pattern matching. PM matches a type by its data ctors (essentially by a tag). Packing a set of functions to repr a type cannot use PM, it needs a different method. The Boolean type repr with functions has the standard Church encoding that consists of the introduction functions `true` and `false` that work as selectors: `true` is the `const` function (so it always selects the first arg, ignoring the second), and `false` is `flip cons` (so it ignores the first arg, returning the second; so, when partially applied, it collapses into the `id` function). The eliminator for Booleans is `if` function. Similarly, a pair is partially applied `λabf.fab` lambda, and it has 2 eliminators, `fst` and `snd`.

When turned into a type, each function's parameter becomes a type parameter. If a function ignores a param, or has a difficult-to-determine return type (i.e. the returned type is determined later by continuations), existential types will be needed.

The newly made data type will usually have a single function, wrapped in a data ctor, and that function will pack all the needed components i.e. elimination functions.

For example, turning a list into a type (ignoring the existing one) must pack the `nil` function and the `cons` function.


## Product types

- Product type: Pair, And, Conjunction
- `a × b`
- 1 binary ctor, `pair`

```hs
newtype And a b = And { unAnd :: forall r. 
       (a -> b -> r) -- pair ctor (binary)
    -> r             -- (return)
    }
```

## Sum types

- Sum type: Either, Or, Disjunction
- `a + b`
- 2 unary ctors, `left` and `right`

```hs
newtype Or a b = Or { unOr :: forall r.
       (a -> r)   -- left ctor
    -> (b -> r)   -- right ctor
    -> r          -- (return)
    }
```

## Maybe

- `1 + a`
- 2 ctors: nullary for `nothing`, unary for `just`

```hs
newtype FMaybe a = FMaybe { unFMaybe :: forall r. 
       r            -- nothing ctor (nullary)
    -> (a -> r)     -- just ctor (unary, needs extra arg)
    -> r
    }
```

## List

- `1 + a × List a`
- 2 ctors: nullary for `nil`, binary for `cons`

```hs
-- data type
newtype FList a = FList { unFList :: forall r. 
       r                    -- nil ctor (nullary)
    -> (a -> FList a -> r)  -- cons ctor (needs 2 extra arg: el and a list)
    -> r                    -- (return)
    }

--   List a = 1   +      a × List a
data List a = Nil | Cons (a, List a) ≅
data List a = Nil | Cons a (List a)
```



## Const

https://blog.jle.im/entry/const-applicative-and-monoids.html

The `Applicative` typeclass has a somewhat infamous reputation for having opaque laws. There are a lot of great alternative rephrasing of these laws, from many different approaches. Ad hoc, we talk about Applicative in terms of one of `Const`.

The Const data type from the std is relatively simple as far as newtypes go, however, let's consider a less polymorphic version, `IntConst`, which is essentially `Const Int`.

```hs
newtype Const a b = Const { unConst :: a }

newtype IntConst a = IntConst { getIntConst :: Int }
```

For `IntConst a`, the `a` is a *phantom type parameter*. This means that there are not necessarily any values of type `a` in the value of type `IntConst a`.

In modern GHC with *PolyKinds* and *ConstraintKinds*, this means that `a` might not even be a type that can have values; you might have, e.g. a value of type `IntConst Maybe` (:set -XPolyKinds), or `IntConst Monad` (:set -XConstraintKinds), and GHC would be perfectly happy.
