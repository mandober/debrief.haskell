# Duplicate record fields


`DuplicateRecordFields`
- Allow definition of record types with identically-named fields
- implies: `DisambiguateRecordFields`
- since: 8.0.1


Going beyond `DisambiguateRecordFields`, the `DuplicateRecordFields` extension allows multiple datatypes to be declared using the same field names in a single module. For example, it allows this:

```hs
module M where
  data S = MkS { x :: Int }
  data T = MkT { x :: Bool }
```

The use of fields that are always unambiguous, because they mention the constructor, including construction and pattern-matching, may freely use duplicated field names. For example, the following are permitted, just as with `DisambiguateRecordFields`:

```hs
s = MkS { x = 3 }
f (MkT { x = b }) = b
```

Field names used as selector functions or in record updates must be unambiguous, either because there is only one such field in scope, or because a type signature is supplied.

## Selector functions

Fields may be used as selector functions only if they are unambiguous, so this is still not allowed if both `S(x)` and `T(x)` are in scope:

```hs
bad r = x r
```

An ambiguous selector may be disambiguated by the type being "pushed down" to the occurrence of the selector (see higher-rank-type-inference for more details on what "pushed down" means). For example, the following are permitted:

```hs
ok1 = x :: S -> Int

ok2 :: S -> Int
ok2 = x

ok3 = k x -- assuming we already have k :: (S -> Int) -> _
```

In addition, the datatype that is meant may be given as a type signature on the argument to the selector:

```hs
ok4 s = x (s :: S)
```

However, we do not infer the type of the argument to determine the datatype, or have any way of deferring the choice to the constraint solver. Thus the following is ambiguous:

```hs
bad :: S -> Int
bad s = x s
```

Even though a field label is duplicated in its defining module, it may be possible to use the selector unambiguously elsewhere. For example, another module could import `S(x)` but not `T(x)`, and then use `x` unambiguously.

## Record updates

In a record update such as `e { x = 1 }`, if there are multiple `x` fields in scope, then the type of the context must fix which record datatype is intended, or a type annotation must be supplied. Consider the following definitions:

```hs
data S = MkS { foo :: Int }
data T = MkT { foo :: Int, bar :: Int }
data U = MkU { bar :: Int, baz :: Int }
```

Without `DuplicateRecordFields`, an update mentioning `foo` will always be ambiguous if all these definitions were in scope. When the extension is enabled, there are several options for disambiguating updates:

* Check for types that have all the fields being updated. For example:

```hs
f x = x { foo = 3, bar = 2 }
```

Here `f` must be updating `T` because neither `S` nor `U` have both fields.


* Use the type being pushed in to the record update, as in the following:

```hs
g1 :: T -> T
g1 x = x { foo = 3 }

g2 x = x { foo = 3 } :: T

g3 = k (x { foo = 3 }) -- assuming we already have k :: T -> _
```

* Use an explicit type signature on the record expression, as in: :

```hs
h x = (x :: T) { foo = 3 }
```

The type of the expression being updated will not be inferred, and no constraint-solving will be performed, so the following will be rejected as ambiguous:

```hs
let x :: T
    x = blah
in x { foo = 3 }

\x -> [x { foo = 3 },  blah :: T ]

\ (x :: T) -> x { foo = 3 }
```


## Import and export of record fields

When `DuplicateRecordFields` is enabled, *an ambiguous field must be exported as part of its datatype*, rather than at the top level. For example, the following is legal:

```hs
module M (S(x), T(..)) where
  data S = MkS { x :: Int }
  data T = MkT { x :: Bool }
```


However, this would not be permitted, because `x` is ambiguous: :

```hs
module M (x) where ...
```

Similar restrictions apply on import.
