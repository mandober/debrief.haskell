# Records

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/records.html


## TOC

- 6.5. Records
  - 6.5.1. Record field name resolution
  - 6.5.2. Traditional record syntax
  - 6.5.3. Field selectors and TypeApplications
  - 6.5.4. Record field disambiguation
  - 6.5.5. Duplicate record fields
  - 6.5.6. Field selectors
  - 6.5.7. Record puns
  - 6.5.8. Record wildcards
  - 6.5.9. Record field selector polymorphism
  - 6.5.10. Overloaded record dot
  - 6.5.11. Overloaded record update

## 6.5.1. Record field name resolution

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_field_resolution.html

A record field name `x` can be used in these 4 contexts:
- in a *record construction*: `C{ x = 3 }`
- in a *record update*:       `r{ x = 4 }`
- in a *record pattern*:      `case r of C{ x = value } -> …`
- as a *record selector*:     `x r`

In these 4 cases, here are the rules for field name resolution:
- unqualified name `x` is unambiguous iff there's one unqualified `x` in scope
- qualified name `M.x` is unambiguous iff there is just one `M.x` in scope

Those rules are amended by the following extensions:
- DisambiguateRecordFields
- DuplicateRecordFields
- NoFieldSelectors


### `DisambiguateRecordFields`

* In record construction and pattern matching, `C{ x = …}`, an *unqualified* field name `x` is unambiguous iff the data constructor `C` has a field `x`, and that field is in scope unqualified, or qualified as `Q.x`, regardless of `Q`.
* Similarly, in record construction and pattern matching, a *qualified* field name `M.x` is unambiguous iff the data constructor `C` has a field `x`, and that field is in scope qualified as `M.x`.

* In record updates, `r{ x = 3 }`, the field name `x` is unambiguous iff there is just one field name `x` in scope *unqualified*. Non-field names are ignored.
* Similarly, the record update with a *qualified* field `r{ M.x = 3 }` is unambiguous if just one field name is in scope as `M.x`. Non-field names are ignored.


### `DuplicateRecordFields`

This extension allows record updates if exactly one type has all the fields being updated, even if they are individually ambiguous according to the two rules for field name resolution above.

```hs
{-# LANGUAGE DuplicateRecordFields #-}

data S = MkS1 { x :: Int, y :: Bool }
       | MkS2 { x :: Int }

data T = MkT1 { x :: Int, z :: Bool }
       | MkT2 { z :: Bool }

f r = r{ x=3, y=True }
```

The only data type that has both `x` and `y` as fields is `S`, so the field names `x` and `y` refer unambiguously to data type `S`.


### `NoFieldSelectors`

This extension being enabled means that field selector names in scope will be ignored in an expression context.

```hs
{-# LANGUAGE NoFieldSelectors #-}

data T = MkT { x :: Int }

x = "Johnny5"

lo = lines x
```

With `NoFieldSelectors` the `x` in `lo`'s rhs refers to the `x :: String`, not to the field `x`.


## 6.5.2. Traditional record syntax

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/traditional_record_syntax.html

- `NoTraditionalRecordSyntax`
- Since: 7.4.1
- Disallow use of record syntax.

Traditional record syntax, such as `C {f = x}`, is enabled by default. To disable it, you can use the `NoTraditionalRecordSyntax` extension.

Under `NoTraditionalRecordSyntax`, it is not permitted to define a record datatype or use record syntax in an expression. For example, the following all require `TraditionalRecordSyntax`:

```hs
data T = MkT { foo :: Int }  -- record datatype definition

x = MkT { foo = 3 }          -- construction

y = x { foo = 3 }            -- update

f (MkT { foo = i }) = i      -- pattern matching
```

However, if a field selector function is in scope, it may be used normally. (This arises if a module using `NoTraditionalRecordSyntax` imports a module that defined a record with `TraditionalRecordSyntax` enabled). If you wish to suppress field selector functions, use the `NoFieldSelectors` extension.

## 6.5.3. Field selectors and TypeApplications

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors_and_type_applications.html

Field selectors can be used in conjunction with `TypeApplications`, as described in Visible type application. The type of a field selector is constructed by using the surrounding definition as context. This section provides a specification for how this construction works. We will explain it by considering 3 different forms of field selector, each of which is a minor variation of the same general theme.


### 6.5.3.1. Field selectors for Haskell98-style data constructors

Consider the following example:

```hs
data T a b = MkT { unT :: forall e. Either e a }
```

This data type uses a Haskell98-style declaration. The only part of this data type that is not Haskell98 code is `unT`, whose type uses higher-rank polymorphism (arbitrary-rank polymorphism). To construct the type of the `unT` field selector, we will assemble the following:

1. The type variables quantified by the *data type head*, (forall a b. <…>)

2. The return type of the data ctor (<…> T a b -> <…>). By virtue of this being a Haskell98-style declaration, the order of type variables in the return type will always coincide with the order in which they are quantified

3. The type of the field (<…> forall e. Either e a)

The final type of `unT` is therefore    
`forall a b. T a b -> forall e. Either e a`

As a result, one way to use `unT` with TypeApplications is:   
`unT @Int @Bool (MkT (Right 1)) @Char`



### 6.5.3.2. Field selectors for GADT constructors

Field selectors for GADT constructors (declaring data types with explicit constructor signatures) are slightly more involved. Consider the following example:

```hs
data G a b where
  MkG :: forall x n a. (Eq a, Show n) =>
    { unG1 :: forall e. Either e (a, x)
    , unG2 :: n
    }
    -> G a (Maybe x)
```

The `MkG` GADT ctor has two records, `unG1` and `unG2`.

However, only `unG1` can be used as a top-level field selector; `unG2` cannot because it is a "hidden" selector (see Record Constructors); its type mentions a free variable `n` that doesn't appear in the result type `G a (Maybe x)`.

On the other hand, the only free type variables in the type of `unG1` are `a` and `x`, so `unG1` is fine to use as a top-level function.

To construct the type of the `unG1` field selector, we will assemble the following:

1. The subset of type variables quantified by the GADT constructor that are mentioned in the return type. Note that the order of these variables follows the same principles as in Ordering of specified variables. If the constructor explicitly quantifies its type variables at the beginning of the type, then the field selector type will quantify them in the same order (modulo any variables that are dropped due to not being mentioned in the return type). If the constructor implicitly quantifies its type variables, then the field selector type will quantify them in the left-to-right order that they appear in the field itself.

In this example, MkG explicitly quantifies forall x n a., and of those type variables, a and x are mentioned in the return type. Therefore, the type of unG1 starts as forall x a. <…>. If MkG had not used an explicit forall, then they would have instead been ordered as forall a x. <…>, since a appears to the left of x in the field type.

2. The GADT return type (<…> G a (Maybe x) -> …)

3. The type of the field (<…> -> forall e. Either e (a, x))


The final type of unG1 is therefore    
`forall x a. G a (Maybe x) -> forall e. Either e (a, x)`

As a result, one way to use unG1 with TypeApplications is   
`unG1 @Int @Bool (MkG (Right (True, 42)) ()) @Char`


### 6.5.3.3. Field selectors for pattern synonyms

Certain record pattern synonyms (Record Pattern Synonyms) can give rise to top-level field selectors. Consider the following example:

```hs
pattern P :: forall a. Read a
          => forall n. (Eq a, Show n)
          => (forall e. Either e (a, Bool)) -> n -> G a (Maybe Bool)
pattern P {unP1, unP2} = MkG unP1 unP2
```

We can only make field selectors for pattern synonym records that do not mention any existential type variables whatsoever in their types, per Record Pattern Synonyms. (This is a stronger requirement than for GADT records, whose types can mention existential type variables provided that they are also mentioned in the return type.) We can see that unP2 cannot be used as a top-level field selector since its type has a free type variable n, which is existential. unP1 is fine, on the other hand, as its type only has one free variable, the universal type variable a.

To construct the type of the unP1 field selector, we will assemble the following:

1. The universal type variables (forall a. <…>).
2. The required constraints (<…> Read a => <…>).
3. The pattern synonym return type (<…> G a (Maybe Bool) -> <…>).
4. The type of the field (<…> -> forall e. Either e (a, Bool)).

The final type of unP1 is therefore   
`forall a. Read a => G a (Maybe Bool) -> forall e. Either e (a, Bool)`

As a result, one way to use unP1 with TypeApplications is    
`unP1 @Double (MkG (Right (4.5, True)) ()) @Char`

## 6.5.4. Record field disambiguation

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/disambiguate_record_fields.html

```hs
{-# LANGUAGE DisambiguateRecordFields #-}
```


## 6.5.5. Duplicate record fields

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html


```hs
{-# LANGUAGE DuplicateRecordFields #-}
```

### 6.5.5.1. Import and export of record fields


## 6.5.6. Field selectors

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html

```hs
{-# LANGUAGE  FieldSelectors #-}
{-# LANGUAGE NoFieldSelectors #-}
```

### 6.5.6.1. Import and export of selector functions



## 6.5.7. Record puns

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html

`NamedFieldPuns`

```hs
{-# LANGUAGE NamedFieldPuns #-}
```

## 6.5.8. Record wildcards

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html

```hs
{-# LANGUAGE RecordWildCards #-}
```

Allow the use of wildcards in record construction and pattern matching.

This extension implies `DisambiguateRecordFields`.

## 6.5.9. Record field selector polymorphism

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html

The module GHC.Records defines the following:

```hs
class HasField (x :: k) r a | x r -> a where
  getField :: r -> a
```

A `HasField x r a` constraint represents the fact that `x` is a field of type `a` belonging to a record type `r`. The `getField` method gets the record's selector function.

This allows definitions that are polymorphic over record types with a specified field. For example, the following works with any record type that has a field `name :: String`

```hs
foo :: HasField "name" r String => r -> String
foo r = reverse (getField @"name" r)
```

`HasField` is a *magic built-in typeclass* (similar to `Coercible`, for example). It is given special treatment by *the constraint solver* (see Solving `HasField` constraints). Users may define their own instances of `HasField` also (see Virtual record fields).

### 6.5.9.1. Solving HasField constraints

### 6.5.9.2. Virtual record fields

Users may define their own instances of HasField, provided they do not conflict with the built-in constraint solving behaviour. *This allows "virtual" record fields to be defined for datatypes that do not otherwise have them*.

For example, this instance would make the name field of `Person` accessible using `#fullname` as well:

```hs
instance HasField "fullname" Person String where
  getField = name
```

More substantially, an anonymous records library could provide `HasField` instances for its anonymous records, and thus be compatible with the polymorphic record selectors introduced by this proposal.

For example, something like this makes it possible to use `getField` to access Record values with the appropriate string in the type-level list of fields:

```hs
data Record (xs :: [(k, Type)]) where
  Nil  :: Record '[]
  Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

instance HasField x (Record ('(x, a) ': xs)) a where
  getField (Cons _ v _) = v
instance HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
  getField (Cons _ _ r) = getField @x r

r :: Record '[ '("name", String) ]
r = Cons Proxy "R" Nil

x = getField @"name" r
```

Since representations such as this can support field labels with kinds other than `Symbol`, the `HasField` class is *poly-kinded* (even though the built-in constraint solving works only at kind `Symbol`). In particular, this allows users to declare scoped field labels such as in the following example:

```hs
data PersonFields = Name

s :: Record '[ '(Name, String) ]
s = Cons Proxy "S" Nil

y = getField @Name s
```

In order to avoid conflicting with the built-in constraint solving, the following user-defined HasField instances are prohibited (in addition to the usual rules, such as the prohibition on type families appearing in instance heads):
- HasField _ r _ where r is a variable;
- HasField _ (T ...) _ if T is a data family (because it might have fields introduced later, using data instance declarations);
- HasField x (T ...) _ if x is a variable and T has any fields at all (but this instance is permitted if T has no fields);
- HasField "foo" (T ...) _ if T has a field foo (but this instance is permitted if it does not).

If a field has a higher-rank or existential type, the corresponding HasField constraint will not be solved automatically (as described above), but in the interests of simplicity we do not permit users to define their own instances either.

If a field is not in scope, the corresponding instance is still prohibited, to avoid conflicts in downstream modules.


## 6.5.10. Overloaded record dot

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html

`OverloadedRecordDot`


- Since: 9.2.0
- Provides record '.' syntax e.g. `x.foo`

When `OverloadedRecordDot` is enabled one can write `a.b` to mean the `b` field of the a record expression.

```hs
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

data Person = Person { name :: String }
data Company = Company { name :: String, owner :: Person }

main = do
  let c = Company { name = "Acme Corp."
                  , owner = Person { name = "Wile E. Coyote" } }
  print $ c.name ++ " is run by " ++ c.owner.name
```


You may also write `(.b)` to mean a function that "projects the `b` field from its argument". For example, `(.b) a` means the same thing as `a.b`.

`OverloadedRecordDot` is normally implemented by desugaring record `.` expressions to `GHC.Records.getField` expressions.

By enabling `OverloadedRecordDot` and `RebindableSyntax` together it is possible to desugar `.` expressions into your own `getField` implementations.

When considering `a.b`, the `b` field that is meant is determined by solving `HasField` constraints (See Solving `HasField` constraints).

## 6.5.11. Overloaded record update

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_update.html


- OverloadedRecordUpdate
- Since: 9.2.0
- Provides record '.' syntax in record updates e.g. `x{foo.bar = 1}`

[EXPERIMENTAL] This design of this extension may well change in the future. It would be inadvisable to start using this extension for long-lived libraries just yet.

It's usual (but not required) that this extension be used in conjunction with Overloaded record dot.


```hs
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate, RebindableSyntax #-}

import Prelude

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a . HasField x r a => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.
setField :: forall x r a . HasField x r a => r -> a -> r
setField = fst . hasField @x -- Note : a{x = b} is setField @"x" a b.

data Person = Person { name :: String } deriving Show
instance HasField "name" Person String where
    hasField r = (\x -> case r of Person { .. } -> Person { name = x, .. }, name r)

data Company = Company { company :: String, owner :: Person } deriving Show
instance HasField "company" Company String where
    hasField r = (\x -> case r of Company { .. } -> Company { company = x, .. }, company r)
instance HasField "owner" Company Person where
    hasField r = (\x -> case r of Company { .. } -> Company { owner = x, .. }, owner r)

main = do
  let c = Company {company = "Acme Corp.", owner = Person { name = "Wile E. Coyote" }}

  -- Top-level update
  print $ c{company = "Acme United"} -- Company {company = "Acme United", owner = Person {name = "Wile E. Coyote"}}

  -- Nested update
  print $ c{owner.name = "Walter C. Johnsen"} -- Company {company = "Acme Corp.", owner = Person {name = "Walter C. Johnsen"}}

  -- Punned update
  let name = "Walter C. Johnsen"
  print $ c{owner.name}  -- Company {company = "Acme Corp.", owner = Person {name = "Walter C. Johnsen"}}
```


`OverloadedRecordUpdate` works by desugaring record `.` update expressions to expressions involving the functions `setField` and `getField`. Note that all record updates will be desugared to `setField` expressions whether they use `.` notation or not.

At this time, `RebindableSyntax` must be enabled when `OverloadedRecordUpdate` is and users are required to provide definitions for `getField` and `setField`. We anticipate this restriction to be lifted in a future release of GHC with builtin support for `setField`.
