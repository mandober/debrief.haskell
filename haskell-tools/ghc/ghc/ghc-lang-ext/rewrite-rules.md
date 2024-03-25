# Rewrite rules

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/rewrite_rules.html#rewrite-rules

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/pragmas.html#rules-pragma

Rewrite rules
https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/rewrite_rules.html#rewrite-rules


```hs
{-# RULES "⟨name⟩" forall ⟨binder⟩ ... . ⟨expr⟩ = ⟨expr⟩ ...
  #-}
```

Rewrite rules can be placed at the top-level only. They define rules used to optimize a source code functions. Here is an example:

```hs
{-# RULES
    "map/map"        forall f  g xs. map f (map g xs) = map (f . g) xs
    "map/append" [2] forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys
  #-}
```

The debug flags:
* `-ddump-simpl-stats`   shows which rules have fired
* `-ddump-rule-firings`  shows each individual rule firing
* `-ddump-rule-rewrites` shows the 

## Syntax

From a syntactic point of view:
- There may be zero or more rules in a `RULES` pragma
- The layout rule applies in a pragma
- Each rule has a name, enclosed in double quotes, e.g. "map/append". The name itself has no significance, it's only used for reporting.
- A rule may optionally have a phase-control number after the name
- The phase number means the rule is active in indicated and subsequent phases
- The inverse notation, e.g. `[~2]`, means the rule is active up to, but not including the indicated phase
- phase-control `[~]` means the rule is never active (used by GHC plugins)
- Each mentioned var must be in scope or bound by the `forall`
- vars bound by the `forall` are called the *pattern variables*
- A pattern variable may optionally have a type signature
- If the type of the pattern variable is polymorphic, patvar must have a sig
- If `ExplicitForAll` is enabled, type/kind variables can be explicitly bound:

```hs
{-# RULES "id" forall a. forall (x :: a). id @a x = x #-}
```

- When a type-level explicit forall is present, each type/kind variable mentioned must be in scope or bound by the forall. In particular, unlike some other places, this means free kind vars will not be implicitly bound:

```hs
"bad" forall   (c :: k). forall (x :: Proxy c) ...
"ok"  forall k (c :: k). forall (x :: Proxy c) ...
```

- When bound type/kind variables are needed, both foralls must always be included, though if no pattern variables are needed, the second can be left out:

```hs
{-# RULES "map/id" forall a. forall. map (id @a) = id @[a] #-}
```

- The LHS of a rule must contain a top-level var applied to expressions:

```hs
-- LHS is not an application
"wrong 1"   forall e1 e2.  case True of { True -> e1; False -> e2 } = e1
-- LHS has a pattern variable in the head
"wrong 2"   forall f.      f True = True
-- LHS has a ctor, rather than a var, applied to an arg
"wrong 3"   forall x.      Just x = Nothing
```

- A rule does not need to be in the same module as (any of) the variables it mentions, though they need to be in scope.

- All rules are implicitly exported from the module, and are therefore in force in any module that imports the module that defined the rule, directly or indirectly. That is, if `A` imports `B`, which imports `C`, then `C`'s rules are in force when compiling `A`. This is very similar to instance declarations.

- Inside `RULES` forall is treated as a keyword, regardless of any other flag settings. Furthermore, the language extension `ScopedTypeVariables` is enabled.

- Like other pragmas, `RULES` are always typechecked and checked for scope errors. Typechecking means that LHS and RHS of a rule are typechecked, and must have the same type.


## Semantics

* Rules are regarded as left-to-right rewrite rules. When GHC finds an expression that is a substitution instance of the LHS of a rule, it replaces the expression by the appropriately substituted RHS. By a "substitution instance" we mean that the LHS can be made equal to the expression by substituting for the pattern variables.
