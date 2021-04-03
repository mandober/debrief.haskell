# MonoLocalBinds

Lang Pragma: `MonoLocalBinds`

https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/let_generalisation.html

6.12.2. Let-generalisation
MonoLocalBinds
Since: 6.12.1

> Infer less polymorphic types for local bindings by default.

An ML-style language usually generalises the type of any `let`-bound or `where`-bound variable, so that it is as polymorphic as possible. With the extension *MonoLocalBinds* GHC implements a slightly more conservative policy, using the following rules:

A variable is **closed** iff
- the variable is let-bound
- one of the following holds (OR)
  - var has an explicit type signature that has no free type variables
  - its binding group is *fully generalised*
- A binding group is fully generalised iff (AND)
  - each of its free variables is either imported or closed, and
  - the binding is not affected by the monomorphism restriction
