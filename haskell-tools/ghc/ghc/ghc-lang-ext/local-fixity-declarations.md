# Local Fixity Declarations

A careful reading of the Haskell 98 Report reveals that fixity declarations are permitted to appear inside local bindings such those introduced by `let` and `where`. However, the Haskell Report does not specify the semantics of such bindings very precisely.

A fixity declaration is most often stated *at the top-level*.

In GHC, a fixity declaration may accompany a local binding, and the fixity declaration applies wherever the binding is in scope. For example, in a *let-expression*, it applies in the RHS of other let-bindings and the body of the let.

```hs
let f = ...
    infixr 3 `f`
in  (...)
```

In a recursive *do-expression*, the local fixity declarations of a `let` statement scope over other statements in the group, just as the bound name does.

```hs
expr = do
  rec x <- compute mx
      y <- compute my
      let z = x + y
          infixr 3 `z`
```

Moreover, a local fixity declaration **must** be accompanyied by a local binding of that name; it's not possible to revise the fixity of name bound elsewhere.

```hs
-- DISALLOWED:
gxx = let infixr 9 +
      in 3 + 4

-- ok:
fxx = let infixr 9 ↑
          (↑) = (+)
      in 3 ↑ 4
```

Because local fixity declarations are technically Haskell 98, no extension is necessary to enable them.

A fixity decl can also appear in a *class declaration*, where it is associated to a particular method; however, since method declaration are implicitly top-level, so is the fixity declaration. Therefore, it is usually defined at the top-level part, outside a class declaration.

```hs
class Eq a where
  (+) = ...
  infixr 5 +
```
