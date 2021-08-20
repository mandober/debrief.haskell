# GHC Rewrite rules

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/rewrite_rules.html

```hs
-- syntax
{-# RULES "⟨name⟩" forall ⟨binder⟩ ... . ⟨expr⟩ = ⟨expr⟩ ... #-}

-- example: 2 rules
{-# RULES
  "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs
  "map/append" forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys
  #-}

-- [2] means that the rule is active in Phase 2 and subsequent phases
-- [~2] means the rule is active up to, but not including, Phase 2
{-# RULES
      "map/map" [2]  forall f g xs. map f (map g xs) = map (f.g) xs
  #-}

-- [~] the rule is never active (but parsed to be available to GHC plugins)
{-# RULES
  "monad_law1" [~] forall a k. return a >>= k = k a
  #-}
```

- `-fno-enable-rewrite-rules` disable rewrite rules application
- `-ddump-simpl-stats` see what rules fired
- `-ddump-rule-firings` shows you each individual rule firing
- `-ddump-rule-rewrites` shows what code looks like before and after the rewrite


- Each rule has a name, enclosed in double quotes (not significant).
- optionally a phase-control number, immediately after the name of the rule.
