# Comments

Haskell has
- line comment, `-- valid to the end of line`
- block comment, `{- block comment -}`
- special block comments for pragmas, `{-# pragma #-}`
- doc comments
  - line doc comments, `-- | line doc comments`
  - block doc comments, `{-| block doc comments -}`
  - field doc comment, ` -- ^ describe a record field`

The two dashes that start a line comment need to be separated by a space if the next character is symbolic, e.g. `-->` won't be parsed as a comment, but `--a` will be parsed as a comment. It's best practice to always follow a comment opening with a space.

For doc comments, the symbol that separates a common comment from a haddok doc comment is `|` when it appears immediately after the `{-`, but separated with a space following `--`, i.e. `-- |` introduces a doc line comment,  but `--|` does not

```hs
-- line comment
--also a line comment
-- <decorative line comment>
--<not a comment at all>
-- | line doc comment
--| not a comment at all
--|not a comment

{-
block
comment
-}

{- | block
doc comment
-}

{-|
also a block
doc comment
-}
```
