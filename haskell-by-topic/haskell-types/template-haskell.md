# Template Haskell

* Beautiful Template Haskell -  Matthew Pickering @MuniHac 2018
https://www.youtube.com/watch?v=AzJVFkm42zM&list=TLPQMDcwNDIwMjSMAbWhER01lg

In Template Haskell, expressions are created by quoting, and used by splicing.

>In Template Haskell, *quotes* produce expressions, *splices* consume them.

```hs
-- quote
x :: Code Int
x = [| 2 + 3 |]

-- splice
s :: Code Int
s = [| 4 + $(x) |]
```

Quotes generate pieces of AST. Splices also generate parts of AST after they evaluate something.

The quote above would generate the piece of AST above, and the splice would generate the piece of AST below, and then it would insert the above AST in place of `x`.

```
  + ────→─┐
 / \      │
2   3     │
          │
  +       │
 / \      │
4   x ←───┘
```

Template Haskell is typed, so the splice below would not type-check:

```hs
-- quote
x :: Code Int
x = [| "string" |]

-- splice
s :: Code Int
s = [| 1 + $(x) |]
```

- Quote:  `e :: T`      so `[| e |] :: Code T`
- Splice: `c :: Code T` so `$(c) :: T`

Non-hygienic expressions are disallowed:

```hs
nonhygienic = [| \ x -> $(x) |]
-- at this point (pass) `x` is not defined
-- even if it appears above somewhere
```

But this can be solved with "levels" (playing a kind of scope for expressions) - each quoting bracket introducing another level of quoting (scope).

```hs
nonhygienic = ⟦ \ x -> $(x) ⟧
--              ↓         ↓
--           @level1    @level0

hygienic = ⟦ \ x -> $(⟦ x ⟧) ⟧
--              ↓         ↓
--           @level1    @level1
```

where `⟦` and `⟧` stand for `[|` and `|]` (unclear if this is accepted)

>**Principle of levels**: a variable can only be used at the level at which it was bound.

so this won't compile:

```hs
dbl :: Int -> Code Int
dbl x = ⟦ x * 2 ⟧ -- (LEVEL) ERROR!
-- l0    l1
```

To correct, we need the `Lift` (oh, no, not again) class which enables serialization-based cross-stage (program compilation-wise) persistence.

```hs
class Lift a where
  lift :: a -> Code a

instance List Int where
  lift = undefined

dbl :: Int -> Code Int
dbl x = ⟦ $(lift x) * 2 ⟧
```

>**Principle of levels**: a variable can only be used at the level at which it was bound ...unless it is persisted across program compilation stages.

Many things are persistable, but functions are not.

However, top-level identifiers will still be bound in the compiled code. It is called **path-based persistence**.

So this is allowed:

```hs
module M where

succ :: Int -> Int
succ x = x + 1
-- ^ level 0

codeSucc :: Code (Int -> Int)
codeSucc = [| succ |]
--              ^ level 1
```
