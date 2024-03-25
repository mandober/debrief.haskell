## Left fold

```hs
foldl :: Foldable f => (b -> a -> b) -> b -> f a -> b

foldr :: Foldable f => (a -> b -> b) -> b -> f a -> b
```

Left-associative fold of a structure, lazy in the accumulator.

This is rarely what you want, but can work well for structures with efficient right-to-left sequencing and an operator that is lazy in its left argument.

In the case of lists, `foldl`, when applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:

>foldl f z [x₀, x₁, …, xₙ] ≡ (… ((z `f` x₀) `f` x₁) `f` …) `f` xₙ


```hs
foldl (⊕) z []        == z
foldl (⊕) z [a]       == z ⊕ a
foldl (⊕) z [a, b]    == z ⊕ a ⊕ b
foldl (⊕) z [a, b, c] == z ⊕ a ⊕ b ⊕ c

foldl (⊕) z [x₀, x₁, …, xₙ] == (… ((z ⊕ x₀) ⊕ x₁) ⊕ …) ⊕ xₙ
```

To produce the outermost application of the operator *the entire input list must be traversed*.

Like all left-associative folds, `foldl` *diverges on infinite lists*.

If you want an efficient strict left-fold, you probably want to use `foldl'` instead of `foldl`. The reason for this is that the latter does not force the "inner results", z `f` x₁ (from the above example) before applying them to the operator, to `f` x₁.

This results in a thunk chain `O(n)` elements long, which then must be evaluated from the outside-in.


For a general Foldable structure this should be semantically identical to:   
`foldl f z = List.foldl f z . toList`


## Examples

The first example is a strict fold, which in practice is best performed with `foldl'`.

```hs
foldl (+) 42 [1,2,3,4] -- 52
```


Though the result below is lazy, the input is reversed before prepending it to the initial accumulator, so co-recursion begins only after traversing the entire input string.

```hs
foldl (\acc c -> c : acc) "abcd" "efgh" -- "hgfeabcd"
```

A left fold of a structure that is infinite on the right cannot terminate, even when for any finite input the fold just returns the initial accumulator:

```hs
foldl (\a _ -> a) 0 $ repeat 1 -- HANGS
```

>When it comes to lists, prefer `foldl'` or `foldr'` over `foldl`.
