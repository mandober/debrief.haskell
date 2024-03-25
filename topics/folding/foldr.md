## Right fold

```hs
foldr :: Foldable f => (a -> b -> b) -> b -> f a -> b

foldl :: Foldable f => (b -> a -> b) -> b -> f a -> b
```

Right-associative fold of a structure, lazy in the accumulator.

In the case of lists, `foldr`, when applied to a binary operator, a starting value (typically the right-identity of the operator), and a list, reduces the list using the binary operator, from right to left:

>foldr f z [x₀, x₁, …, xₙ] == x₀ `f` (x₁ `f` … (xₙ `f` z) …)

```hs
foldr (⊕) z []        == z
foldr (⊕) z [a]       == a ⊕ z
foldr (⊕) z [a, b]    == a ⊕ b ⊕ z
foldr (⊕) z [a, b, c] == a ⊕ b ⊕ c ⊕z

foldr (⊕) z [x₀, x₁, …, xₙ] == x₀ ⊕ (x₁ ⊕ … (xₙ ⊕ z) …)
```


Note that since the head of the resulting expression is produced by an
application of the operator to the first element of the list, given an
operator lazy in its right argument, 'foldr' can produce a terminating
expression from an unbounded list.

For a general 'Foldable' structure this should be semantically identical
to,

@foldr f z = 'List.foldr' f z . 'toList'@

==== __Examples__

Basic usage:

>>> foldr (||) False [False, True, False]
True

>>> foldr (||) False []
False

>>> foldr (\c acc -> acc ++ [c]) "foo" ['a', 'b', 'c', 'd']
"foodcba"

===== Infinite structures

⚠️ Applying 'foldr' to infinite structures usually doesn't terminate.

It may still terminate under one of the following conditions:

* the folding function is short-circuiting
* the folding function is lazy on its second argument

====== Short-circuiting

@('||')@ short-circuits on 'True' values, so the following terminates
because there is a 'True' value finitely far from the left side:

>>> foldr (||) False (True : repeat False)
True

But the following doesn't terminate:

>>> foldr (||) False (repeat False ++ [True])
* Hangs forever *

====== Laziness in the second argument

Applying 'foldr' to infinite structures terminates when the operator is
lazy in its second argument (the initial accumulator is never used in
this case, and so could be left 'undefined', but @[]@ is more clear):

>>> take 5 $ foldr (\i acc -> i : fmap (+3) acc) [] (repeat 1)
[1,4,7,10,13]
