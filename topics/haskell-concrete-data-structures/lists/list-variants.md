# List variants

- cons-list
- snoc-list
- dlist

Haskell's builtin list datatype is a singly-linked list, or cons-list, i.e. right-biased list made of cons cells, basically pairs of `(data, next)`, where each cons-cell contains two fields: "data" field points to the payload, and the "next" field holds a pointer to the next list element.

```hs
-- cons-list
data List a = Nil | Cons a (List a)

-- snoc-list
data List a = Nil | Snoc (List a) a
```

The snoc-list is also based on cells only snoc-cells since the order of the fields is reversed, `(next, data)`, which doesn't change a goddamn thing; the list still begins with the head cell, and the tailing cells follow. It is not "reversed" in any practical way.

Difference list, or `DList`, is a repr of list using partially applied (`++`) function. The type then becomes `DList :: [a] -> [a]`. It is useful repr when you need to build a list out of a large number of small ones. Because when concatenating two lists, `xs ++ ys`, the right list is not touched, only `xs` list is traversed to locate its last element, which is then cons'ed (copied or possibly shared) onto the `ys` list; and so on, recursively, until `xs` is depleted.

```hs
-- (++) is right-associative
infixr 5 ++

-- good concat: right-associative
a ++ b ++ c ++ d ++ e ++ f

-- good concat: right-associative
a ++ (b ++ (c ++ (d ++ (e ++ f))))

-- bad concat: left-associative
((((a ++ b) ++ c) ++ d) ++ e) ++ f
```
