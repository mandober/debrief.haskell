# Monoid

For any Monoid: `mconcat = foldr (<>) mempty`

In general, ant recursive function on lists can be expressed in terms of `foldr`. For example

```hs
idList :: [a] -> [a]
idList = (foldr op id) []
  where
  op x f = \ls -> f (x : ls)
```
