# Heterogeneous list

## Heterogeneous list with advanced typing

```hs
cons :: Type -> [Type]

```


## Dynamic list

Another way to create a heterogeneous list is to use the `Dynamic` type from the module `Data.Dynamic`, along with the `toDyn` function to cast a value of any conrete type to a dynamic type.

```hs
-- entered in ghci as a bare expressions:

[toDyn 1, toDyn 'c', toDyn "abc", toDyn True]
-- [<<Integer>>,<<Char>>,<<[Char]>>,<<Bool>>]
-- it :: [Dynamic]

map toDyn [toDyn 1, toDyn 'c', toDyn "abc", toDyn True]
-- [<<Dynamic>>,<<Dynamic>>,<<Dynamic>>,<<Dynamic>>]

-- cannot be entered like this:
-- map toDyn [1, 'c', "abc", True]
```

First time to witness the double angle-brackets notation, `<<Type>>`.
