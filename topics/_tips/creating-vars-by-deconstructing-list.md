# Creating vars by deconstructing a list

You can quickly (and unidiomatically) define a bunch of variables by pattern matching. If needed, type signatures may only be stated separately for each variable. May come handy for quick ghci prototyping.

```hs
-- homo
[a,b,c,d] = [0, 1, 2, 3]

-- hetero
(a,b,c,d) = (1, 'a', "come", True)

-- bind first 3 entries only:
-- one underscore for each ignored value
(a,b,_,_) = (10, '\n', "see", False)


-- we cannot do this
[a,b,c,d] = [1, 'a', "over", True]
-- we can do this
[a,b,c,d] = [toDyn 1, toDyn 'a', toDyn "over", toDyn True]
-- but then we get these types
a :: <<Integer>>
b :: <<Char>>
c :: <<[Char]>>
d :: <<Bool>>
-- whaaa?!
```
