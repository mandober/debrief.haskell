# const

The Prelude's function `const`, in combinatory logic called the `K` combinator, is incorrectly called constant function, when the "constant function maker" is probabily more appropriate.

1. When consider in a traditional view of fully applied functions, it is a binary (heterogeneous, i.e. the two args may have diff types) function that just returns its first arg, ignoring the second.

```hs
const :: a -> b -> a
const a _ = a

const 1 2       -- 1
const 1 ()      -- 1
const 1 'a'     -- 1
const 1 id      -- 1
const id 2 1    -- 1
const id id 1   -- 1
const const 3 2 1               -- 1
const (\a b -> b) 3 2 1         -- 1
const (\a b -> a - b) 3 5 4     -- 1
const 1 $ (\a b -> a + b) 5 2   -- 1
```

const ≡ (\a b -> a)
const ≡ curry fst
const ≡ flip $ curry snd ≡ (flip . curry) snd


2. When partially applied (only first arg provided), it may be considered as a simplest container that will hold onto the first arg.

```hs
setVal :: a -> b -> a
setVal = const

xv = setVal 1

getVal :: (() -> a) -> a
getVal c = c ()

v = getVal xv
```
