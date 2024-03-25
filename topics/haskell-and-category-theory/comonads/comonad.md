## Relative queries

A relative QUERY takes the next `n` slots form the current VIEW.

```hs
takeS :: Int -> Stream a -> [a]
takeS n s = take n (toList s)

toList :: Stream a -> [a]
toList (x :> xs) = x : toList xs

fromList :: [a] -> Stream a
fromList xs = aux $ cycle xs
  where
  aux [] = error "empty list"
  aux (x:xs) = x :> aux xs
```


The `extend` LIFTS QUERIES into mutations (change of configuration of the stream, like a change of focus - has nothing to do with mutations proper).

The `extend` runs a query over each slot's VIEW.

```hs
natS -- stream a la [0..]

x1 = takeS 3 natS -- [0,1,2]

x2 = extend (takeS 3) natS
-- [0,1,2]
-- [1,2,3]
-- [2,3,4]
-- …
```

We can use `extend` to extend a contextual query across the entire stream, turning a query into a mutation.

If we just `takeS` the first 3 elements of the stream of ℕ we get [0,1,2] as in x1, but if we `extend` the query along the entire stream - recall that `extend` is **mapping a function over the duplicated stream**,

```hs
extend :: (w a -> b) -> w a -> w b
extend :: (Stream a -> b) -> Stream a -> Stream b
extend f s = fmap f (duplicate s)
```

then we get the stream of "windows" (as in the example `x2`).

Duplicating a stream results in a stream of streams with each slot holding a stream "shifted" in some way related to a previous one. Duplicating a stream of ℕ gives us the stream where each slot is focused on the stream, shifted by the index number of slots.

Mapping a function over a duplicated stream then results in various realizations of the mapped stream in each slot. Mapping a duplicated stream with `id` changes nothing. Mapping a duplicated stream with `takeS 3` produces a stream of shifted finite windows or views into the original stream.

Extending `takeS 3` over the stream of ℕ gets us the stream with slots that have finitely many elements (3). We've ran this query over every possible root of the original stream. And with that, we have *captured a contextual window of the stream inside each slot* (this *is* something that is normally pretty tough to accomplish). We wrote a plain `takeS` function which performs a simple operation over a structure, doing its thing once, but by calling `extend` on it, we are able to cover the entire structure with it (query the entire stream).


### Exercise: compute a ROLLING AVERAGE over a stream of integers

At each slot, we want to figure out what the average of the nearest `n` elements (the window) is.

```hs
rollingAverage :: Int             -- window size
               -> Stream Int      -- input stream of integers
               -> Stream Double   -- stream of averages

-- e.g.
rollingAverage 2 evens
-- (0 + 2) / 2 :> 
-- (2 + 4) / 2 :> 
-- (4 + 6) / 2 :> 
-- …
-- evens + tail evens
-- (x + (+ 2)) / 2
-- ((/ 2) . (+ 2))
```

## Abstract comonads

(since comonads just aren't abstract enough…)

Recall the `Comonad` class:

```hs
class Functor w => Comonad w where
  extract   :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f s = fmap f (duplicate s)
```

### Selection queries¹

(¹non-standard name)

Selection queries are special kind of queries of type `forall a. w a -> a`. We have already met a few like `ixS` and `extract`.

Selection queries can select a slot no matter the type (but, as normal, cannot touch a polymorphic type). Running a selection query retrieves the slot's value. *Extending a selection query rearranges slot positions*.

For example, `extend (ix 2)` selects the slot two to the right (two places to the right) in all views. Basically, shifting each slot is equivalent to shifting the entire stream.

>Shifting each slot individually = shifting the entire stream.



```hs
x1 :: Stream Int
x1 = natS
-- 0 :> 1 :> 2 :> 3 :> 4 :> …

x2 :: Stream (Stream Int)
x2 = duplicate natS
-- 0 :> 1 :> 2 :> …
-- 1 :> 2 :> 3 :> …
-- 2 :> 3 :> 4 :> …
-- …

x3 :: Stream (Stream Int)
x3 = ixS 2 $ duplicate natS
-- 2 :> 3 :> 4 :> …

x4 :: Stream Int
x4 = ixS 2 <$> duplicate natS
x4 = fmap (ixS 2) duplicate natS
-- 2 :> 3 :> 4 :> …


q1 :: Stream Int
q1 = extend (ixS 2) (duplicate natS)
-- the stream is shifted
-- 2 :> 3 :> 4 :> …
-- 3 :> 4 :> 5 :> …
-- 4 :> 5 :> 6 :> …
-- …
```

*flattens* the stream and *shifts* it by 2 (relatively) to the right. The mapping function `ix` just retrieves the value at a given index `n`, but when `extend`ed over the stream, it has the effect of shifting the stream by `n` places (like `drop n`). All in all, shifting each slot is equivalent to shifting the entire stream.


But `windowedAvg :: Int -> Stream Int -> Double` is not a selection query.

```hs
windowedAvg 3  $  duplicate natS
-- Type error: Stream (Stream Int) !== Stream Int

windowedAvg 3 <$> duplicate natS
-- 1.0 :> 2.0 :> 3.0 :> …
```

More examples of selection queries:

```hs
natS                -- 0 :> 1 :> 2 :> 3 :> 4 :> …

ix 5 natS           -- 5

extend (ix 5) natS  -- 5 :> 6 :> 7 :> 8 :> 9 :> …
```

## Comonad notation

- `>>=` vs `<<=` (`> > =` vs `< < =`)
- `=<<` vs `=>>` (`= < <` vs `= > >`)

- `(=>>) = extend`

-  `>>=` : `=<<`     `> > =` : `= < <`
- `=>>=` ? `=<<=`   `= > > =` ? `= < < =` Cokleisli composition
- `=>>`  :  `<<=`   `= > >`  :  `< < =`

The `=>>` takes a structure on the left and a function on the right, `s =>> f`


```hs
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend

-- Kleisli composition
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

-- | Co-Kleisli (right-to-left) composition
-- (<=<) ::           (b -> m c) -> (a -> m b) -> (a -> m c)  Kleisli R2L
-- (=<=) ::           (b <- w c) -> (a <- w b) -> (a <- w c)  rev
-- (=<=) ::           (w c -> b) -> (w b -> a) -> (w c -> a)  canon
(=<=) :: Comonad w => (w a -> b) -> (w b -> c) -> (w a -> c)

-- | Co-Kleisli L2R composition
-- (<=<) ::           (b -> m c) -> (a -> m b) -> (a -> m c)  Kleisli R2L
-- (>=>) ::           (a -> m b) -> (b -> m c) -> (a -> m c)  Kleisli L2R
-- (=>=) ::           (a <- w b) -> (b <- w c) -> (a <- w c)  rev Kleisli L2R
-- (=>=) ::           (b <- w c) -> (a <- w b) -> (a <- w c)  flip
-- (=>=) ::           (w c -> b) -> (w b -> a) -> (w c -> a)  canon
-- (=>=) ::           (w a -> b) -> (w b -> c) -> (w a -> c)  rename
(=>=) :: Comonad w => (w a -> b) -> (w c -> a) -> (w c -> b)
```


* Operators

```hs
-- | 'extend' in operator form
(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend

-- | 'extend' with the arguments swapped. Dual to '>>=' for Monad
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

-- | Right-to-left Co-Kleisli composition
(=<=) :: Comonad w => (w b -> c) -> (w a -> b) -> (w a -> c)
f =<= g = f . extend g

-- | Left-to-right Co-Kleisli composition
(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> (w a -> c)
f =>= g = g . extend f
```

## Comonad composition

Comonad composition is dual to Kleisli composition of Kleisli arrows.

```hs
-- | Left-to-right Co-Kleisli composition
(=>=) :: Comonad w
      => (w a -> b)
      -> (w b -> c)
      -> (w a -> c)
f =>= g = g . extend f

-- | Right-to-left Co-Kleisli composition
(=<=) :: Comonad w
      => (w b -> c)
      -> (w a -> b)
      -> (w a -> c)
f =<= g = f . extend g
```

<!-- #region Interezzo -->

<details><summary>Interezzo</summary>

```hs
-- | Right-to-left composition, .⃗ , b⃗ , c⃗
(⥁) :: (b -> c) -> (a -> b) -> a -> c
(⥁) = (.)

-- | Left-to-right composition, .⃖ , b⃖, c⃖, ○⃗
(⥀) :: (a -> b) -> (b -> c) -> a -> c
(⥀) = flip (.)

f,g :: a -> Integer
f = const 0
g = const 1

h1,h0 :: Integer
h0 = (f ⥁ g) () -- 0    f (g x) = f . g
h1 = (f ⥀ g) () -- 1    g (f x) = g . f
```

</details>

<!-- #endregion -->

## Store abstract comonad

https://youtu.be/dOw7FRLVgY4?list=PLcAu_kKy-krxDD1WwRX_9rc0knAFK3nHs&t=740

Store aka co-State

The working example is a well-stocked (there is always an item on the shelf) warehouse with shelf with items and there is a forklift parked at a specific shlef (focused view). We can use `pos` to get the index of the shelf; `peek` gets the item by an absolute shelf index; `peeks` get an item by a relative address.

The signature of the `store` comonad is below, where `s` is the shelf name and the `a` is the type of item.

```hs
data Store s a = Store (s -> a) s
```

We're defining it not as data but as a function, so we can say that for any shelf number we can get some item. It's an infinite wharehouse. Alternativelly, we can wrap the output within a `Maybe` just so we can provide some output for every possible input.

```hs
inventory :: Map Int String
inventory =
  [ 0 ⟼ "Ache"
  , 1 ⟼ "Bladder" -- focus
  , 2 ⟼ "Circus"
  , 3 ⟼ "Door"
  ]

warehouse :: Store Int (Maybe String)
warehouse = Store (\ shelf -> M.lookup shelf inventory) 1

>>> pos warehouse         -- 1 (position)
>>> peek 0 warehouse      -- Just "Ache"   (absolute address)
>>> peeks (- 1) warehouse -- Just "Ache"   (relative address)
>>> peeks (+ 1) warehouse -- Just "Circus" (relative address)
>>> seek 3 warehouse      -- moves the focus (absolute)
>>> seeks (+1) warehouse  -- moves the focus (relative)
```

Store aka co-State
- has a slot for every key
- has a view from every key
- can extract the value at a key
