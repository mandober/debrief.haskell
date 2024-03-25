# Store

- https://hackage.haskell.org/package/comonad-5.0.8/
- https://hackage.haskell.org/package/comonad
- https://hackage.haskell.org/package/comonad/docs/Control-Comonad.html
- https://hackage.haskell.org/package/comonad/docs/src/Control-Comonad.html
- https://hackage.haskell.org/package/comonad/docs/Control.Comonad.Trans.Store.html
- https://hackage.haskell.org/package/comonad/docs/src/Control.Comonad.Trans.Store.html


The `Store` comonad (aka co-State) is a data structure with a focused position.

>A store is a getter and a key. The `Store` comonad holds a constant value, and a modifiable accessor function, which maps the stored value to the focus.

The `Store` data type is essentailly a pair made of an accessor or getter function, `s -> a`, and a constant value of type `s`. The constant value represents a position (an index) within the structure; it represents the focused positon. Applying the accessor to the value would yield the item at the focused position.


The **store comonad** holds a constant value along with a modifiable *accessor* function that maps the *stored value* to the *focus*.

The module `Control.Comonad.Trans.Store` defines the **strict store** (aka state-in-context/costate) comonad transformer.

```hs
-- stored value = (1, 5)
-- accessor = fst
-- resulting focus = 1

storeTuple :: Store (Int, Int) Int
storeTuple = store fst (1, 5)
```

Add something to the focus:

```hs
addToFocus :: Int -> Store (Int, Int) Int -> Int
addToFocus x wa = x + extract wa

added3 :: Store (Int, Int) Int
added3 = extend (addToFocus 3) storeTuple
```

The focus of `added3` is now 1 + 3 = 4. However, this action changed only the accessor function and therefore the focus, but not the stored value:

```hs
x1 = pos added3 -- (1,5)
x2 = extract added3 -- 4
```

The strict store (state-in-context/costate) comonad transformer is subject
to the laws:

```hs
x = seek (pos x) x
y = pos (seek y x)
seek y x = seek y (seek z x)
```



## Store data type

```hs
-- Store is essentially a pair
type    Store s a = (s -> a, s)
-- so it's best defined as a newtype
newtype Store s a = Store (s -> a, s)
-- possibly with an accessor
newtype Store s a = Store { runStore :: (s -> a, s) }

-- as ADT
data Store s a = Store (s -> a) s

-- as GADT
data Store s a where
  Store :: (s -> a) -> s -> Store s a


-- Store as StoreT
type    Store  s     = StoreT s Identity
type    Store  s     = StoreT (Identity (s -> a)) s

-- StoreT
data    StoreT s w a = StoreT (w (s -> a)) s

type    StoreT s w a =         w (s -> a, s)
newtype StoreT s w a = StoreT (w (s -> a), s)
```


## Store Functor

The `Store` is a `Comonad`, but we first need to define its `Functor` instance.

```hs
instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap g (Store (f, x)) = Store (g . s, a)
```

So, we need to map a value of type `Store s a` with a function `a -> b` to get another type of store, `Store s b`.

```hs
map :: (a -> b) -> Store s a -> Store s b
map g sa = ...
```

Obviously, we'll have to deconstruct the input store `sa` since we can't do anything at the moment with only `g` and `sa` at our disposal.

```hs
map :: (a -> b) -> Store s a -> Store s b
map g (Store f x) = Store (g . f) x
```

As soon as we deconstruct the store `sa` into `Store f x`, we get the function `f :: s -> a` and it becomes obvious we'll need to precompose `g` with it.

The composition `g . f` yields the function `s -> b`, which when paired with the value `x` (we already have) gives us the store `Store s b`.

```hs
newtype Store s a = Store { runStore :: (s -> a, s) }

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap g (Store (f, x)) = Store (g . s, a)



type Store s a = (s -> a, s)
-- Store s a = (s -> a, s)
-- Store s b = (s -> b, s)
map :: (a -> b) -> (s -> a, s) -> (s -> b, s)
map g (f, x) = (g . f) x
```


## Store Comonad

Then we can define its `Comonad` instance:

```hs
instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store s a) = s a

  duplicate :: Store s a -> Store s (Store s a)
  duplicate = extend id

  extend :: forall a b. (Store s a -> b) -> Store s a -> Store s b
  extend g (Store f s) = Store (g . sb) s
    where
    sb :: s -> Store s a
    sb = \s' -> (Store f s')
```

## Warehouse

Warehouse:
- infinite number of labeled shelves that hold various items
- well stocked: every shelf is occupied
- focused key: forklift is at the focused shelf
- every key (position) has a value (item at the shelf)

A store can be tought of as a warehouse with an infinite number of shelfs, each shelf identified by a position, and with the focus on a particular shelf. We can imagine a forklift parked at the focused position that can retrieve the item off the shelf.

Due to the presence of the focus, which in this example represents a position, some operations won't need us to supply the position explicitly since they can refer to the implicit position. The implicit (focused) position is always guaranteed to exist and be valid with regards to the comonadic structure.

We can model a warehouse by first defining an inventory, as a mapping from integer indices (representing positions) to strings (representing items at the shelfs).

```hs
inv :: M.Map Int String
inv = M.fromList
  [ (0, "Spun")
  , (1, "Muck")
  , (2, "Gut")
  , (3, "Lap")
  ]
```

To actually build a store (which is a getter and a key) we'll use the `Map`'s `lookup` function as a getter, and choosing the first index `0` as a key.

The fact that `M.lookup` returns a `Maybe` works in our favour here because a store is an infinite storage where any possible position (index) is present and guranteed to return something (even if that something is `Nothing`).

```hs
-- an infinite warehouse as a Store comonad
w :: Store Int (Maybe String)
w = Store (\ s -> M.lookup s inv) 0
```

## Queries over a store

### extract

`extract` gets the focused item, i.e. item at the focused position/shelf.

```hs
extract wh -- "Spun"
```

### pos

- pos reads the stored value (reads the focused position).
- pos query returns the focused position in the store.
- pos asks which shelf has the focus (which shelf the forklift is at).

```hs
pos $ store fst (1,5)   -- (1,5)

pos          wh -- 0
pos $ seek 1 wh -- 1
```

### seek

- seek sets the stored value.
- seek returns the item identified by an absolute `pos`.
- seek moves the forklift to the specific shelf.
- seek is an *absolute mutation*.
- seek puts the focus on the given key
- seek satisfies the law: __seek s = peek s . duplicate__

```hs
pos . seek (3,7) $ store fst (1,5)  -- (3,7)

seek 3 wh
pos     $ seek 1 wh -- 1
extract $ seek 1 wh -- Just "Spun"
```

### seeks

- seeks modifies the stored value.
- seeks is a *relative mutation*
- seek moves the forklift to a new, relative, position( shelf).
- seeks satisfies the law: __seeks f = peeks f . duplicate__

```hs
pos . seeks swap $ store fst (1,5) -- (5,1)

seeks (+0) w -- Just "Spun"
seeks (+1) w -- Just "Muck"
seeks (+2) w -- Just "Gut"
seeks (+3) w -- Just "Lap"
seeks (+4) w -- Nothing
```

### peek

- peek at what the current focus would be for a different stored value
- peek gets the item from a specified shelf.
- peek satisfies the law: __peek x . extend (peek y) = peek y__
- absolute queries like `peek`ignore the current view (current focus)
- `peek 2` is an *absolute query*

```hs
peek 2 wh -- Just "Gut"
peek 2    -- absolute query
```

### peeks

- peeks at what current focus would be if the stored value was modified by a fn.
- peeks gets the item from a shelf relative to current position.
- relative queries like `peeks` depend on the current focus.
- relative queries like `peeks` have diff results depending on current view
- `peeks (+1)` is a *relative query*.

```hs
peeks (+ 1) wh -- Just "Gut"
```

### experiment

Applies a functor-valued function to the stored value, and then uses the new accessor to read the resulting focus.

```hs
experiment :: Functor f => (s -> f s) -> Store s a -> f a
```

The purpose of `experiment` is to quickly survey a bunch of neighbouring values at once, instead of doing it in sequance. Especially useful if we have to `peek` at many values that are located in proximity. Experiment gets a bunch of values relative to the focused position.

```hs
let f x = if x > 0 then Just (x ^ 2) else Nothing
experiment f $ store (+ 1)    2  -- Just 5
experiment f $ store (+ 1) (- 2) -- Nothing
```

## Store example: squared

```hs
squared :: Store Int Int
squared = Store (\x -> x^2) -- getter
          10                -- focus

x0 = experiment (\n -> [n - 10, n + 10]) squared
-- [ 0    -- (10 - 10)^2
-- , 400  -- (10 + 10)^2
-- ]

aboveZero n | n > 0     = Just n
            | otherwise = Nothing

x1 = experiment aboveZero (seek 10 squared)     -- Just 100
x2 = experiment aboveZero (seeks (-10) squared) -- Nothing
```
