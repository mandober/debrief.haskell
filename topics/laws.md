# Laws


```hs
-- ----------------------------------------------------------------------------
Monoid
-- ----------------------------------------------------------------------------
             m <> mempty === m === mempty <> m
              x <> (y <> z) === (x <> y) <> z          -- associativity
                    mconcat === foldr (<>) mempty      -- concatenation
-- ----------------------------------------------------------------------------
Foldable
-- ----------------------------------------------------------------------------

foldr f z t === appEndo (foldMap (Endo . f) t) z
foldl f z t === appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
       fold === foldMap id
     length === getSum . foldMap (Sum . const 1)
-- ----------------------------------------------------------------------------
Functor
-- ----------------------------------------------------------------------------
                    fmap id === id
            fmap g . fmap f === fmap (g . f)

-- ----------------------------------------------------------------------------
Applicative
-- ----------------------------------------------------------------------------

              pure id <*> x === x
         pure  f <*> pure x === pure (f x)                  -- homomorphism
               f <*> pure x === pure ($ x) <*> f            -- interchange
pure (.) <*> f <*> g <*> x  === f <*> (g <*> x)
-- ----------------------------------------------------------------------------
Monad
-- ----------------------------------------------------------------------------

             return a >>= k === k a                         -- left identity
               m >>= return === m                           -- right identity
    m >>= (\x -> k x >>= h) === (m >>= k) >>= h             -- assoc

-- ----------------------------------------------------------------------------
Functor / Applicative / Monad
-- ----------------------------------------------------------------------------
                  fmap f xs === pure f <*> xs
                  fmap f xs === xs >>= return . f
              pure f <*> xs === xs >>= return . f
fmap f xs === pure f <*> xs === xs >>= return . f
                       pure === return
                       fmap === liftA
                       <*>  === ap
-- ----------------------------------------------------------------------------
Monad transformers
-- ----------------------------------------------------------------------------
             lift . return  === return
             lift (m >>= f) === lift m >>= (lift . f)
-- ----------------------------------------------------------------------------
Category
-- ----------------------------------------------------------------------------
               f . id === f === id . f
                f . (g . h) === (f . g) . h
-- ----------------------------------------------------------------------------
Arrow
-- ----------------------------------------------------------------------------
                     arr id === id
              arr (f >>> g) === arr f >>> arr g
              first (arr f) === arr (first f)
            first (f >>> g) === first f >>> first g
        first f >>> arr fst === arr fst >>> f
 first f >>> arr (id *** g) === arr (id *** g) >>> first f
first (first f)>>>arr assoc === arr assoc >>> first f
                      where
                      assoc ((a,b),c) = (a,(b,c))


-- ----------------------------------------------------------------------------
Foldable Laws
-- ----------------------------------------------------------------------------
[1] foldr f z t = appEndo (foldMap (Endo . f) t) z
[2] foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
[3] fold = foldMap id
[4] length = getSum . foldMap (Sum . const 1)

-- The `sum`, `product`, `maximum`, `minimum` should all be essentially equivalent to `foldMap` forms, such as (but may be less defined):
[5] sum     = getSum     . foldMap Sum
    product = getProduct . foldMap Product

-- If the type is also a `Functor` instance, it should satisfy:
[6] foldMap f = fold . fmap f
-- which implies
[7] foldMap f . fmap g = foldMap (f . g)


-- ----------------------------------------------------------------------------
MonadZip
-- ----------------------------------------------------------------------------

-- naturality:
liftM (f *** g) (mzip ma mb) = mzip (liftM f ma) (liftM g mb)
-- Information Preservation:
liftM (const ()) ma = liftM (const ()) mb -->
munzip (mzip ma mb) = (ma, mb)

-- ----------------------------------------------------------------------------
MonadFix
-- ----------------------------------------------------------------------------
-- Monads having fixed points with a 'knot-tying' semantics. Instances of MonadFix should satisfy the following laws:

-- purity
mfix (return . h) = return (fix h)
-- left shrinking (or tightening)
mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)
-- sliding
mfix (liftM h . f) = liftM h (mfix (f . h)), for strict h.
-- nesting
mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)


-- ----------------------------------------------------------------------------
Monadic laws (do-notation)
-- ----------------------------------------------------------------------------
do { y <- do { x <- m; f x } g y }
do { x <- m; do { y <- f x; g y } }
do { x <- m; y <- f x; g y }
```
