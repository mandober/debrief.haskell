# Comonad class

## Comonad

```hs
class Functor w => Comonad w where
  {-# MINIMAL extract, (duplicate | extend) #-}
  counit    :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend    :: (w a -> b) -> (w a -> w b)
  extend f = fmap f . duplicate

  extend'   :: w a -> (w a -> b) -> w b

  bird      :: (w a -> b) -> (w b -> c) -> (w a -> c)
  bird f g = g . fmap f . duplicate -- vs (join . fmap g . f)

  bird'      :: (w a -> b) -> (w b -> c) -> (w a -> c)
  bird' f g = g . extend f


  -- bird f g wa = g (fmap f (duplicate wa))
  --
  -- (g . fmap f . duplicate) wa = g (fmap f (duplicate wa))
  --
  --  let wwa = duplicate wa
  --      wwac = g . fmap f
  --  in  wwac wwa

  -- bird f g = g . extend f
  -- bird f g wa =
  --   let m = extend f
  --       wb = m wa
  --   in  g wb

{-
  f =>= g = g . fmap f . duplicate
  f =>= g = g . extend f
  extend f = fmap f . duplicate
  duplicate = extend id
-}
```

## Monad

```hs
class Functor m => Triple m where
  unit  :: a -> m a

  join  :: m (m a) -> m a
  join = bind' id

  bind' :: (a -> m b) -> (m a -> m b)
  bind' f = join . fmap f

  bind  :: m a -> (a -> m b) -> m b
  bind ma f = join (fmap f ma)

  fish  :: (a -> m b) -> (b -> m c) -> (a -> m c)
  fish f g = join . fmap g . f -- vs (g . fmap f . duplicate)

  fish'  :: (a -> m b) -> (b -> m c) -> (a -> m c)
  fish' f g = bind' g . f

    -- fish f g a = join (fmap g (f a))
    -- 2)
    -- let mb = f a         -- mb = (f :: a -> m b) a
    --     mmc = fmap g mb  -- fmap (g :: b -> m c) :: m b -> m (m c)
    -- in  join mmc         -- mc = (join :: m (m a) -> m a) mmc

{-
  f >=> g = (=<<) g . f
  f >=> g = join . fmap g . f
  (>>=) f = join . fmap f
  join = (=<<) id
-}
```
