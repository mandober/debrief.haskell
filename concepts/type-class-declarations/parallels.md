# Type class parallels

class         | main  | semi1 | semi2 | flipped | unit
Functor       | <$>   | <$    | $>    | <&>     | void
Contravariant | >$<   | >$    | $<    | <$$>    | phantom
Applicative   | <*>   | <*    | *>    | <**>    | 
Monad         | >>=   | >>    | <<    | =<<     | 
Kleisli       | >=>   | =>=   | =<=   | <=<     | 
Comonad       | =>>   | >>-   | -<<   | <<=     | 
Alternative   | <|>   |       |       |         | 
Arrow         | <***> | <&&&> |       |         | 


- Maybe : Alternative : MonadPlus



Functor
- void :: Functor f => f a -> f ()

Applicative
- pure
- liftA2

Monad
- return :: a -> m a
- (>>=) :: m a -> (a -> m b) -> m b
- (>>) :: m a -> m b -> m b
- join
- mapM
- (>=>)
- (<=<)
- (<$!>)



## Control.Monad

```hs
fmap :: Functor f => (a -> b) -> f a -> f b
(<$) :: Functor f => a -> f b -> f a

when         :: Applicative f => Bool -> f () -> f ()
unless       :: Applicative f => Bool -> f () -> f ()

forever      :: Applicative f => f a -> f b
filterM      :: Applicative m => (a -> m Bool) -> [a] -> m [a]
mapAndUnzipM :: Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])

replicateM   :: Applicative m => Int -> m a -> m [a]
replicateM_  :: Applicative m => Int -> m a -> m ()

zipWithM     :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_    :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()

-- Control.Monad.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
(<=<)  :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>)  :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(=<<)  :: Monad m => (a -> m b) -> m a -> m b
ap     :: Monad m => m (a -> b) -> m a -> m b
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r

foldM  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a

fail   :: MonadFail m => forall a. String -> m a

(>>=)  :: Monad m => m a -> (a -> m b) -> m b
(>>)   :: Monad m => m a -> m b -> m b
return :: Monad m => a -> m a
join   :: Monad m => m (m a) -> m a
liftM  :: Monad m => (a1 -> r) -> m a1 -> m r

mapM_  :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM   :: (Functor t, Foldable t, Monad m) => (a -> m b) -> t a -> m (t b)

mzero :: (Alternative m, Monad m) => m a
mplus :: (Alternative m, Monad m) => m a -> m a -> m a

-- Data.Traversable.
forM_ :: (Foldable t,    Monad m) => t a -> (a -> m b) -> m ()
forM  :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

-- Data.Foldable.
msum      :: (Foldable t, MonadPlus m) => t (m a) -> m a
sequence  :: (Functor t, Foldable t) => Monad m => t (m a) -> m (t a)
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
```

## Control.Applicative

```hs
pure     :: Applicative f => a -> f a

optional :: Alternative f => f a -> f (Maybe a)

(<*>)    :: Applicative f => f (a -> b) -> f a -> f b
(*>)     :: Applicative f => f a -> f b -> f b
(<*)     :: Applicative f => f a -> f b -> f a
(<**>)   :: Applicative f => f a -> f (a -> b) -> f b

liftA    :: Applicative f => (a -> b) -> f a -> f b
liftA2   :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3   :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

empty :: Alternative f => f a
(<|>) :: Alternative f => f a -> f a -> f a
some  :: Alternative f => f a -> f [a]
many  :: Alternative f => f a -> f [a]
guard :: Alternative f => Bool -> f ()

-- Data.Foldable.
asum  :: (Foldable t, Alternative f) => t (f a) -> f a
```
