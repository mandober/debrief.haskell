# Haskell and CT :: Monad Homomorphisms

Monad Homomorphisms - Edward Kmett @ZuriHac2016
https://www.youtube.com/watch?v=YTaNkWjd-ac
https://wiki.haskell.org/ZuriHac2016
https://github.com/zfoh/HaskellerZ#2016-meetups

Abstract: The way that we use the monad transformer library today leads to code that has pathological performance problems. Can we do better?

## MonadTrans class

A simple way to define the class `MonadTrans` is:

```hs
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

or, in a more safer way as:

```hs
class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a
```

With the 2 laws:

```hs
lift (return a) == return a
-- sort of distributivity:
lift (m >>= f)  == lift m >>= lift . f
```

`lift` is a monad-structure-preserving natural transformation.


## Control.Monad.Morph

https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html

A **monad morphism** is a natural transformation:

```hs
morph :: forall a . m a -> n a

-- as an arrow
type m ~> n = forall a . m a -> n a
```

that obeys the following two laws:

```hs
morph (return x) = return x

morph $ do x <- m  =  do x <- morph m
           f x           morph (f x)
```

which are equivalent to the following two functor laws:

```hs
morph . return = return
morph . (f >=> g) = morph . f >=> morph . g
```

Existing well-known monad homomorphisms:
- `lift`
- `squash`
- `hoist f`, if f is a monad morphism
- `(f . g)`, if f and g are both monad morphisms
- `id`


```hs
type m ~> n = forall a . m a -> n a

-- Examples of monad morphisms include:

-- monic unless t is terminal
lift :: (MonadTrans t, Monad m) => m ~> t m

-- monic unless m is terminal
liftIO :: MonadIO m => IO ~> m

-- monad iso with inverse stToIO
ioToST :: IO ~> ST RealWorld

-- monad iso with inverse ioToST
stToIO :: ST RealWorld ~> IO
```

Monad morphisms commonly arise when manipulating existing monad transformer code for compatibility purposes. The `MFunctor`, `MonadTrans`, and `MMonad` classes define standard ways to change monad transformer stacks:
- `lift` introduces a new monad transformer layer of any type
- `squash` flattens two identical mt layers into a single one of the same type
- `hoist` maps monad morphisms to modify deeper layers of the mt stack
