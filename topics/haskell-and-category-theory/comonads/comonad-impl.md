# Comonads


Just like with monads, we define the class of comonads - this parallel presentation shows the alternate, categorical, definition of monads (with the `join` operation).

Monadic `return`, that pushes a value into a monadic context, is paralleled with `extract`, acting as `return`'s inverse, as it pulls a value from the context.

```hs
class Functor m => Monad m where
  return :: a -> m a
  join   :: m (m a) -> m a

class Functor w => Comonad w where
  extract   :: w a -> a
  duplicate :: w a -> w (w a)
```

Monads are at the base functors and comonads are cofunctors, but since a functor is its own cofunctor (since they are the same, we just call them functors), the `Functor` class is the superclass of both. However, there is a comonadic analog of the `Applicative` class, which is called `Apply`. Anyway, being functors, means we are welcome to the `fmap` function with comonads as well.


## Comonad class

```hs
class Functor w => Comonad w where
  -- | Extracts the focused element from a structure.
  extract :: w a -> a
  -- return  :: a -> m a
  -- extract :: m a <- a

  -- | All possible VIEWS into a structure.
  -- Fills each slot with a copy of the structure viewed form that position.
  -- It helps us lift a query into a mutation (contextually altered structure).
  duplicate :: w a -> w (w a)
  -- join      :: m (m a) -> m a
  -- duplicate :: m (m a) <- m a

  extend :: (w a -> b) -> w a -> w b
  -- (=<<)  :: (a -> m b) -> (m a -> m b)
  -- extend :: (a <- m b) -> (m a <- m b)

{-# MINIMAL extract, (duplicate | extend) #-}
```



As the `MINIMAL` pragma suggest, `duplicate` and `extend` can be defined in terms of each other (that is, it is sufficient to define only one), similarly to the monadic `join` and `bind` operations. In fact, `extend` is the opposite of `>>=`, more precisely, of its twin with the args flipped, `=<<`.

```hs
(>>=)  :: m a -> (a -> m b) -> m b
(=<<)  :: (a -> m b) ->  m a -> m b   -- flip args to get (=<<)
(=<<)  :: (a -> m b) -> (m a -> m b)  -- note the implicit parens!
extend :: (a <- m b) -> (m a <- m b)  -- reverse the internal arrows
extend :: (m b -> a) -> (m b -> m a)  -- canonicalize the arrow direction
extend :: (w b -> a) -> (w b -> w a)  -- change m into w
extend :: (w a -> b) -> (w a -> w b)  -- relabel the type params
extend :: (w a -> b) ->  w a -> w b   -- so the first param is 'a'
```




## Views, queries and coeffects

When dealing with comonads, we often work with an infinite structure (like a stream) that has some notion of positions (indices) and slots (cells) to store data. We talk about the *currect view* into a structure, i.e. the *currently focused slot*.


## Comonad laws

https://hackage.haskell.org/package/comonad-5.0.8/docs/src/Control.Comonad.html#Comonad

```js
extract  .  duplicate = id                                        (1)
extract <$> duplicate = id                                        (2)
duplicate . duplicate = duplicate <$> duplicate                   (3)
```


1. The original view into the structure must be stored in the focused slot.

```js
extract . duplicate = id                                          (1)
extract (duplicate s) = s                                         (1)

extract . fmap f   = f . extract                                  (1)
extract (fmap f s) = f (extract s)                                (1)
```


2. After duplicating, the FOCUSED SLOT of each VIEW must match the SLOT that view is stored in. (Comands are functors, hence we can use fmap).

```js
fmap extract (duplicate s) = s                                    (2)
extract <$> duplicate = id                                        (2)
```


3. Equality BETWEEN duplicating the outside of the structure than duplicating it again, AND duplicating the inside of the structure than duplicating it again.

```js
duplicate (duplicate s) = fmap duplicate (duplicate s)            (3)
duplicate . duplicate = duplicate <$> duplicate                   (3)
```




```hs
extract . fmap f = f . extract                  (1)

duplicate = extend id                           (2)
fmap (fmap f) . duplicate = duplicate . fmap f  (2)

extend f = fmap f . duplicate                   (3)
```





## Refs

Comonads by Example - Chris Penner
Monadic Party 2019 (4 videos)
https://www.youtube.com/watch?v=HOmOQnQGtPU&list=PLcAu_kKy-krxDD1WwRX_9rc0knAFK3nHs&index=8
