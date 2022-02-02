# Index of monad transformers

By default, monad transformers are located in the module `Control.Monad.Trans`.

- `AccumT`    
- `ContT`     continuation monad transformer
- `ExceptT`   
- `IdentityT` 
- `ListT`     nondeterminism monad transformer
- `MaybeT`    
- `RandomT`   
- `ReaderT`   
- `RWST`      
- `SelectT`   Selection monad transformer, modelling search algorithms
- `StateT`    
- `WriterT`   


## Desc of monad transformers

## SelectT

SelectT - selection monad transformer

https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Select.html

- module: `Control.Monad.Trans.Select`
- precursor monad: `Select`
- monad transformer: `SelectT`
- newtype: `SelectT r m a = SelectT ((a -> m r) -> m a)`

`SelectT` is not a functor on the category of monads, and many operations cannot be lifted through it.

```hs
-- Selection monad transformer
type role SelectT nominal representational nominal
type      SelectT :: Type -> (Type -> Type) -> Type -> Type
newtype   SelectT r m a = SelectT ((a -> m r) -> m a)

-- Selection monad
type Select r = SelectT r Identity

-- | Constructor for computations in the selection monad.
select :: ((a -> r) -> a) -> Select r a
select f = SelectT $ \ k -> Identity (f (runIdentity . k))

-- | Runs a SelectT computation with a function for evaluating answers
-- to select a particular answer. It is the inverse of 'select'.
runSelectT :: SelectT r m a -> (a -> m r) -> m a
runSelectT (SelectT g) = g

-- | Apply a function to transform the result of a selection computation. This has a more restricted type than the map operations for other monad transformers because SelectT doesn:t define a functor in the category of monads.
--
-- runSelectT (mapSelectT f m) = f . runSelectT m
--
mapSelectT :: (m a -> m a) -> SelectT r m a -> SelectT r m a
mapSelectT f m = SelectT $ f . SelectT r m a -> (a -> m r) -> m a
forall r (m :: * -> *) a. SelectT r m a -> (a -> m r) -> m a
runSelectT m

-- | Convert a selection computation to a continuation-passing computation.
selectToContT :: (Monad m) => SelectT r m a -> ContT r m a
selectToContT (SelectT g) = ContT $ \ k -> g k >>= k
```


Papers about selection monad transformer, modelling search algorithms:
* `Selection functions, bar recursion and backward induction`, 
  Martin Escardo and Paulo Oliva, 2010   
  https://www.cs.bham.ac.uk/~mhe/papers/selection-escardo-oliva.pdf

* `Monad transformers for backtracking search`, Jules Hedges, 2004   
  https://arxiv.org/abs/1406.2058
