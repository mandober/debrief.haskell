# Monad transformers

- https://wiki.haskell.org/Monad_Transformers_Explained
- https://wiki.haskell.org/Monad_Transformers_Tutorial
- https://en.wikibooks.org/wiki/Haskell/Monad_transformers

Content
- IdentityT
- StateT
- WriterT
- ExceptT
- ReaderT


## Intro

Having a single monad means working within its effects only. However, if we need to combine the effects of several monads, we need a better solution than to make ad hoc monadic combinations.

For example, if we need to combine the State monad and Maybe monad, we can implement a suitable monad corresponding to their combination from scratch:

```hs
-- State + Maybe: should it be like this:
newtype StateMaybe s a = StateMaybe (s -> Maybe (a, s))

-- State + Maybe: or perhaps like this?
newtype StateMaybe s a = StateMaybe (s -> (Maybe a, s))
```

but doing so for all the different combinations of monads is unwieldy. Also, it is not always obvious how to combine monads, e.g. how to combine State, Error and CPS monads.

Monad Transformers can help with this. A monad transformer transforms the base monad by combining it with other monads, thereby aggregating support for additional effects.

A library of monad transformers can be developed, each monad adding its specific effect (Maybe, List, Reader, Writer, State, RSW, Except, Cont, etc.), allowing the programmer to mix and match. This is a form of aspect-oriented programming.

A monad transformer maps monads to monads. This is represented by a type-ctor of the kind `(* -> *) -> * -> *` == `(* -> *) -> (* -> *)`. Moreover, we require monad transformers to aggregate computational effects. Thus we require a mapping from computations in the underlying monad to computations in the transformed monad. These requirements are captured by the following (multi-parameter) type class:

```hs
class (Monad m, Monad (t m)) => MonadTrans t m where
  lift :: m a -> t m a
```

A monad transformer adds specific effects to any monad. Thus there can be many monads supporting the same operations. We can introduce additional classes to handle the overloading:

```hs
class (Monad m) => MonadError m where
  fail   :: m a
  handle :: m a -> m a -> m a

class (Monad m) => MonadState m s | m -> s where
  set :: s -> m ()
  get :: m s
```

Usually there is a base monad (`Identity` or `IO`) onto which the other monads are added.

```hs
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return a = Identity a
  m >>= f = f $ runIdentity m

-- ErrorT monad transformer
newtype ErrorT m a = ErrorT { runErrorT :: m (Maybe a) }

instance Monad m => Monad (ErrorT m) where
  return 

-- Error monad
type Error a = ErrorT Identity a
```
