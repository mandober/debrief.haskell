# Reader

## Overview

- Computation type: Computations which read values from a shared environment.
- Binding strategy: Monad values are functions from the environment to a value. The bound function is applied to the bound value, and both have access to the shared environment.
- Useful for: Maintaining variable bindings, or other shared environment.
- Zero and plus: None
- Example type: `Reader [(String,Value)] a`

## Motivation

Some programming problems require a computation to reference a shared environment, such as a set of variable bindings (symbols table). These computations typically read values from a shared environment and sometimes execute sub-computations in an environment that is modified on-the-fly, e.g. with new bindings or with bindings that temporary shadow existing bindings. However, such computations don't require the full generality of the `State` monad. The `Reader` monad is specifically designed for this use pattern and is often a clearer and easier mechanism than using the `State` monad.

## Definition

The `Reader` data type is just a newtype wrapper for a `r -> a` function type. The input type has the `r` type var as a convention - it marks the type (var) that is fixed, while the usual `a` type (var) marks a type that varies. This is reflected in the `Reader` type that reads from a fixed environment `r` and returns some type `a`. The environment is hardly ever a base type, most often it is a mapping of sorts, e.g. [(k,v)]. Taking this example type to be `r`, the returned type can be, perhaps, `v`, i.e. a type of values in a key/value mapping. In such a situation, a reader is a function of type,

```hs
reader :: r -> a        -- general
reader :: [(k,v)] -> v  -- reading a k/v map and returning a v
```

In order to make this type an instance of the FAM classes, we need to wrap it in a newtype to obtain the type `Reader r a`.

The FAM classes take a unary type ctor, `f :: * -> *`, and the `Reader` type ctor itself has kind `Reader :: * -> * -> *`, so we need to partially apply the type ctor, hence fixing the input type arg, `r`, and obtaining `Reader r` type ctor that expects another type to be fully saturated. So `Reader r :: * -> *` will be the target type for the impl of FAM methods.

```hs
newtype Reader r a = Reader { runReader :: r -> a }
```

Actually, the `Reader` type is expressed in terms of the `ReaderT` monad transformer with `Identity` monad substituted for the `m` type parameter:

```hs
import Data.Functor.Identity (Identity(..))

-- ReaderT monad transf
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- Reader in terms of ReaderT
type Reader r a = ReaderT r Identity a
```

FAM instances

```hs
instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader $ \r -> a

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader r >>= f = Reader $ \e -> f (r e) e
```
