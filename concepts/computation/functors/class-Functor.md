# Functor

Functor
- Functor: overview
- Functor: ex/internal methods
- Functor: declaration
- Functor: minimal definition
- method interdefinitions
- additional methods
- in terms of…

## Functor: overview




## Functor: all functions

- `fmap`, `<$`; `<$>`, `$>`, `<&>`, `void`

```hs
fmap  :: Functor => (a -> b) -> f a -> f b
(<$>) :: Functor => (a -> b) -> f a -> f b
(<&>) :: Functor => f a -> (a -> b) -> f b
(<$)  :: Functor => a -> f b -> f a
($>)  :: Functor => f a -> b -> f b
void  :: Functor => f a -> f ()

(<$>) = fmap
(<&>) = flip fmap
(<$)  = fmap . const
($>)  = flip (fmap . const)
void  = fmap (const ())
```


## Functor: class declaration

```hs
class Functor f where
  -- | Mapping
  fmap :: (a -> b) -> f a -> f b

  -- | Replace all locations in the input with the same value.
  (<$) :: a -> f b -> f a
  (<$) = fmap . const
  infixl 4 <$
```

The `Functor` type class declares the interface that mappable structures may implement. The interface is based on a carrier type ctor `f :: * -> *`; it is a unary type ctor, and it also plays the role of the functorial mapping of objects, which in Haskell means mapping types to types. For example, when applied to a type `Int`, the type ctor yields the type `f Int`. As a more concrete example, the `Maybe` type ctor has a Functor instance. So the Maybe functor maps (1) types to types - it maps e.g. `Int` to Maybe `Int`, and (2) it maps arrows to arrows, which in Haskell means mapping of functions to functions. Being a functor, it has the capability to *lift* regular functions into functions that can be applied to map the entire structure. That is, if there is a function `a -> b` mapping values of type `a` into values of type `b`, then a functor can lift it into a function `f a -> f b` that can map the entire structure by mapping each element.

`Just` is a data ctor, but also a function that puts a value into the Maybe context, `Just :: a -> Maybe a`, e.g. `Just` acting on the integer 4 produces `Just 4 :: Maybe Int`. So the lifted `Just` function maps a list element-wise into a list of maybe values, `[1, 2] ⟼ [Just 1, Just 2]`, i.e. `fmap Just [1,2]` yields `[Just 1, Just 2]`.


The Functor type class also declares an extra method `<$`, which is used to replace all locations in the input with the same value, i.e. it is used to replace all elements in the structure with the same value. This is all it can do because it doesn't have a function to map the elements - it is only given a single value, so it can only replace the elements in the structure with that value.

```hs
fmap  :: (a -> b) -> f a -> f b
(<$>) :: (a -> b) -> f a -> f b
(<$)  ::       b  -> f a -> f b
```

The operator `<$` corresponds to the operator `<$>`, which in turn is just the operator form of `fmap`. However, while `<$>` (i.e. fmap) takes a function `a -> b` to function `f a -> f b`, the `<$` only has a constant which it applies to the struture by replacing all elements within.

## Functor: additional methods

```hs
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
infixl 4 <$>

-- | Flipped version of (<$)
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>

-- | Flipped version of fmap
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

-- | Discards or ignores the result of evaluation.
void :: Functor f => f a -> f ()
void x = () <$ x
```

- `<$>` is the infix form of fmap
- `<&>` is the flipped version of `<$>`
- `$>`  is the flipped version of `<$`
- `void` discards the the result


## Functor: in terms of

>All methods and auxillary functions of the `Functor` class are defined in terms of `fmap`. In fact, `fmap` is the only "proper" function there, all others are just convenience versions of fmap.

- `<$>`  fmap in infix operator form
- `<&>`  fmap in infix operator form, flipped
- `<$`   fmap that replaces everything with a constant
- `$>`   fmap that replaces everything with a constant, flipped
- `void` fmap that replaces everything with the constant value `()`
- flippedoff - amazingly there is no flipped version of 'void'



($>)  = flip (fmap . const)
void  = fmap (const ())




```hs
_x = fmap      succ    [1, 2]                   -- [2,3]
_x = liftA     succ    [1, 2]                   -- [0,0]
_x = liftM     succ    [1, 2]                   -- [0,0]
_x =            succ <$> [1, 2]                   -- [0,0]
_x = [1, 2] <&> succ                              -- [0,0]
_x = pure succ <*> [1,2]                 -- [0,0]
_x = [1,2] >>= \x -> return (succ x)     -- [0,0]
_x = [ succ x | x <- [1,2] ]             -- [0,0]
```

Save for arg flipping and other minutia, there are really only 2 interesting function in the Functor class, fmap and void.

Right-crippled fmap, i.e. `<$`, doesn't get a function to map a structure but only a value, `x`. We can just as well create a lambda function `(\_ -> x)`, aka `const x`, and give it to `fmap` to do the same job.

```hs
_x = 0 <$ [1, 2]                              -- [0,0]
_x = fmap (\_ -> 0) [1, 2]                    -- [0,0]
_x = pure (\x -> 0) <*> [1,2]                 -- [0,0]
_x = [1,2] >>= \x -> return ((\_ -> 0) x)     -- [0,0]
_x = [ (\_ -> 0) x | x <- [1,2] ]             -- [0,0]
```

`void` function is used to discard or ignore the result of evaluation.

```hs
void :: Functor f => f a -> f ()
-- void x = (fmap . const) () x
-- void   = (fmap . const) ()
void x = () <$ x

_x = 

```

### fmap: in terms of

`fmap` can be defined in terms of:
- Applicative's (<*>)
  - (<*>): fmap f xs = pure f <*> xs
- Monad's (>>=)
  - fmap f xs = xs >>= return . f
  - fmap f xs = xs >>= \x -> return (f x)



```hs
x1 = fmap (+9) [1,2]             -- [10,11]
x2 = [1,2] >>= return . (+9)     -- [10,11]
```

`fmap` is also equivalent to `lift`?
