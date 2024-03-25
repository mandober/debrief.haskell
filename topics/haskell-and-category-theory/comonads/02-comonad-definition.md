# Comonad definition

Control.Comonad
https://hackage.haskell.org/package/comonad/docs/src/Control.Comonad.html

In Haskell, comonads are defined as a class, `Comonad`, that is a subclass of the `Functor` class. This is similar to monads, except there's the class of `Applicative` functors there, between the `Functor` and `Monad` class - meaning Monad is a subclass of Applicative, which in turn is a subclass of Functor. However, it amounts to the same thing - every monad is a functor, just like every comonad is, which justifies the availability of `fmap`.

Comonads and monads are duals, so just like there are two ways to define monads, there are two ways to define comonads:
1. in terms of `extract` and `extend`
2. in terms of `fmap`, `extract` and `duplicate`

The first way is preferred in Haskell, while the second is preferred in category theory. Each comonadic method has a dual monadic method:
- `return` (`η`) is dual of `extract` (`ϵ`)
- `join` (`μ`) is dual of `duplicate` (`δ`)
- `>>=` (bind) is dual of `extend`

In CT, the methods with greek names in parens are NTs.


## Comonad as `extract` and `extend`

Provide definitions for `extract` and `extend` satisfying the laws:

```hs
extend extract      == id                       -- (1)
extract . extend f  == f                        -- (2)
extend f . extend g == extend (f . extend g)    -- (3)
```

In this case, you can define `fmap` as `liftW` if you didn't alreadydefine a Functor instance for your comonad.

These laws are directly analogous to the laws for monads. They may be made clearer by viewing them as laws stating that *Cokleisli composition*, (`=>=`), must be associative and has `extract` as (left and right) unit:

```hs
f =>= extract   == f                  -- LEFT_UNIT
extract =>= f   == f                  -- RIGHT_UNIT
(f =>= g) =>= h == f =>= (g =>= h)    -- ASSOC
```


## Comonad as `extract`, and `duplicate`

Provide definitions for `extract` and `duplicate` that satisfy these laws:

```hs
extract . duplicate      == id
fmap extract . duplicate == id
duplicate . duplicate    == fmap duplicate . duplicate
```

In this case you cannot define `fmap` in terms of `liftW`, in fact, you must have already defined a Functor instance for your comonadm, so `fmap` should already be available.


## Comonad as both `duplicate` and `extend`

In case you define comonads in terms of both `duplicate` and `extend` (and `extract` naturally), you must also make sure these laws are satisfied:

```hs
extend f  == fmap f . duplicate
duplicate == extend id
fmap f    == extend (f . extract)
```

These are the default definitions of `extend` and `duplicate`, and the definition of `liftW` respectively.


```hs
class Functor w => Comonad w where
  -- 'extract' . 'fmap' f = f . 'extract'
  extract :: w a -> a

  -- 'duplicate' = 'extend' 'id'
  -- 'fmap' ('fmap' f) . 'duplicate' = 'duplicate' . 'fmap' f
  duplicate :: w a -> w (w a)
  duplicate = extend id

  -- 'extend' f = 'fmap' f . 'duplicate'
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

  {-# MINIMAL extract, (duplicate | extend) #-}
```




```hs
-- https://hackage.haskell.org/package/comonad/docs/src/Control.Comonad.html

infixl 4 <@, @>, <@@>, <@>
infixl 1 =>>
infixr 1 <<=, =<=, =>=


instance Comonad ((,)e) where
instance Monoid m => Comonad ((->) m) where
instance Comonad Identity where
instance Comonad w => Comonad (IdentityT w) where
instance Comonad Tree where
instance Comonad NonEmpty where
instance (Comonad f, Comonad g) => Comonad (FSum.Sum f g) where

coproduct :: (f a -> b) -> (g a -> b) -> FSum.Sum f g a -> b
```

`ComonadApply` is to `Comonad` as `Applicative` is to `Monad`.

Mathematically, it is a *strong lax symmetric semi-monoidal comonad* on the category *Hask* of Haskell types.

That is, `w` is a strong lax symmetric semi-monoidal functor on Hask, where both `extract` and `duplicate` are *symmetric monoidal natural transformations*.


## The Laws

1. The laws

    (.) <$> u <@> v <@> w = u <@> (v <@> w)
        extract (p <@> q) = extract p (extract q)
      duplicate (p <@> q) = (<@>) <$> duplicate p <@> duplicate q


2. If the type is both a `ComonadApply` and `Applicative` we further require:

    (<*>) = (<@>)


3. Finally, if you choose to define (`<@`) and (`@>`), the results of your definitions should match the following laws:

    a @> b = const id <$> a <@> b
    a <@ b = const <$> a <@> b




```hs
-- | @ComonadApply@ is to @Comonad@ like @Applicative@ is to @Monad@.
--
-- Mathematically, it is a strong lax symmetric semi-monoidal comonad on the
-- category @Hask@ of Haskell types. That it to say that @w@ is a strong lax
-- symmetric semi-monoidal functor on Hask, where both 'extract' and 'duplicate' are
-- symmetric monoidal natural transformations.
--
-- Laws:
--
-- @
-- ('.') '<$>' u '<@>' v '<@>' w = u '<@>' (v '<@>' w)
-- 'extract' (p '<@>' q) = 'extract' p ('extract' q)
-- 'duplicate' (p '<@>' q) = ('<@>') '<$>' 'duplicate' p '<@>' 'duplicate' q
-- @
--
-- If our type is both a 'ComonadApply' and 'Applicative' we further require
--
-- @
-- ('<*>') = ('<@>')
-- @
--
-- Finally, if you choose to define ('<@') and ('@>'), the results of your
-- definitions should match the following laws:
--
-- @
-- a '@>' b = 'const' 'id' '<$>' a '<@>' b
-- a '<@' b = 'const' '<$>' a '<@>' b
-- @

class Comonad w => ComonadApply w where
  (<@>) :: w (a -> b) -> w a -> w b
  default (<@>) :: Applicative w => w (a -> b) -> w a -> w b
  (<@>) = (<*>)

  (@>) :: w a -> w b -> w b
  a @> b = const id <$> a <@> b

  (<@) :: w a -> w b -> w a
  a <@ b = const <$> a <@> b

instance Semigroup m => ComonadApply ((,)m) where
  (m, f) <@> (n, a) = (m <> n, f a)
  (m, a) <@  (n, _) = (m <> n, a)
  (m, _)  @> (n, b) = (m <> n, b)

instance ComonadApply NonEmpty where
  (<@>) = ap

instance Monoid m => ComonadApply ((->)m) where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)

instance ComonadApply Identity where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)

instance ComonadApply w => ComonadApply (IdentityT w) where
  IdentityT wa <@> IdentityT wb = IdentityT (wa <@> wb)

instance ComonadApply Tree where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)


-- | A suitable default definition for 'fmap' for a 'Comonad'.
-- Promotes a function to a comonad.
--
-- You can only safely use 'liftW' to define 'fmap' if your 'Comonad'
-- defines 'extend', not just 'duplicate', since defining
-- 'extend' in terms of duplicate uses 'fmap'!
--
-- @
-- 'fmap' f = 'liftW' f = 'extend' (f . 'extract')
-- @
liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)


-- | Comonadic fixed point à la David Menendez
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (extend wfix w)


-- | Comonadic fixed point à la Dominic Orchard
cfix :: Comonad w => (w a -> a) -> w a
cfix f = fix (extend f)


-- | Comonadic fixed point à la Kenneth Foner:
--
-- This is the `evaluate` function from his talk:
-- https://www.youtube.com/watch?v=F7F-BzOB670 "Getting a Quick Fix on Comonads
--
kfix :: ComonadApply w => w (w a -> a) -> w a
kfix w = fix $ \u -> w <@> duplicate u

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

-- | 'extend' in operator form
(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend

-- | Right-to-left 'Cokleisli' composition
(=<=) :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
f =<= g = f . extend g

-- | Left-to-right 'Cokleisli' composition
(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
f =>= g = g . extend f

-- | A variant of '<@>' with the arguments reversed.
(<@@>) :: ComonadApply w => w a -> w (a -> b) -> w b
(<@@>) = liftW2 (flip id)

-- | Lift a binary function into a 'Comonad' with zipping
liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW2 f a b = f <$> a <@> b

-- | Lift a ternary function into a 'Comonad' with zipping
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 f a b c = f <$> a <@> b <@> c


-- | The 'Cokleisli' 'Arrow's of a given 'Comonad'
newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }
-- #if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
-- #else
-- #ifdef __GLASGOW_HASKELL__
instance Typeable1 w => Typeable2 (Cokleisli w) where
  typeOf2 twab = mkTyConApp cokleisliTyCon [typeOf1 (wa twab)]
        where wa :: Cokleisli w a b -> w a
              wa = undefined


cokleisliTyCon :: TyCon
cokleisliTyCon = mkTyCon3 "comonad" "Control.Comonad" "Cokleisli"


instance Comonad w => Category (Cokleisli w) where
  id = Cokleisli extract
  Cokleisli f . Cokleisli g = Cokleisli (f =<= g)

instance Comonad w => Arrow (Cokleisli w) where
  arr f = Cokleisli (f . extract)
  first f = f *** id
  second f = id *** f
  Cokleisli f *** Cokleisli g = Cokleisli (f . fmap fst &&& g . fmap snd)
  Cokleisli f &&& Cokleisli g = Cokleisli (f &&& g)

instance Comonad w => ArrowApply (Cokleisli w) where
  app = Cokleisli $ \w -> runCokleisli (fst (extract w)) (snd <$> w)

instance Comonad w => ArrowChoice (Cokleisli w) where
  left = leftApp

instance ComonadApply w => ArrowLoop (Cokleisli w) where
  loop (Cokleisli f) = Cokleisli (fst . wfix . extend f') where
    f' wa wb = f ((,) <$> wa <@> (snd <$> wb))

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)

instance Applicative (Cokleisli w a) where
  pure = Cokleisli . const
  Cokleisli f <*> Cokleisli a = Cokleisli (\w -> f w (a w))

instance Monad (Cokleisli w a) where
  return = pure
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w



infixl 4 $>

-- | Replace the contents of a functor uniformly with a constant value.
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
```
