# Comonad :: Definition

Control.Comonad
https://hackage.haskell.org/package/comonad/docs/src/Control.Comonad.html

Just like with monads, there are two ways to define comonads
1. in terms of `extract` and `extend`
2. in terms of `fmap`, `extract` and `duplicate`

These two ways to define comonads are dual to the two ways to define monads: the first is preferred in Haskell, the second is preferred in category theory.




* the first way, in terms of `extract` and `extend`, is dual to defining monads the way Haskell does it, i.e. in terms of `return` (dual of `extract`) and `>>=` (dual of `extend`).

* the second way, in terms of `fmap`, `extract` and `duplicate`, is dual to defining comonads the way category theory does it, i.e. in terms of `return` (dual of `extract`) and `join` (dual of `duplicate`).





There are two ways to define a comonad:
- in terms 

1. Provide definitions for `extract` and `extend` satisfying the laws:

    extend extract      = id
    extract . extend f  = f
    extend f . extend g = extend (f . extend g)

In this case, you may simply set `fmap = liftW`.

These laws are directly analogous to the laws for monads and perhaps can be made clearer by viewing them as laws stating that *Cokleisli composition* must be associative, and has *extract* for a unit:

    f =>= extract   = f
    extract =>= f   = f
    (f =>= g) =>= h = f =>= (g =>= h)


2. Alternately, you may choose to provide definitions for `fmap`, `extract`, and `duplicate` satisfying these laws:

    extract . duplicate      = id
    fmap extract . duplicate = id
    duplicate . duplicate    = fmap duplicate . duplicate

In this case you may not rely on the ability to define fmap in terms of `liftW`.


3. In case you define comonads in terms of both `duplicate` and `extend`, you must also satisfy these laws:

    extend f  = fmap f . duplicate
    duplicate = extend id
    fmap f    = extend (f . extract)


These are the default definitions of `extend` and `duplicate`, and the definition of `liftW` respectively.


```hs
{-
There are two ways to define a comonad:

I. Provide definitions for 'extract' and 'extend'
satisfying these laws:

@
'extend' 'extract'      = 'id'
'extract' . 'extend' f  = f
'extend' f . 'extend' g = 'extend' (f . 'extend' g)
@

In this case, you may simply set 'fmap' = 'liftW'.

These laws are directly analogous to the laws for monads
and perhaps can be made clearer by viewing them as laws stating
that Cokleisli composition must be associative, and has extract for
a unit:

@
f '=>=' 'extract'   = f
'extract' '=>=' f   = f
(f '=>=' g) '=>=' h = f '=>=' (g '=>=' h)
@

II. Alternately, you may choose to provide definitions for 'fmap',
'extract', and 'duplicate' satisfying these laws:

@
'extract' . 'duplicate'      = 'id'
'fmap' 'extract' . 'duplicate' = 'id'
'duplicate' . 'duplicate'    = 'fmap' 'duplicate' . 'duplicate'
@

In this case you may not rely on the ability to define 'fmap' in
terms of 'liftW'.

You may of course, choose to define both 'duplicate' /and/ 'extend'.
In that case you must also satisfy these laws:

@
'extend' f  = 'fmap' f . 'duplicate'
'duplicate' = 'extend' id
'fmap' f    = 'extend' (f . 'extract')
@

These are the default definitions of 'extend' and 'duplicate' and
the definition of 'liftW' respectively.

-}

class Functor w => Comonad w where
  -- |
  -- @
  -- 'extract' . 'fmap' f = f . 'extract'
  -- @
  extract :: w a -> a

  -- |
  -- @
  -- 'duplicate' = 'extend' 'id'
  -- 'fmap' ('fmap' f) . 'duplicate' = 'duplicate' . 'fmap' f
  -- @
  duplicate :: w a -> w (w a)
  duplicate = extend id

  -- |
  -- @
  -- 'extend' f = 'fmap' f . 'duplicate'
  -- @
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

  {-# MINIMAL extract, (duplicate | extend) #-}



infixl 4 <@, @>, <@@>, <@>
infixl 1 =>>
infixr 1 <<=, =<=, =>=

-- see impls at:
-- https://hackage.haskell.org/package/comonad-5.0.8/docs/src/Control.Comonad.html#Comonad

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
