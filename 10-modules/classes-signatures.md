# Type Classes: Signatures

Signatures of type classes:
- Show, Read
- Eq, Ord
- Bounded, Enum
- Num, Integral, Floating, Fractional
- Real, RealFrac, RealFloat
- Foldable
- Semigroup, Monoid
- Functor, Applicative, Monad
- Traversable


```hs
class Show a where
    show      :: a   -> String
    showList  :: [a] -> ShowS
    showsPrec :: Int -> a -> ShowS
-- MINIMAL: showsPrec | show

class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]
    GHC.Read.readPrec     :: Text.ParserCombinators.ReadPrec.ReadPrec a
    GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec [a]
-- MINIMAL: readsPrec | readPrec


class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
-- MINIMAL: (==) | (/=)

class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<)     :: a -> a -> Bool
    (<=)    :: a -> a -> Bool
    (>)     :: a -> a -> Bool
    (>=)    :: a -> a -> Bool
    max     :: a -> a -> a
    min     :: a -> a -> a
-- MINIMAL: compare | (<=)


class Bounded a where
    minBound :: a
    maxBound :: a
-- MINIMAL: minBound, maxBound

class Enum a where
    succ            :: a -> a
    pred            :: a -> a
    toEnum          :: Int -> a
    fromEnum        :: a -> Int
    enumFrom        :: a -> [a]
    enumFromThen    :: a -> a -> [a]
    enumFromTo      :: a -> a -> [a]
    enumFromThenTo  :: a -> a -> a -> [a]
-- MINIMAL: toEnum, fromEnum


class Num a where
    (+)    :: a -> a -> a
    (-)    :: a -> a -> a
    (*)    :: a -> a -> a
    abs    :: a -> a
    negate :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
-- MINIMAL: (+), (*), abs, signum, fromInteger, negate|(-)

class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a
-- MINIMAL: fromRational, (recip | (/))

class Fractional a => Floating a where
    pi      :: a
    exp     :: a -> a
    log     :: a -> a
    sqrt    :: a -> a
    (**)    :: a -> a -> a
    logBase :: a -> a -> a
    sin     :: a -> a
    cos     :: a -> a
    tan     :: a -> a
    asin    :: a -> a
    acos    :: a -> a
    atan    :: a -> a
    sinh    :: a -> a
    cosh    :: a -> a
    tanh    :: a -> a
    asinh   :: a -> a
    acosh   :: a -> a
    atanh   :: a -> a
    GHC.Float.log1p    :: a -> a
    GHC.Float.expm1    :: a -> a
    GHC.Float.log1pexp :: a -> a
    GHC.Float.log1mexp :: a -> a
-- pi, exp, log, sin , cos , asin , acos , atan,sinh, cosh, asinh, acosh, atanh

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational
-- MINIMAL: toRational

class (Real a, Enum a) => Integral a where
    quot        :: a -> a -> a
    rem         :: a -> a -> a
    div         :: a -> a -> a
    mod         :: a -> a -> a
    quotRem     :: a -> a -> (a, a)
    divMod      :: a -> a -> (a, a)
    toInteger   :: a -> Integer
-- MINIMAL: quotRem, toInteger

class (Real a, Fractional a) => RealFrac a where
    properFraction  :: Integral b => a -> (b, a)
    truncate        :: Integral b => a -> b
    round           :: Integral b => a -> b
    ceiling         :: Integral b => a -> b
    floor           :: Integral b => a -> b
-- MINIMAL: properFraction

class (RealFrac a, Floating a) => RealFloat a where
    scaleFloat     :: Int -> a -> a
    encodeFloat    :: Integer -> Int -> a
    decodeFloat    :: a -> (Integer, Int)
    floatRange     :: a -> (Int, Int)
    floatRadix     :: a -> Integer
    floatDigits    :: a -> Int
    exponent       :: a -> Int
    significand    :: a -> a
    atan2          :: a -> a -> a
    isNaN          :: a -> Bool
    isIEEE         :: a -> Bool
    isInfinite     :: a -> Bool
    isDenormalized :: a -> Bool
    isNegativeZero :: a -> Bool
-- MINIMAL: floatRadix, floatDigits, floatRange, decodeFloat, isNaN
-- encodeFloat, isInfinite, isDenormalized, isNegativeZero, isIEEE



class Foldable (t :: * -> *) where
    foldl                :: (b -> a -> b) -> b -> t a -> b
    foldr                :: (a -> b -> b) -> b -> t a -> b
    foldl1               :: (a -> a -> a) -> t a -> a
    foldr1               :: (a -> a -> a) -> t a -> a
    foldMap              :: Monoid m => (a -> m) -> t a -> m
    Data.Foldable.fold   :: Monoid m => t m -> m
    Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
    Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
    null                 :: t a -> Bool
    length               :: t a -> Int
    Data.Foldable.toList :: t a -> [a]
    maximum     :: Ord a => t a -> a
    minimum     :: Ord a => t a -> a
    sum         :: Num a => t a -> a
    product     :: Num a => t a -> a
    elem        :: Eq  a => a -> t a -> Bool
-- MINIMAL: foldMap | foldr

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
-- MINIMAL: mempty, mappend

class Functor (f :: * -> *) where
    fmap  :: (a -> b) -> f a -> f b
    (<$)  ::  a       -> f b -> f a
-- MINIMAL: fmap

class Functor f => Applicative (f :: * -> *) where
    pure  ::    a              -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (*>)  :: f  a       -> f b -> f b
    (<*)  :: f  a       -> f b -> f a
-- MINIMAL: pure, (<*>)

class Applicative m => Monad (m :: * -> *) where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a       -> m b  -> m b
    return ::   a               -> m a
    fail   :: String            -> m a
-- MINIMAL: (>>=)

class (Functor t, Foldable t) => Traversable (t :: * -> *) where
    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)
    mapM      :: Monad m => (a -> m b) -> t a -> m (t b)
    sequence  :: Monad m => t (m a) -> m (t a)
-- MINIMAL: traverse | sequenceA
```
