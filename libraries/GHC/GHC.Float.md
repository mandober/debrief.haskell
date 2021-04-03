# GHC.Float


```hs
-- imported via GHC.Float

data FFFormat = ...E

FFFixed     :: FFFormat
FFExponent  :: FFFormat
FFGeneric   :: FFFormat

expFloat    :: Float -> Float
gtFloat     :: Float -> Float -> Bool
geFloat     :: Float -> Float -> Bool
leFloat     :: Float -> Float -> Bool
ltFloat     :: Float -> Float -> Bool
eqFloat     :: Float -> Float -> Bool
acosFloat   :: Float -> Float
acoshFloat  :: Float -> Float
asinFloat   :: Float -> Float
asinhFloat  :: Float -> Float
atanFloat   :: Float -> Float
atanhFloat  :: Float -> Float
cosFloat    :: Float -> Float
coshFloat   :: Float -> Float
minusFloat  :: Float -> Float -> Float
plusFloat   :: Float -> Float -> Float
powerFloat  :: Float -> Float -> Float
timesFloat  :: Float -> Float -> Float
sinFloat    :: Float -> Float
sinhFloat   :: Float -> Float
negateFloat :: Float -> Float
sqrtFloat   :: Float -> Float

isFloatNaN           :: Float -> Int
isFloatFinite        :: Float -> Int
isFloatInfinite      :: Float -> Int
isFloatDenormalized  :: Float -> Int
isFloatNegativeZero  :: Float -> Int

isDoubleNaN          :: Double -> Int
isDoubleFinite       :: Double -> Int
isDoubleInfinite     :: Double -> Int
isDoubleDenormalized :: Double -> Int
isDoubleNegativeZero :: Double -> Int

expDouble   :: Double -> Double
sinDouble   :: Double -> Double
sinhDouble  :: Double -> Double
negateDouble:: Double -> Double
sqrtDouble  :: Double -> Double
eqDouble    :: Double -> Double -> Bool
leDouble    :: Double -> Double -> Bool
ltDouble    :: Double -> Double -> Bool
geDouble    :: Double -> Double -> Bool
gtDouble    :: Double -> Double -> Bool
acosDouble  :: Double -> Double
acoshDouble :: Double -> Double
asinDouble  :: Double -> Double
asinhDouble :: Double -> Double
atanDouble  :: Double -> Double
atanhDouble :: Double -> Double
cosDouble   :: Double -> Double
coshDouble  :: Double -> Double
timesDouble :: Double -> Double -> Double
minusDouble :: Double -> Double -> Double
plusDouble  :: Double -> Double -> Double
powerDouble :: Double -> Double -> Double

castDoubleToWord64 :: Double -> GHC.Word.Word64
castFloatToWord32  :: Float -> GHC.Word.Word32
castWord32ToFloat  :: GHC.Word.Word32 -> Float
castWord64ToDouble :: GHC.Word.Word64 -> Double

clamp :: Int -> Int -> Int

divideDouble    :: Double -> Double -> Double
divideFloat     :: Float -> Float -> Float
double2Float    :: Double -> Float

expm1       :: Floating a => a -> a
expm1Double :: Double -> Double
expm1Float  :: Float -> Float
expt        :: Integer -> Int -> Integer
expts       :: GHC.Arr.Array Int Integer
expts10     :: GHC.Arr.Array Int Integer

fabsDouble :: Double -> Double
fabsFloat :: Float -> Float

float2Double  :: Float -> Double
floatToDigits :: RealFloat a => Integer -> a -> ([Int], Int)

formatRealFloat :: RealFloat a => FFFormat -> Maybe Int -> a -> String
formatRealFloatAlt :: RealFloat a 
                   => FFFormat -> Maybe Int -> Bool -> a -> String

fromRat   :: RealFloat a => Rational -> a
fromRat'  :: RealFloat a => Rational -> a
fromRat'' :: RealFloat a => Int -> Int -> Integer -> Integer -> a

rationalToDouble :: Integer -> Integer -> Double
rationalToFloat  :: Integer -> Integer -> Float


integerLogBase       :: Integer -> Integer -> Int
logFloat             :: Float -> Float
log1pFloat           :: Float -> Float
logDouble            :: Double -> Double
log1pDouble          :: Double -> Double
log1p                :: Floating a => a -> a
log1mexp             :: Floating a => a -> a
log1pexp             :: Floating a => a -> a

maxExpt   :: Int
minExpt   :: Int
maxExpt10 :: Int

roundTo :: Int -> Int -> [Int] -> (Int, [Int])

showFloat   :: RealFloat a => a -> ShowS
showSignedFloat :: RealFloat a => (a -> ShowS) -> Int -> a -> ShowS

stgDoubleToWord64 :: Double# -> GHC.Prim.Word#
stgFloatToWord32 :: Float# -> GHC.Prim.Word#
stgWord32ToFloat :: GHC.Prim.Word# -> Float#
stgWord64ToDouble :: GHC.Prim.Word# -> Double#

tanDouble :: Double -> Double
tanFloat :: Float -> Float
tanhDouble :: Double -> Double
tanhFloat :: Float -> Float

word2Double :: Word -> Double
word2Float :: Word -> Float
double2Int :: Double -> Int
float2Int :: Float -> Int
int2Double :: Int -> Double
int2Float :: Int -> Float

D# :: Double# -> Double
data Double# :: TYPE 'GHC.Types.DoubleRep

F# :: Float# -> Float
data Float# :: TYPE 'GHC.Types.FloatRep

-- imported via GHC.Float, Prelude
class Fractional a => Floating a
class (RealFrac a, Floating a) => RealFloat a

pi      :: Floating a => a
(**)    :: Floating a => a -> a -> a
sqrt    :: Floating a => a -> a
exp     :: Floating a => a -> a
log     :: Floating a => a -> a
logBase :: Floating a => a -> a -> a

acos  :: Floating a => a -> a
acosh :: Floating a => a -> a
asin  :: Floating a => a -> a
asinh :: Floating a => a -> a
cos   :: Floating a => a -> a
cosh  :: Floating a => a -> a
sin   :: Floating a => a -> a
sinh  :: Floating a => a -> a
tan   :: Floating a => a -> a
tanh  :: Floating a => a -> a
atan  :: Floating a => a -> a
atanh :: Floating a => a -> a
atan2 :: RealFloat a => a -> a -> a

decodeFloat     :: RealFloat a => a -> (Integer, Int)
encodeFloat     :: RealFloat a => Integer -> Int -> a
significand     :: RealFloat a => a -> a
exponent        :: RealFloat a => a -> Int
floatDigits     :: RealFloat a => a -> Int
floatRadix      :: RealFloat a => a -> Integer
floatRange      :: RealFloat a => a -> (Int, Int)
scaleFloat      :: RealFloat a => Int -> a -> a
isNaN           :: RealFloat a => a -> Bool
isIEEE          :: RealFloat a => a -> Bool
isInfinite      :: RealFloat a => a -> Bool
isDenormalized  :: RealFloat a => a -> Bool
isNegativeZero  :: RealFloat a => a -> Bool

data Double = ...E
data Float  = ...E
```
