# Function signatures

1. Sig in example (1) denotes the most general type of function. Even though, the `a` could be any basic like `Int` or even a function type like `(a -> b)`, it is (saved for lang extensions) still limited to the types of kind `*`. Types like `Maybe a` of kind: `* -> *`, or `Either a b` of kind: `* -> * -> *` may not be used in place of `a` (or `b`).

2. In example (2) we immediately know that foo is a binary function. arg1 is of some type `a` and arg2 is a unary function from `a` to `b`. This tells us that arg2 i.e. a unary function will probably receive the arg1 and it is expected to produce a value of the type `b` which may as well be the same as `a` (but need not be). Seeing that foo returns (a value of) type `b`, we can be pretty sure that the `b` the unary function returned will just be forwarded as foo's return as well. This means the foo function itself does very little, it acts more as a conductor over its arguments.

3. Signature in the example (3) would disallow input and output types to differ.
4. Signature in the example (4) doesn't tell us much. It makes no sense if we consider foo as a "conductor" function.

5. bind

`>>=`:

- arg1: m a          m a          m a
          ↓            ↓            ↓
- arg2: ( a -> m b) | (a -> b) | m (a -> b)
                ↓   |       ↓  |         ↓
- ret:         m b  |     m b  |       m b

semi-normal func that accepts plain `a` and produces `m b`, which is a wrapped value of other type, forwarded as return of bind


6. fmap, `<$>` 

```hs
general :: a -> b          -- (1)
foo :: a -> (a -> b) -> b  -- (2)
foo :: a -> (a -> a) -> a  -- (3)
foo :: a -> (b -> c) -> d  -- (4)

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) = flip (>>=)
(=<<) :: Monad m => (a -> m b) -> m a -> m b

(<$>) :: Functor f => (a -> b) -> f a -> f b

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```





# Signatures

```hs
(+)  :: Num a => a -> a -> a            -- GHC.Num   infixl 6
(-)  :: Num a => a -> a -> a            -- GHC.Num   infixl 6
(*)  :: Num a => a -> a -> a            -- GHC.Num   infixl 7
(**) :: Floating a => a -> a -> a       -- GHC.Float infixr 8
(/)  :: Fractional a => a -> a -> a     -- GHC.Real infixl 7
(/=) :: Eq a => a -> a -> Bool          -- ghc-prim-0.5.3:GHC.Classes infix 4
(!!) :: [a] -> Int -> a                 -- GHC.List
(.)  :: (b -> c) -> (a -> b) -> a -> c  -- GHC.Base infixr 9
(=<<):: Monad m => (a -> m b) -> m a -> m b
(*>) :: Applicative f => f a -> f b -> f b  -- GHC.Base infixl 4

(^) :: (Num a, Integral b) => a -> b -> a  -- GHC.Real, infixr 8
(^^) :: (Fractional a, Integral b) => a -> b -> a -- GHC.Real, infixr 8
(**) :: Floating a => a -> a -> a     -- GHC.Float, infixr 8

```

```hs
      map  ::                      (a ->   b)  -> [a]           -> [b]
     (<$>) :: (Functor     f) =>   (a ->   b)  -> f a           -> f b
     (<*>) :: (Applicative f) => f (a ->   b)  -> f a           -> f b
flip (<$>) :: (Functor     f) => f a           ->   (a ->   b)  -> f b
flip (<*>) :: (Applicative f) => f a           -> f (a ->   b)  -> f b
     (>>=) :: (Monad       f) => f a           ->   (a -> f b)  -> f b
flip (>>=) :: (Monad       f) =>   (a -> f b)  -> f a           -> f b
 concatMap :: (Foldable    f) =>   (a -> [b])  -> f a           -> [b]
    concat :: (Foldable    f) => f [a]                          -> [a]
     (=<<) :: (Monad       f) =>   (a -> f b)  -> f a           -> f b
     (>> ) :: (Monad       f) => f a           -> f b           -> f b
     (<* ) :: (Applicative f) => f a           -> f b           -> f a
     ( *>) :: (Applicative f) => f a           -> f b           -> f b
```


The list monad operations are traditionally described in terms of `concatMap`. Similarly, `mconcat` for lists is just `concat`.

```hs
xs >>= f = concatMap f xs

concatMap :: Foldable t =>  (a -> [b])  -> t a  -> [b]
mconcat   :: Monoid   a =>    [a]               -> a
concat    :: Foldable t =>  t [a]               -> [a]
map       ::                (a -> b)    -> [a]  -> [b]
```




```hs
fmap   :: (Applicative m, Monad m) =>   (a -> b) -> (m a -> m b)
(<*>)  :: Applicative f            => f (a -> b) ->  f a -> f b
return :: Monad m =>                     a       ->  m a
fmap   :: Functor f =>                  (a -> b) -> f a -> f b
fmap = (<*>) . return
       (<*>) . return :: Monad   f => (a -> b) -> f a -> f b
fmap                  :: Functor f => (a -> b) -> f a -> f b
       (<*>) . pure::Applicative f => (a -> b) -> f a -> f b
```





```hs
class Functor (f :: * -> *) where
  fmap  :: (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b
  (<$)  ::  a       -> f b -> f a
MINIMAL: fmap

class (Functor f) => Applicative (f :: * -> *) where
  pure  ::    a              -> f a  -- ~return
  (<*>) :: f (a -> b) -> f a -> f b
  (*>)  :: f  a       -> f b -> f b
  (<*)  :: f  a       -> f b -> f a
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
MINIMAL: pure, (<*>) | liftA2

(<*>) :: f (a -> b) -> f a -> f b
pure  ::          a -> f a

(<*>) . pure :: (Applicative f) => (a -> b) -> f a -> f b









&&                              
^
^^
+                               
++                              
<                               
<*                              
<*>                             
<=                              
<>                              
<$                              
<$>                             
=<<                             
==                              
>                               
>=                              
>>                              
>>=
||
$                               
$!                              


abs
acos
acosh
all
and
any
appendFile
asin
asinh
asTypeOf
atan
atan2
atanh
break
ceiling
compare
concat
concatMap
const
cos
cosh
curry
cycle
decodeFloat
div
divMod
drop
dropWhile
either
elem
encodeFloat
enumFrom
enumFromThen
enumFromThenTo
enumFromTo
error
errorWithoutStackTrace
even
exp
exponent
fail
filter
flip
floatDigits
floatRadix
floatRange
floor
fmap
foldl
foldl1
foldMap
foldr
foldr1
fromEnum
fromInteger
fromIntegral
fromRational
fst
gcd
getChar
getContents
getLine
head
id
init
interact
ioError
isDenormalized
isIEEE
isInfinite
isNaN
isNegativeZero
iterate
last
lcm
length
lex
lines
log
logBase
lookup
map
mapM
mapM_
mappend
max
maxBound
maximum
maybe
mconcat
mempty
min
minBound
minimum
mod
negate
not
notElem
null
odd
or
otherwise
pi
pred
print
product
properFraction
pure
putChar
putStr
putStrLn
quot
quotRem
Rational
Read
read
readFile
readIO
readList
readLn
readParen
ReadS
reads
readsPrec
Real
RealFloat
RealFrac
realToFrac
recip
rem
repeat
replicate
return
reverse
Right
round
scaleFloat
scanl
scanl1
scanr
scanr1
Semigroup
seq
sequence
sequence_
sequenceA
show
Show
showChar
showList
showParen
ShowS
shows
showsPrec
showString
significand
signum
sin
sinh
snd
span
splitAt
sqrt
String
subtract
succ
sum
tail
take
takeWhile
tan
tanh
toEnum
toInteger
toRational
Traversable
traverse
True
truncate
uncurry
undefined
unlines
until
unwords
unzip
unzip3
userError
Word
words
writeFile
zip
zip3
zipWith
zipWith3

Applicative                     
Bool                            
Bounded                         
Char                            
Double                          
EQ                              
Either                          
Enum                            
Eq                              
False                           
FilePath                        
Float                           
Floating                        
Foldable                        
Fractional                      
Functor                         
GT                              
IO                              
IOError                         
Int                             
Integer                         
Integral                        
Just                            
LT                              
Left                            
Maybe                           
Monad                           
Monoid                          
Nothing                         
Num                             
Ord                             
Ordering                        


Prelude.!!                      
Prelude.$                       
Prelude.$!                      
Prelude.&&                      
Prelude.*                       
Prelude.**                      
Prelude.*>                      
Prelude.+                       
Prelude.++                      
Prelude.-                       
Prelude..                       
Prelude./                       
Prelude./=                      
Prelude.<                       
Prelude.<$                      
Prelude.<$>                     
Prelude.<*                      
Prelude.<*>                     
Prelude.<=                      
Prelude.<>                      
Prelude.=<<                     
Prelude.==                      
Prelude.>                       
Prelude.>=                      
Prelude.>>                      
Prelude.>>=                     


Prelude.Applicative             
Prelude.Bool                    
Prelude.Bounded                 
Prelude.Char                    
Prelude.Double                  
Prelude.EQ                      
Prelude.Either                  
Prelude.Enum                    
Prelude.Eq                      
Prelude.False                   
Prelude.FilePath                
Prelude.Float                   
Prelude.Floating                
Prelude.Foldable                
Prelude.Fractional              
Prelude.Functor                 
Prelude.GT                      
Prelude.IO                      
Prelude.IOError                 
Prelude.Int                     
Prelude.Integer                 
Prelude.Integral                
Prelude.Just                    
Prelude.LT                      
Prelude.Left                    
Prelude.Maybe                   
Prelude.Monad                   
Prelude.Monoid                  
Prelude.Nothing                 
Prelude.Num                     
Prelude.Ord                     
Prelude.Ordering                
Prelude.Rational                
Prelude.Read                    
Prelude.ReadS                   
Prelude.Real                    
Prelude.RealFloat               
Prelude.RealFrac                
Prelude.Right                   
Prelude.Semigroup               
Prelude.Show                    
Prelude.ShowS                   
Prelude.String                  
Prelude.Traversable             
Prelude.True                    
Prelude.Word                    
Prelude.^                       
Prelude.^^                      
Prelude.abs                     
Prelude.acos                    
Prelude.acosh                   
Prelude.all                     
Prelude.and                     
Prelude.any                     
Prelude.appendFile              
Prelude.asTypeOf                
Prelude.asin                    
Prelude.asinh                   
Prelude.atan                    
Prelude.atan2                   
Prelude.atanh                   
Prelude.break                   
Prelude.ceiling                 
Prelude.compare                 
Prelude.concat                  
Prelude.concatMap               
Prelude.const                   
Prelude.cos                     
Prelude.cosh                    
Prelude.curry                   
Prelude.cycle                   
Prelude.decodeFloat             
Prelude.div                     
Prelude.divMod                  
Prelude.drop                    
Prelude.dropWhile               
Prelude.either                  
Prelude.elem                    
Prelude.encodeFloat             
Prelude.enumFrom                
Prelude.enumFromThen            
Prelude.enumFromThenTo          
Prelude.enumFromTo              
Prelude.error                   
Prelude.errorWithoutStackTrace  
Prelude.even                    
Prelude.exp                     
Prelude.exponent                
Prelude.fail                    
Prelude.filter                  
Prelude.flip                    
Prelude.floatDigits             
Prelude.floatRadix              
Prelude.floatRange              
Prelude.floor                   
Prelude.fmap                    
Prelude.foldMap                 
Prelude.foldl                   
Prelude.foldl1                  
Prelude.foldr                   
Prelude.foldr1                  
Prelude.fromEnum                
Prelude.fromInteger             
Prelude.fromIntegral            
Prelude.fromRational            
Prelude.fst                     
Prelude.gcd                     
Prelude.getChar                 
Prelude.getContents             
Prelude.getLine                 
Prelude.head                    
Prelude.id                      
Prelude.init                    
Prelude.interact                
Prelude.ioError                 
Prelude.isDenormalized          
Prelude.isIEEE                  
Prelude.isInfinite              
Prelude.isNaN                   
Prelude.isNegativeZero          
Prelude.iterate                 
Prelude.last                    
Prelude.lcm                     
Prelude.length                  
Prelude.lex                     
Prelude.lines                   
Prelude.log                     
Prelude.logBase                 
Prelude.lookup                  
Prelude.map                     
Prelude.mapM                    
Prelude.mapM_                   
Prelude.mappend                 
Prelude.max                     
Prelude.maxBound                
Prelude.maximum                 
Prelude.maybe                   
Prelude.mconcat                 
Prelude.mempty                  
Prelude.min                     
Prelude.minBound                
Prelude.minimum                 
Prelude.mod                     
Prelude.negate                  
Prelude.not                     
Prelude.notElem                 
Prelude.null                    
Prelude.odd                     
Prelude.or                      
Prelude.otherwise               
Prelude.pi                      
Prelude.pred                    
Prelude.print                   
Prelude.product                 
Prelude.properFraction          
Prelude.pure                    
Prelude.putChar                 
Prelude.putStr                  
Prelude.putStrLn                
Prelude.quot                    
Prelude.quotRem                 
Prelude.read                    
Prelude.readFile                
Prelude.readIO                  
Prelude.readList                
Prelude.readLn                  
Prelude.readParen               
Prelude.reads                   
Prelude.readsPrec               
Prelude.realToFrac              
Prelude.recip                   
Prelude.rem
Prelude.repeat
Prelude.replicate
Prelude.return
Prelude.reverse
Prelude.round
Prelude.scaleFloat
Prelude.scanl
Prelude.scanl1
Prelude.scanr
Prelude.scanr1
Prelude.seq
Prelude.sequence
Prelude.sequenceA
Prelude.sequence_
Prelude.show
Prelude.showChar
Prelude.showList
Prelude.showParen
Prelude.showString
Prelude.shows
Prelude.showsPrec
Prelude.significand
Prelude.signum
Prelude.sin
Prelude.sinh
Prelude.snd
Prelude.span
Prelude.splitAt
Prelude.sqrt
Prelude.subtract
Prelude.succ
Prelude.sum
Prelude.tail
Prelude.take
Prelude.takeWhile
Prelude.tan
Prelude.tanh
Prelude.toEnum
Prelude.toInteger
Prelude.toRational
Prelude.traverse
Prelude.truncate
Prelude.uncurry
Prelude.undefined
Prelude.unlines
Prelude.until
Prelude.unwords
Prelude.unzip
Prelude.unzip3
Prelude.userError
Prelude.words
Prelude.writeFile
Prelude.zip
Prelude.zip3
Prelude.zipWith
Prelude.zipWith3
Prelude.||

```
