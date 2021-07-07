# GHC.Show


```hs
GHC.Show.appPrec  :: Int
GHC.Show.appPrec1 :: Int

GHC.Show.asciiTab :: [String]
GHC.Show.showMultiLineString :: String -> [String]

GHC.Show.showCommaSpace :: ShowS
GHC.Show.showSpace      :: ShowS

GHC.Show.showLitString :: String     -> ShowS
GHC.Show.showSignedInt :: Int -> Int -> ShowS

GHC.Show.protectEsc    :: (Char -> Bool) -> ShowS -> ShowS
GHC.Show.showList__    :: (a -> ShowS)   -> [a]   -> ShowS

intToDigit  :: Int  -> Char         -- Data.Char

showLitChar :: Char -> ShowS        -- Data.Char

type ShowS :: *
type ShowS = String -> String

show       :: Show a => a -> String                   -- class Show

showList   :: Show a =>              [a]     -> ShowS  -- class Show
showsPrec  :: Show a =>   Int   ->   a       -> ShowS  -- class Show
shows      :: Show a =>              a       -> ShowS
showChar   ::                        Char    -> ShowS
showString ::                        String  -> ShowS
showParen  ::             Bool  ->   ShowS   -> ShowS

-- ----------------------------------------------------------------------------
shows :: Show a => a -> ShowS
shows a = \s -> a ++ s

showParen :: Bool -> ShowS -> ShowS
showParen True  f = \s -> "(" ++ f s ++ ")"
showParen False f = \s -> f s
```




## Show class

```hs
type Show :: * -> Constraint
class Show a where
    showsPrec :: Int -> a -> ShowS
    show      ::        a -> String
    showList  ::      [a] -> ShowS
{-# MINIMAL showsPrec | show #-}


instance Show Int
instance Show Integer
instance Show Word
instance Show GHC.Word.Word8
instance Show GHC.Word.Word64
instance Show GHC.Word.Word32
instance Show GHC.Word.Word16

instance Show ()
instance Show Char
instance Show Ordering
instance Show Bool
instance Show a => Show (Maybe a)
instance (Show a, Show b) => Show (a, b)
instance Show a => Show [a]
instance Show a => Show (GHC.Base.NonEmpty a)

-- Data.Either
instance (Show a, Show b) => Show (Either a b)


-- GHC.Float
instance Show Float
instance Show Double

-- GHC.Real
instance Show a => Show (GHC.Real.Ratio a)


instance Show GHC.TypeNats.SomeNat              -- GHC.TypeNats
instance Show GHC.TypeLits.SomeSymbol           -- GHC.TypeLits

-- Data.Functor.Const
instance forall k a (b :: k).
    Show a =>
    Show (Data.Functor.Const.Const a b)

-- Data.Functor.Identity
instance Show a => Show (Data.Functor.Identity.Identity a)

-- Control.Applicative
instance Show a => Show (Control.Applicative.ZipList a)

-- GHC.Unicode
instance Show GHC.Unicode.GeneralCategory

-- Text.Read.Lex
instance Show Text.Read.Lex.Number
instance Show Text.Read.Lex.Lexeme


-- GHC.Arr
instance (GHC.Ix.Ix a, Show a, Show b) => Show (GHC.Arr.Array a b)

-- Data.Ord
instance Show a => Show (Data.Ord.Down a)

-- Data.Monoid
instance Show a => Show (Data.Monoid.Last a)
instance Show a => Show (Data.Monoid.First a)

instance forall k (f :: k -> *) (a :: k).
    Show (f a) =>
    Show (Data.Monoid.Ap f a)


-- Data.Proxy
instance forall k (s :: k). Show (Data.Proxy.Proxy s)


-- base-4.14.1.0:Data.Semigroup.Internal
instance Show base-4.14.1.0:Data.Semigroup.Internal.Any
instance Show base-4.14.1.0:Data.Semigroup.Internal.All

instance Show a => Show (base-4.14.1.0:Data.Semigroup.Internal.Sum a)
instance Show a => Show (base-4.14.1.0:Data.Semigroup.Internal.Product a)
instance Show a => Show (base-4.14.1.0:Data.Semigroup.Internal.Dual a)

instance forall k (f :: k -> *) (a :: k).
    Show (f a) =>
    Show (base-4.14.1.0:Data.Semigroup.Internal.Alt f a)


-- GHC.Ptr
instance Show (GHC.Ptr.Ptr a)
instance Show (GHC.Ptr.FunPtr a)


-- GHC.Generics
instance Show GHC.Generics.SourceUnpackedness
instance Show GHC.Generics.SourceStrictness
instance Show GHC.Generics.Fixity
instance Show GHC.Generics.DecidedStrictness
instance Show GHC.Generics.Associativity

instance forall k (p :: k). Show (GHC.Generics.V1 p)
instance forall k (p :: k). Show (GHC.Generics.URec Char p)
instance forall k (p :: k). Show (GHC.Generics.URec Double p)
instance forall k (p :: k). Show (GHC.Generics.URec Float p)
instance forall k (p :: k). Show (GHC.Generics.URec Int p)
instance forall k (p :: k). Show (GHC.Generics.URec Word p)
instance forall k (p :: k). Show (GHC.Generics.U1 p)

-- ∀ k f p.
instance forall 
          k 
    (f :: k -> *)
    (p :: k).
    Show (f p) =>
    Show (GHC.Generics.Rec1 f p)

instance Show p => Show (GHC.Generics.Par1 p)

instance forall i 
    (c :: GHC.Generics.Meta) 
          k 
    (f :: k -> *) 
    (p :: k).
    Show (f p) =>
    Show (GHC.Generics.M1 i c f p)

instance forall i c k (p :: k).
    Show c =>
    Show (GHC.Generics.K1 i c p)

instance forall k2 
          (f :: k2 -> *) 
                k1 
          (g :: k1 -> k2) 
          (p :: k1).
    Show (f (g p)) => 
    Show ((GHC.Generics.:.:) f g p)

instance forall k 
          (f :: k -> *) 
          (g :: k -> *) 
          (p :: k).
    (Show (f p), Show (g p)) =>
    Show ((GHC.Generics.:+:) f g p)

instance forall k 
          (f :: k -> *) 
          (g :: k -> *) 
          (p :: k).
    (Show (f p), Show (g p)) =>
    Show ((GHC.Generics.:*:) f g p)



-- GHC.Exception.Type
instance Show GHC.Exception.Type.SomeException
instance Show GHC.Exception.Type.ArithException

instance Show GHC.IO.MaskingState

-- GHC.IO.Exception
instance Show GHC.IO.Exception.Deadlock
instance Show GHC.IO.Exception.ExitCode
instance Show GHC.IO.Exception.IOException
instance Show GHC.IO.Exception.IOErrorType
instance Show GHC.IO.Exception.FixIOException
instance Show GHC.IO.Exception.AsyncException
instance Show GHC.IO.Exception.ArrayException
instance Show GHC.IO.Exception.AssertionFailed
instance Show GHC.IO.Exception.CompactionFailed
instance Show GHC.IO.Exception.SomeAsyncException
instance Show GHC.IO.Exception.AllocationLimitExceeded
instance Show GHC.IO.Exception.BlockedIndefinitelyOnSTM
instance Show GHC.IO.Exception.BlockedIndefinitelyOnMVar

instance Show GHC.Natural.Natural

instance Show GHC.Types.TyCon
instance Show GHC.Types.TrName
instance Show GHC.Types.Module
instance Show GHC.Types.KindRep
instance Show GHC.Types.VecElem
instance Show GHC.Types.VecCount
instance Show GHC.Types.TypeLitSort
instance Show GHC.Types.RuntimeRep

instance Show GHC.Stack.Types.SrcLoc
instance Show GHC.Stack.Types.CallStack
```
