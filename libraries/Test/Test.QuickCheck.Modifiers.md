# Test.QuickCheck.Modifiers

```hs
ASCIIString :: String -> ASCIIString
type ASCIIString :: *
newtype ASCIIString = ...
Blind :: a -> Blind a
type Blind :: * -> *
newtype Blind a = ...
Fixed :: a -> Fixed a
type Fixed :: * -> *
newtype Fixed a = ...
InfiniteList ::
  [a]
  -> Test.QuickCheck.Modifiers.InfiniteListInternalData a
  -> InfiniteList a
type InfiniteList :: * -> *
data InfiniteList a = ...
Large :: a -> Large a
type Large :: * -> *
newtype Large a = ...
Negative :: a -> Negative a
type Negative :: * -> *
newtype Negative a = ...
NonEmpty :: [a] -> NonEmptyList a
type NonEmptyList :: * -> *
newtype NonEmptyList a = ...
NonNegative :: a -> NonNegative a
type NonNegative :: * -> *
newtype NonNegative a = ...
NonPositive :: a -> NonPositive a
type NonPositive :: * -> *
newtype NonPositive a = ...
NonZero :: a -> NonZero a
type NonZero :: * -> *
newtype NonZero a = ...
Ordered :: [a] -> OrderedList a
type OrderedList :: * -> *
newtype OrderedList a = ...
Positive :: a -> Positive a
type Positive :: * -> *
newtype Positive a = ...
PrintableString :: String -> PrintableString
type PrintableString :: *
newtype PrintableString = ...
Shrink2 :: a -> Shrink2 a
type Shrink2 :: * -> *
newtype Shrink2 a = ...
type ShrinkState :: * -> * -> Constraint
class ShrinkState s a
  ...
Shrinking :: s -> a -> Shrinking s a
type Shrinking :: * -> * -> *
data Shrinking s a = ...
Small :: a -> Small a
type Small :: * -> *
newtype Small a = ...
Smart :: Int -> a -> Smart a
type Smart :: * -> *
data Smart a = ...
Sorted :: [a] -> SortedList a
type SortedList :: * -> *
newtype SortedList a = ...
UnicodeString :: String -> UnicodeString
type UnicodeString :: *
newtype UnicodeString = ...
getASCIIString :: ASCIIString -> String

getBlind :: Blind a -> a
getFixed :: Fixed a -> a
getInfiniteList :: InfiniteList a -> [a]
getLarge :: Large a -> a
getNegative :: Negative a -> a
getNonEmpty :: NonEmptyList a -> [a]
getNonNegative :: NonNegative a -> a
getNonPositive :: NonPositive a -> a
getNonZero :: NonZero a -> a
getOrdered :: OrderedList a -> [a]
getPositive :: Positive a -> a
getPrintableString :: PrintableString -> String
getShrink2 :: Shrink2 a -> a
getSmall :: Small a -> a
getSorted :: SortedList a -> [a]
getUnicodeString :: UnicodeString -> String
infiniteListInternalData ::
  InfiniteList a
  -> Test.QuickCheck.Modifiers.InfiniteListInternalData a
shrinkInit :: ShrinkState s a => a -> s
shrinkState :: ShrinkState s a => a -> s -> [(a, s)]
```
