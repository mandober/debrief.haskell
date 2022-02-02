# Data.Text

```hs
import Data.Text qualified as T

-- convert between String and T.Text
T.pack    :: String -> T.Text
T.unpack  :: T.Text -> String

-- predicates
T.all        :: (Char -> Bool) -> T.Text -> Bool
T.any        :: (Char -> Bool) -> T.Text -> Bool
T.isInfixOf  :: T.Text -> T.Text -> Bool
T.isPrefixOf :: T.Text -> T.Text -> Bool
T.isSuffixOf :: T.Text -> T.Text -> Bool
T.null       :: T.Text -> Bool

-- formattimg
T.center       :: Int -> Char -> T.Text -> T.Text
T.justifyLeft  :: Int -> Char -> T.Text -> T.Text
T.justifyRight :: Int -> Char -> T.Text -> T.Text
T.toLower      :: T.Text -> T.Text
T.toTitle      :: T.Text -> T.Text
T.toUpper      :: T.Text -> T.Text
T.toCaseFold   :: T.Text -> T.Text

-- common ops
T.empty     :: T.Text
T.copy      :: T.Text -> T.Text
T.singleton :: Char -> T.Text
T.cons      :: Char   -> T.Text -> T.Text
T.uncons    :: T.Text -> Maybe (Char, T.Text)
T.append    :: T.Text -> T.Text -> T.Text
T.concat    :: [T.Text] -> T.Text
T.concatMap :: (Char -> T.Text) -> T.Text -> T.Text
T.reverse   :: T.Text -> T.Text
T.head      :: T.Text -> Char
T.last      :: T.Text -> Char
T.init      :: T.Text -> T.Text
T.inits     :: T.Text -> [T.Text]
T.length    :: T.Text -> Int
T.tail      :: T.Text -> T.Text
T.tails     :: T.Text -> [T.Text]
T.snoc      :: T.Text -> Char -> T.Text
T.lines     :: T.Text -> [T.Text]
T.unlines   :: [T.Text] -> T.Text
T.unsnoc    :: T.Text -> Maybe (T.Text, Char)
T.words     :: T.Text -> [T.Text]
T.unwords   :: [T.Text] -> T.Text


T.break      :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
T.breakOn    :: T.Text -> T.Text -> (T.Text, T.Text)
T.breakOnEnd :: T.Text -> T.Text -> (T.Text, T.Text)
T.breakOnAll :: T.Text -> T.Text -> [(T.Text, T.Text)]
T.chunksOf :: Int -> T.Text -> [T.Text]
T.commonPrefixes :: T.Text -> T.Text -> Maybe (T.Text, T.Text, T.Text)
T.compareLength :: T.Text -> Int -> Ordering
T.count :: T.Text -> T.Text -> Int
T.drop :: Int -> T.Text -> T.Text
T.dropAround :: (Char -> Bool) -> T.Text -> T.Text
T.dropEnd :: Int -> T.Text -> T.Text
T.dropWhile :: (Char -> Bool) -> T.Text -> T.Text
T.dropWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
T.filter :: (Char -> Bool) -> T.Text -> T.Text
T.find :: (Char -> Bool) -> T.Text -> Maybe Char
T.findIndex :: (Char -> Bool) -> T.Text -> Maybe Int
T.group   :: T.Text -> [T.Text]
T.groupBy :: (Char -> Char -> Bool) -> T.Text -> [T.Text]
T.index   :: T.Text -> Int -> Char
T.intercalate :: T.Text -> [T.Text] -> T.Text
T.intersperse :: Char -> T.Text -> T.Text
T.maximum :: T.Text -> Char
T.minimum :: T.Text -> Char
T.partition :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
T.replace :: T.Text -> T.Text -> T.Text -> T.Text
T.replicate :: Int -> T.Text -> T.Text
T.span :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
T.split :: (Char -> Bool) -> T.Text -> [T.Text]
T.splitAt :: Int -> T.Text -> (T.Text, T.Text)
T.splitOn :: T.Text -> T.Text -> [T.Text]
T.strip :: T.Text -> T.Text
T.stripEnd :: T.Text -> T.Text
T.stripPrefix :: T.Text -> T.Text -> Maybe T.Text
T.stripStart :: T.Text -> T.Text
T.stripSuffix :: T.Text -> T.Text -> Maybe T.Text
T.take :: Int -> T.Text -> T.Text
T.takeEnd :: Int -> T.Text -> T.Text
T.takeWhile :: (Char -> Bool) -> T.Text -> T.Text
T.takeWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
T.transpose :: [T.Text] -> [T.Text]
T.zip :: T.Text -> T.Text -> [(Char, Char)]
T.zipWith :: (Char -> Char -> Char) -> T.Text -> T.Text -> T.Text
T.unfoldr :: (a -> Maybe (Char, a)) -> a -> T.Text
T.unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> T.Text

-- folding
T.foldl :: (a -> Char -> a) -> a -> T.Text -> a
T.foldl' :: (a -> Char -> a) -> a -> T.Text -> a
T.foldl1 :: (Char -> Char -> Char) -> T.Text -> Char
T.foldl1' :: (Char -> Char -> Char) -> T.Text -> Char
T.foldr :: (Char -> a -> a) -> a -> T.Text -> a
T.foldr1 :: (Char -> Char -> Char) -> T.Text -> Char

T.map :: (Char -> Char) -> T.Text -> T.Text
T.mapAccumL :: (a -> Char -> (a, Char)) -> a -> T.Text -> (a, T.Text)
T.mapAccumR :: (a -> Char -> (a, Char)) -> a -> T.Text -> (a, T.Text)

T.scanl :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
T.scanl1 :: (Char -> Char -> Char) -> T.Text -> T.Text
T.scanr :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
T.scanr1 :: (Char -> Char -> Char) -> T.Text -> T.Text


-- misc
T.unpackCString# :: GHC.Prim.Addr# -> T.Text

data T.Text = Data.Text.Internal.Text {-# UNPACK #-}Data.Text.Array.Array
                                      {-# UNPACK #-}Int
                                      {-# UNPACK #-}Int
-- simplified:
data T.Text = Text Array Int Int
                          ↑   ↑
--         probably: length   capacity
```
