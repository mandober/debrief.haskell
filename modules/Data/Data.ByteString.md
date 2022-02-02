# Data.ByteString

```hs
import Data.ByteString
-- GHC.Word.Word8


data ByteString = ...ETC

empty :: ByteString

getContents :: IO ByteString
getLine     :: IO ByteString

all            :: (GHC.Word.Word8 -> Bool) -> ByteString -> Bool
any            :: (GHC.Word.Word8 -> Bool) -> ByteString -> Bool

append         :: ByteString -> ByteString -> ByteString
appendFile     :: FilePath -> ByteString -> IO ()

break          :: (GHC.Word.Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd       :: (GHC.Word.Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

breakByte      :: GHC.Word.Word8 -> ByteString -> (ByteString, ByteString)
breakSubstring ::     ByteString -> ByteString -> (ByteString, ByteString)

concat         :: [ByteString] -> ByteString
copy           ::  ByteString  -> ByteString

count          :: GHC.Word.Word8 -> ByteString -> Int

concatMap      :: (GHC.Word.Word8 -> ByteString) -> ByteString -> ByteString

cons           :: GHC.Word.Word8 -> ByteString -> ByteString
uncons         :: ByteString -> Maybe (GHC.Word.Word8, ByteString)

drop           :: Int -> ByteString -> ByteString
dropWhile      :: (GHC.Word.Word8 -> Bool) -> ByteString -> ByteString

elem           :: GHC.Word.Word8 -> ByteString -> Bool
elemIndex      :: GHC.Word.Word8 -> ByteString -> Maybe Int
elemIndexEnd   :: GHC.Word.Word8 -> ByteString -> Maybe Int
elemIndices    :: GHC.Word.Word8 -> ByteString -> [Int]

filter         :: (GHC.Word.Word8 -> Bool) -> ByteString -> ByteString

find           :: (GHC.Word.Word8 -> Bool) -> ByteString -> Maybe GHC.Word.Word8
findIndex      :: (GHC.Word.Word8 -> Bool) -> ByteString -> Maybe Int
findIndices    :: (GHC.Word.Word8 -> Bool) -> ByteString -> [Int]

findSubstring  :: ByteString -> ByteString -> Maybe Int
findSubstrings :: ByteString -> ByteString -> [Int]

foldl   :: (a -> GHC.Word.Word8 -> a) -> a -> ByteString -> a
foldl'  :: (a -> GHC.Word.Word8 -> a) -> a -> ByteString -> a

foldl1  :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> GHC.Word.Word8
foldl1' :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> GHC.Word.Word8

foldr   :: (GHC.Word.Word8 -> a -> a) -> a -> ByteString -> a
foldr'  :: (GHC.Word.Word8 -> a -> a) -> a -> ByteString -> a

foldr1  :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> GHC.Word.Word8
foldr1' :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> GHC.Word.Word8


group           :: ByteString -> [ByteString]
groupBy         :: (GHC.Word.Word8 -> GHC.Word.Word8 -> Bool) -> ByteString -> [ByteString]
hGet            :: GHC.IO.Handle.Types.Handle -> Int -> IO ByteString

hGetContents    :: GHC.IO.Handle.Types.Handle -> IO ByteString
hGetLine        :: GHC.IO.Handle.Types.Handle -> IO ByteString

hGetNonBlocking :: GHC.IO.Handle.Types.Handle -> Int -> IO ByteString
hGetSome        :: GHC.IO.Handle.Types.Handle -> Int -> IO ByteString

hPutNonBlocking :: GHC.IO.Handle.Types.Handle -> ByteString -> IO ByteString

hPut            :: GHC.IO.Handle.Types.Handle -> ByteString -> IO ()
hPutStr         :: GHC.IO.Handle.Types.Handle -> ByteString -> IO ()
hPutStrLn       :: GHC.IO.Handle.Types.Handle -> ByteString -> IO ()

head            :: ByteString -> GHC.Word.Word8
index           :: ByteString -> Int -> GHC.Word.Word8

init            :: ByteString -> ByteString
inits           :: ByteString -> [ByteString]

interact        :: (ByteString -> ByteString) -> IO ()

intercalate     :: ByteString -> [ByteString] -> ByteString
intersperse     :: GHC.Word.Word8 -> ByteString -> ByteString

isInfixOf       :: ByteString -> ByteString -> Bool
isPrefixOf      :: ByteString -> ByteString -> Bool
isSuffixOf      :: ByteString -> ByteString -> Bool

last            :: ByteString -> GHC.Word.Word8
length          :: ByteString -> Int

map             :: (GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> ByteString
mapAccumL       :: (acc -> GHC.Word.Word8 -> (acc, GHC.Word.Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR       :: (acc -> GHC.Word.Word8 -> (acc, GHC.Word.Word8)) -> acc -> ByteString -> (acc, ByteString)

maximum         :: ByteString -> GHC.Word.Word8
minimum         :: ByteString -> GHC.Word.Word8

notElem         :: GHC.Word.Word8 -> ByteString -> Bool
null            :: ByteString -> Bool

pack            :: [GHC.Word.Word8] -> ByteString
unpack          :: ByteString -> [GHC.Word.Word8]

packCString     :: Foreign.C.String.CString -> IO ByteString
packCStringLen  :: Foreign.C.String.CStringLen -> IO ByteString

partition       :: (GHC.Word.Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

putStr          :: ByteString -> IO ()
putStrLn        :: ByteString -> IO ()

readFile        :: FilePath -> IO ByteString
replicate       :: Int -> GHC.Word.Word8 -> ByteString
reverse         :: ByteString -> ByteString

scanl           :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> GHC.Word.Word8 -> ByteString -> ByteString
scanl1          :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> ByteString
scanr           :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> GHC.Word.Word8 -> ByteString -> ByteString
scanr1          :: (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8) -> ByteString -> ByteString

singleton       :: GHC.Word.Word8 -> ByteString

snoc            :: ByteString -> GHC.Word.Word8 -> ByteString
unsnoc          :: ByteString -> Maybe (ByteString, GHC.Word.Word8)

sort            :: ByteString -> ByteString

span            :: (GHC.Word.Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd         :: (GHC.Word.Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

split           :: GHC.Word.Word8 -> ByteString -> [ByteString]
splitAt         :: Int -> ByteString -> (ByteString, ByteString)
splitWith       :: (GHC.Word.Word8 -> Bool) -> ByteString -> [ByteString]

stripPrefix     :: ByteString -> ByteString -> Maybe ByteString
stripSuffix     :: ByteString -> ByteString -> Maybe ByteString

tail            :: ByteString -> ByteString
tails           :: ByteString -> [ByteString]

take            :: Int -> ByteString -> ByteString
takeWhile       :: (GHC.Word.Word8 -> Bool) -> ByteString -> ByteString
transpose       :: [ByteString] -> [ByteString]

uncons          :: ByteString -> Maybe (GHC.Word.Word8, ByteString)
unfoldr         :: (a -> Maybe (GHC.Word.Word8, a)) -> a -> ByteString
unfoldrN        :: Int -> (a -> Maybe (GHC.Word.Word8, a)) -> a -> (ByteString, Maybe a)
unpack          :: ByteString -> [GHC.Word.Word8]
unsnoc          :: ByteString -> Maybe (ByteString, GHC.Word.Word8)
unzip           :: [(GHC.Word.Word8, GHC.Word.Word8)] -> (ByteString, ByteString)

useAsCString    :: ByteString -> (Foreign.C.String.CString -> IO a) -> IO a
useAsCStringLen :: ByteString -> (Foreign.C.String.CStringLen -> IO a) -> IO a

writeFile       :: FilePath -> ByteString -> IO ()

zip             :: ByteString -> ByteString -> [(GHC.Word.Word8, GHC.Word.Word8)]
unzip           :: [(GHC.Word.Word8, GHC.Word.Word8)] -> (ByteString, ByteString)

zipWith         :: (GHC.Word.Word8 -> GHC.Word.Word8 -> a) -> ByteString -> ByteString -> [a]
```
