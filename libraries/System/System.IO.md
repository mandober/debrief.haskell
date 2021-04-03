# System.IO


## Values

char8           :: TextEncoding
latin1          :: TextEncoding
utf8            :: TextEncoding
utf8_bom        :: TextEncoding
utf16           :: TextEncoding
utf16be         :: TextEncoding
utf16le         :: TextEncoding
utf32           :: TextEncoding
utf32be         :: TextEncoding
utf32le         :: TextEncoding
localeEncoding  :: TextEncoding

stderr :: Handle
stdin  :: Handle
stdout :: Handle

nativeNewline        :: Newline
universalNewlineMode :: NewlineMode
nativeNewlineMode    :: NewlineMode
noNewlineTranslation :: NewlineMode


## Functions

getChar     :: IO Char
getLine     :: IO String
getContents :: IO String
putChar     :: Char   -> IO ()
putStr      :: String -> IO ()
putStrLn    :: String -> IO ()
isEOF       :: IO Bool
print       :: Show a => a -> IO ()

fixIO          :: (a -> IO a) -> IO a
interact       :: (String -> String) -> IO ()
mkTextEncoding :: String -> IO TextEncoding

openFile       :: FilePath -> IOMode -> IO Handle
openBinaryFile :: FilePath -> IOMode -> IO Handle
openTempFile   :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)

readFile   :: FilePath -> IO String
writeFile  :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile       :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

readIO :: (Read a) => String -> IO a
readLn :: (Read a) => IO a


## Handlers

hClose             :: Handle -> IO ()
hFlush             :: Handle -> IO ()
hFileSize          :: Handle -> IO Integer
hGetBuffering      :: Handle -> IO BufferMode
hGetBuf            :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hGetBufNonBlocking :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hGetBufSome        :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hPutBufNonBlocking :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hPutBuf            :: Handle -> GHC.Ptr.Ptr a -> Int -> IO ()

hGetChar           :: Handle -> IO Char
hGetContents       :: Handle -> IO String
hGetLine           :: Handle -> IO String
hGetPosn           :: Handle -> IO HandlePosn
hGetEncoding       :: Handle -> IO (Maybe TextEncoding)
hGetEcho           :: Handle -> IO Bool

hReady             :: Handle -> IO Bool
hIsClosed          :: Handle -> IO Bool
hIsEOF             :: Handle -> IO Bool
hIsOpen            :: Handle -> IO Bool
hIsReadable        :: Handle -> IO Bool
hIsSeekable        :: Handle -> IO Bool
hIsTerminalDevice  :: Handle -> IO Bool
hIsWritable        :: Handle -> IO Bool

hLookAhead         :: Handle -> IO Char
hWaitForInput      :: Handle -> Int -> IO Bool
hSeek              :: Handle -> SeekMode -> Integer -> IO ()
hPutChar           :: Handle -> Char         -> IO ()
hPutStr            :: Handle -> String       -> IO ()
hPutStrLn          :: Handle -> String       -> IO ()

hSetBinaryMode     :: Handle -> Bool         -> IO ()
hSetEcho           :: Handle -> Bool         -> IO ()
hSetFileSize       :: Handle -> Integer      -> IO ()
hSetBuffering      :: Handle -> BufferMode   -> IO ()
hSetEncoding       :: Handle -> TextEncoding -> IO ()
hSetNewlineMode    :: Handle -> NewlineMode  -> IO ()

hShow              :: Handle -> IO String
hTell              :: Handle -> IO Integer
hSetPosn           :: HandlePosn -> IO ()
hPrint :: (Show a) => Handle -> a -> IO ()



## data

type FilePath = String

newtype IO a = IO (State# RealWorld -> (State# RealWorld, a #))

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)

data Handle
  = FileHandle FilePath
  {-# UNPACK #-}(MVar Handle__)
  | DuplexHandle FilePath
  {-# UNPACK #-}(MVar Handle__)
  {-# UNPACK #-}(MVar Handle__)

data HandlePosn = Handle.HandlePosn Handle Handle.HandlePosition

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

data Newline = LF | CRLF

data NewlineMode = NewlineMode {inputNL :: Newline, outputNL :: Newline}

data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd

data TextEncoding = forall dstate estate.TextEncoding
    {
      textEncodingName :: String,
      mkTextDecoder :: IO (TextDecoder dstate),
      mkTextEncoder :: IO (TextEncoder estate)
    }
