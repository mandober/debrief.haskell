# System.Directory

```hs
import System.Directory

-- predicates: Permissions
executable :: Permissions -> Bool
readable   :: Permissions -> Bool
writable   :: Permissions -> Bool
searchable :: Permissions -> Bool

-- monadic predicates
doesDirectoryExist          :: FilePath -> IO Bool
doesPathExist               :: FilePath -> IO Bool
isSymbolicLink              :: FilePath -> IO Bool
pathIsSymbolicLink          :: FilePath -> IO Bool
doesFileExist               :: FilePath -> IO Bool

-- rename
renameFile                  :: FilePath -> FilePath -> IO ()
renamePath                  :: FilePath -> FilePath -> IO ()
renameDirectory             :: FilePath -> FilePath -> IO ()

-- create
createFileLink              :: FilePath -> FilePath -> IO ()
createDirectory             :: FilePath -> IO ()
createDirectoryLink         :: FilePath -> FilePath -> IO ()
createDirectoryIfMissing    :: Bool -> FilePath -> IO ()

-- copy
copyFile                    :: FilePath -> FilePath -> IO ()
copyFileWithMetadata        :: FilePath -> FilePath -> IO ()

-- remove
removeFile                  :: FilePath -> IO ()
removeDirectory             :: FilePath -> IO ()
removeDirectoryRecursive    :: FilePath -> IO ()
removeDirectoryLink         :: FilePath -> IO ()
removePathForcibly          :: FilePath -> IO ()

-- find
findExecutable              :: String -> IO (Maybe FilePath)
findExecutables             :: String -> IO [FilePath]
findExecutablesInDirectories:: [FilePath] -> String -> IO [FilePath]
findFiles                   :: [FilePath] -> String -> IO [FilePath]
findFile                    :: [FilePath] -> String -> IO (Maybe FilePath)
findFileWith                :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFilesWith               :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]

-- get
exeExtension                :: String
getFileSize                 :: FilePath -> IO Integer
canonicalizePath            :: FilePath -> IO FilePath
getAppUserDataDirectory     :: FilePath -> IO FilePath
getSymbolicLinkTarget       :: FilePath -> IO FilePath
getCurrentDirectory         :: IO FilePath
getHomeDirectory            :: IO FilePath
getTemporaryDirectory       :: IO FilePath
getUserDocumentsDirectory   :: IO FilePath
getXdgDirectory             :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectoryList         :: XdgDirectoryList -> IO [FilePath]
getAccessTime       :: FilePath -> IO time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime
getModificationTime :: FilePath -> IO time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime

-- dir listing (without . and ..)
listDirectory        :: FilePath -> IO [FilePath]
-- dir listing (with . and ..)
getDirectoryContents :: FilePath -> IO [FilePath]

-- set
setCurrentDirectory   :: FilePath -> IO ()
withCurrentDirectory  :: FilePath -> IO a -> IO a
setAccessTime         :: FilePath -> time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime -> IO ()
setModificationTime   :: FilePath -> time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime -> IO ()
setOwnerExecutable    :: Bool -> Permissions -> Permissions
setOwnerReadable      :: Bool -> Permissions -> Permissions
setOwnerSearchable    :: Bool -> Permissions -> Permissions
setOwnerWritable      :: Bool -> Permissions -> Permissions
makeAbsolute                    :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory  :: FilePath -> IO FilePath

-- Permissions
emptyPermissions   :: Permissions
copyPermissions    :: FilePath -> FilePath -> IO ()
getPermissions     :: FilePath -> IO Permissions
setPermissions     :: FilePath -> Permissions -> IO ()



type XdgDirectoryList :: Type
data XdgDirectoryList = XdgDataDirs | XdgConfigDirs

type XdgDirectory :: Type
data XdgDirectory = XdgData | XdgConfig | XdgCache

type Permissions :: Type
data Permissions
  = directory-1.3.6.2:System.Directory.Internal.Common.Permissions
    { readable   :: Bool
    , writable   :: Bool
    , executable :: Bool
    , searchable :: Bool
    }
```
