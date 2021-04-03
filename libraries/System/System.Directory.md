# System.Directory

```hs
import System.Directory

-- dirs
doesDirectoryExist          :: FilePath -> IO Bool
doesPathExist               :: FilePath -> IO Bool
isSymbolicLink              :: FilePath -> IO Bool
pathIsSymbolicLink          :: FilePath -> IO Bool
-- list dir without . and ..
listDirectory        :: FilePath -> IO [FilePath]
-- list dir with . and ..
getDirectoryContents :: FilePath -> IO [FilePath]
renameDirectory             :: FilePath -> FilePath -> IO ()
renamePath                  :: FilePath -> FilePath -> IO ()
createDirectory             :: FilePath -> IO ()
createDirectoryLink         :: FilePath -> FilePath -> IO ()
createDirectoryIfMissing    :: Bool -> FilePath -> IO ()

-- file
doesFileExist               :: FilePath -> IO Bool
renameFile                  :: FilePath -> FilePath -> IO ()
copyFile                    :: FilePath -> FilePath -> IO ()
copyFileWithMetadata        :: FilePath -> FilePath -> IO ()
createFileLink              :: FilePath -> FilePath -> IO ()

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
getAppUserDataDirectory     :: FilePath -> IO FilePath
canonicalizePath            :: FilePath -> IO FilePath
getSymbolicLinkTarget       :: FilePath -> IO FilePath

getFileSize                 :: FilePath -> IO Integer

-- get $PWD
getCurrentDirectory         :: IO FilePath
-- get $HOME
getHomeDirectory            :: IO FilePath
-- get $TEMP
getTemporaryDirectory       :: IO FilePath

getUserDocumentsDirectory   :: IO FilePath

makeAbsolute                    :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory  :: FilePath -> IO FilePath

setCurrentDirectory  :: FilePath -> IO ()
withCurrentDirectory :: FilePath -> IO a -> IO a


getAccessTime       :: FilePath -> IO time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime
getModificationTime :: FilePath -> IO time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime

setAccessTime       :: FilePath -> time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime -> IO ()
setModificationTime :: FilePath -> time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime -> IO ()


data XdgDirectory = ...
data XdgDirectoryList = ...

XdgData       :: XdgDirectory
XdgConfig     :: XdgDirectory
XdgCache      :: XdgDirectory
XdgDataDirs   :: XdgDirectoryList
XdgConfigDirs :: XdgDirectoryList

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectoryList :: XdgDirectoryList -> IO [FilePath]


data Permissions = ...

executable :: Permissions -> Bool
readable   :: Permissions -> Bool
searchable :: Permissions -> Bool
writable   :: Permissions -> Bool

exeExtension        :: String

emptyPermissions    :: Permissions
copyPermissions     :: FilePath -> FilePath -> IO ()
getPermissions      :: FilePath -> IO Permissions

setPermissions     :: FilePath -> Permissions -> IO ()
setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerReadable   :: Bool -> Permissions -> Permissions
setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerWritable   :: Bool -> Permissions -> Permissions
```
