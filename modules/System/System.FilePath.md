# System.FilePath

```hs
import via System.FilePath

type FilePath = String

-- predicates
isPathSeparator           :: Char -> Bool
isSearchPathSeparator     :: Char -> Bool

isExtSeparator            :: Char -> Bool
hasExtension              :: FilePath -> Bool
isExtensionOf             :: String -> FilePath -> Bool

hasDrive                  :: FilePath -> Bool
hasTrailingPathSeparator  :: FilePath -> Bool

isDrive                   :: FilePath -> Bool
isValid                   :: FilePath -> Bool
isAbsolute                :: FilePath -> Bool
isRelative                :: FilePath -> Bool
equalFilePath             :: FilePath -> FilePath -> Bool


pathSeparator             :: Char
searchPathSeparator       :: Char
extSeparator              :: Char
pathSeparators            :: [Char]

(</>)        :: FilePath -> FilePath -> FilePath

(-<.>)       :: FilePath -> String -> FilePath
(<.>)        :: FilePath -> String -> FilePath
addExtension :: FilePath -> String -> FilePath

addTrailingPathSeparator  :: FilePath -> FilePath
dropTrailingPathSeparator :: FilePath -> FilePath

dropDrive                 :: FilePath -> FilePath
dropFileName              :: FilePath -> FilePath
dropExtension             :: FilePath -> FilePath
dropExtensions            :: FilePath -> FilePath

takeDirectory             :: FilePath -> FilePath
takeDrive                 :: FilePath -> FilePath
takeFileName              :: FilePath -> FilePath

takeBaseName              :: FilePath -> String
takeExtension             :: FilePath -> String
takeExtensions            :: FilePath -> String

getSearchPath             :: IO [FilePath]

combine                   :: FilePath -> FilePath -> FilePath

joinDrive                 :: FilePath -> FilePath -> FilePath
joinPath                  :: [FilePath] -> FilePath

makeRelative              :: FilePath -> FilePath -> FilePath
makeValid                 :: FilePath -> FilePath
normalise                 :: FilePath -> FilePath

replaceBaseName           :: FilePath -> String -> FilePath
replaceDirectory          :: FilePath -> String -> FilePath
replaceExtension          :: FilePath -> String -> FilePath
replaceExtensions         :: FilePath -> String -> FilePath
replaceFileName           :: FilePath -> String -> FilePath

splitDirectories          :: FilePath -> [FilePath]
splitDrive                :: FilePath -> (FilePath, FilePath)
splitExtension            :: FilePath -> (String, String)
splitExtensions           :: FilePath -> (FilePath, String)
splitFileName             :: FilePath -> (String, String)
splitPath                 :: FilePath -> [FilePath]
splitSearchPath           :: String -> [FilePath]

stripExtension            :: String -> FilePath -> Maybe FilePath
```
