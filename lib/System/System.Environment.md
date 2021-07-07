# System.Environment

- module        : System.Environment
- package       : base, base-4.14.0.0
- Stability     : provisional
- Portability   : portable
- Safe Haskell  : Safe
- Language      : Haskell2010
- desc: Miscellaneous information about the system environment
- docs: https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html


```hs
import System.Environment

getExecutablePath   :: IO FilePath
-- get argv as string list
getArgs             :: IO [String]

withArgs            :: [String] -> IO a -> IO a
withProgName        ::  String  -> IO a -> IO a

getProgName         :: IO String

-- get all enwars
getEnvironment      :: IO [(String, String)]
-- get the specified enwar
getEnv              :: String -> IO String
-- maybe get the specified enwar
lookupEnv           :: String -> IO (Maybe String)
-- set enwar
setEnv              :: String -> String -> IO ()
-- unset enwar
unsetEnv            :: String -> IO ()
```
