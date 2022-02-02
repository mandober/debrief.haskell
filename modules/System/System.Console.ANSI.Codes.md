# System.Console.ANSI.Codes

```hs
-- System.Console.ANSI.Codes
csi         :: [Int] -> String -> String
colorToCode :: Color -> Int
sgrToCode   :: SGR -> [Int]

-- imported via System.Console.ANSI.Codes, System.Console.ANSI
clearFromCursorToLineBeginningCode   :: String
clearFromCursorToLineEndCode         :: String

clearFromCursorToScreenBeginningCode :: String
clearFromCursorToScreenEndCode       :: String

reportCursorPositionCode             :: String
clearLineCode                        :: String
clearScreenCode                      :: String

hideCursorCode                       :: String
restoreCursorCode                    :: String
saveCursorCode                       :: String
showCursorCode                       :: String

setSGRCode                           :: [SGR] -> String
setTitleCode                         :: String -> String

setCursorPositionCode                :: Int -> Int -> String
setCursorColumnCode                  :: Int -> String

cursorForwardCode                    :: Int -> String
cursorBackwardCode                   :: Int -> String

cursorUpCode                         :: Int -> String
cursorDownCode                       :: Int -> String

cursorUpLineCode                     :: Int -> String
cursorDownLineCode                   :: Int -> String

scrollPageUpCode                     :: Int -> String
scrollPageDownCode                   :: Int -> String


-- and all from
-- System.Console.ANSI.Types
-- ...
```
