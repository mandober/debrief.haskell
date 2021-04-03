# Test.QuickCheck.Text

```hs
type Test.QuickCheck.Text.Cell :: *
data Test.QuickCheck.Text.Cell = ...

Test.QuickCheck.Text.Centred :: String -> Test.QuickCheck.Text.Cell
Test.QuickCheck.Text.LJust :: String -> Test.QuickCheck.Text.Cell
Test.QuickCheck.Text.MkStr :: String -> Test.QuickCheck.Text.Str
Test.QuickCheck.Text.RJust :: String -> Test.QuickCheck.Text.Cell

type Test.QuickCheck.Text.Str :: *
newtype Test.QuickCheck.Text.Str = ...

type Test.QuickCheck.Text.Terminal :: *
data Test.QuickCheck.Text.Terminal = ...

Test.QuickCheck.Text.bold :: String -> String

Test.QuickCheck.Text.centre :: Int -> [Char] -> [Char]
Test.QuickCheck.Text.drawTable ::
  [String] -> [[Test.QuickCheck.Text.Cell]] -> [String]
Test.QuickCheck.Text.handle ::
  GHC.IO.Handle.Types.Handle -> String -> IO ()
Test.QuickCheck.Text.isOneLine :: String -> Bool
Test.QuickCheck.Text.ljust :: Int -> [Char] -> [Char]
Test.QuickCheck.Text.lpercent ::
  (Integral a, Integral b) => a -> b -> String
Test.QuickCheck.Text.lpercentage ::
  Integral a => Double -> a -> String
Test.QuickCheck.Text.newTerminal ::
  (String -> IO ())
  -> (String -> IO ()) -> IO Test.QuickCheck.Text.Terminal
Test.QuickCheck.Text.number :: Int -> String -> String
Test.QuickCheck.Text.oneLine :: String -> String
Test.QuickCheck.Text.paragraphs :: [[String]] -> [String]
Test.QuickCheck.Text.putLine ::
  Test.QuickCheck.Text.Terminal -> String -> IO ()
Test.QuickCheck.Text.putPart ::
  Test.QuickCheck.Text.Terminal -> String -> IO ()
Test.QuickCheck.Text.putTemp ::
  Test.QuickCheck.Text.Terminal -> String -> IO ()
Test.QuickCheck.Text.ranges ::
  (Show a, Integral a) => a -> a -> Test.QuickCheck.Text.Str
Test.QuickCheck.Text.rjust :: Int -> [Char] -> [Char]
Test.QuickCheck.Text.rpercent ::
  (Integral a, Integral b) => a -> b -> String
Test.QuickCheck.Text.rpercentage ::
  Integral a => Double -> a -> String
Test.QuickCheck.Text.short :: Int -> String -> String
Test.QuickCheck.Text.showErr :: Show a => a -> String
Test.QuickCheck.Text.terminalOutput ::
  Test.QuickCheck.Text.Terminal -> IO String
Test.QuickCheck.Text.withHandleTerminal ::
  GHC.IO.Handle.Types.Handle
  -> Maybe GHC.IO.Handle.Types.Handle
  -> (Test.QuickCheck.Text.Terminal -> IO a)
  -> IO a
Test.QuickCheck.Text.withNullTerminal ::
  (Test.QuickCheck.Text.Terminal -> IO a) -> IO a
Test.QuickCheck.Text.withStdioTerminal ::
  (Test.QuickCheck.Text.Terminal -> IO a) -> IO a
```
