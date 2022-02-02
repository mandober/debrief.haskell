# System.Console.ANSI.Types

```hs
-- System.Console.ANSI

xterm24LevelGray :: Int                     -> GHC.Word.Word8
xterm6LevelRGB   :: Int -> Int -> Int       -> GHC.Word.Word8
xtermSystem      :: ColorIntensity -> Color -> GHC.Word.Word8

data Color
  = Black     -- \e[30m text \e[m
  | Red       -- \e[31m text \e[m
  | Green     -- \e[32m text \e[m
  | Yellow    -- \e[33m text \e[m
  | Blue      -- \e[34m text \e[m
  | Magenta   -- \e[35m text \e[m
  | Cyan      -- \e[36m text \e[m
  | White     -- \e[37m text \e[m

data Underlining = SingleUnderline | DoubleUnderline | NoUnderline
data BlinkSpeed = SlowBlink | RapidBlink | NoBlink
data ConsoleLayer = Foreground | Background
data ColorIntensity = Dull | Vivid
data ConsoleIntensity = BoldIntensity | FaintIntensity | NormalIntensity

data SGR = Reset
    | SetConsoleIntensity         !ConsoleIntensity
    | SetItalicized               !Bool
    | SetUnderlining              !Underlining
    | SetBlinkSpeed               !BlinkSpeed
    | SetVisible                  !Bool
    | SetSwapForegroundBackground !Bool
    | SetColor        !ConsoleLayer !ColorIntensity !Color
    | SetDefaultColor !ConsoleLayer
    | SetPaletteColor !ConsoleLayer {-# UNPACK #-}GHC.Word.Word8
    | SetRGBColor     !ConsoleLayer
      !(colour-2.3.5:Data.Colour.Internal.Colour Float)
```
