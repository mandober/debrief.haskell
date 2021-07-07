# System.Console.ANSI.Types

```hs
-- System.Console.ANSI
Black   :: Color                          -- \e[30m text \e[m
Red     :: Color                          -- \e[31m text \e[m
Green   :: Color                          -- \e[32m text \e[m
Yellow  :: Color                          -- \e[33m text \e[m
Blue    :: Color                          -- \e[34m text \e[m
Magenta :: Color                          -- \e[35m text \e[m
Cyan    :: Color                          -- \e[36m text \e[m
White   :: Color                          -- \e[37m text \e[m

Reset           :: SGR

Background      :: ConsoleLayer
Foreground      :: ConsoleLayer

NoUnderline     :: Underlining
DoubleUnderline :: Underlining
SingleUnderline :: Underlining

Dull            :: ColorIntensity
Vivid           :: ColorIntensity

BoldIntensity   :: ConsoleIntensity
FaintIntensity  :: ConsoleIntensity
NormalIntensity :: ConsoleIntensity

NoBlink         :: BlinkSpeed
SlowBlink       :: BlinkSpeed
RapidBlink      :: BlinkSpeed

xterm24LevelGray :: Int                     -> GHC.Word.Word8
xterm6LevelRGB   :: Int -> Int -> Int       -> GHC.Word.Word8
xtermSystem      :: ColorIntensity -> Color -> GHC.Word.Word8

SetItalicized               :: Bool                                     -> SGR
SetVisible                  :: Bool                                     -> SGR
SetSwapForegroundBackground :: Bool                                     -> SGR
SetBlinkSpeed               :: BlinkSpeed                               -> SGR
SetUnderlining              :: Underlining                              -> SGR
SetDefaultColor             :: ConsoleLayer                             -> SGR
SetConsoleIntensity         :: ConsoleIntensity                         -> SGR
SetPaletteColor    :: ConsoleLayer -> GHC.Word.Word8                    -> SGR
SetColor           :: ConsoleLayer -> ColorIntensity -> Color           -> SGR
SetRGBColor        :: ConsoleLayer -> Data.Colour.Internal.Colour Float -> SGR


type Underlining :: *
data Underlining = SingleUnderline | DoubleUnderline | NoUnderline

type Color :: *
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type BlinkSpeed :: *
data BlinkSpeed = SlowBlink | RapidBlink | NoBlink

type ConsoleLayer :: *
data ConsoleLayer = Foreground | Background

type ColorIntensity :: *
data ColorIntensity = Dull | Vivid

type ConsoleIntensity :: *
data ConsoleIntensity = BoldIntensity | FaintIntensity | NormalIntensity

type SGR :: *
data SGR = Reset
  | SetConsoleIntensity
      !ConsoleIntensity
  | SetItalicized
      !Bool
  | SetUnderlining
      !Underlining
  | SetBlinkSpeed
      !BlinkSpeed
  | SetVisible
      !Bool
  | SetSwapForegroundBackground
      !Bool
  | SetColor
      !ConsoleLayer
      !ColorIntensity
      !Color
  | SetDefaultColor
      !ConsoleLayer
  | SetPaletteColor
      !ConsoleLayer 
      {-# UNPACK #-}GHC.Word.Word8
  | SetRGBColor
      !ConsoleLayer
      !(colour-2.3.5:Data.Colour.Internal.Colour Float)

```
