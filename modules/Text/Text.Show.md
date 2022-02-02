# Text.Show

```hs
shows         :: forall a. Show a => a -> ShowS
showChar      :: Char -> ShowS
showString    :: String -> ShowS
showParen     :: Bool -> ShowS -> ShowS
showListWith  :: forall a. (a -> ShowS) -> [a] -> ShowS

type ShowS :: Type
type ShowS = String -> String

type Show :: Type -> Constraint
class Show a where
  show      :: a -> String
  showsPrec :: Int -> a -> ShowS
  showList  :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
```
