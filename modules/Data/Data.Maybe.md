# Data.Maybe

```hs
-- imported via Data.Maybe, Prelude
data Maybe a = Nothing | Just a
    Just    :: a -> Maybe a
    Nothing :: Maybe a

maybe       :: b -> (a -> b) -> Maybe a -> b

-- imported via Data.Maybe
isJust      :: Maybe a -> Bool
isNothing   :: Maybe a -> Bool

catMaybes   :: [Maybe a] -> [a]
mapMaybe    :: (a -> Maybe b) -> [a] -> [b]

listToMaybe :: [a] -> Maybe a
maybeToList :: Maybe a -> [a]

fromMaybe   :: a -> Maybe a -> a
fromJust    :: GHC.Stack.Types.HasCallStack => Maybe a -> a
```
