


```hs
class Show a where
    show      :: a   -> String
    showList  :: [a] -> ShowS
    showsPrec :: Int -> a -> ShowS
-- MINIMAL: showsPrec | show
```

**Show**
- Members of Show can be presented as strings
- All types covered so far except for functions are a part of Show
- The most used function that deals with the Show typeclass is `show`
- It takes a value whose type is a Show and presents it as a string
