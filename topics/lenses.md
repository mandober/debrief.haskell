# Lenses

## Introduction to Lenses

**Optics** allow us to separate concerns, to split the action we perform on data from the selection of data. The data selectors are the actual *optics*, and the operations which can be performed on data are *actions*. Each type of optic comes with a set of compatible actions. Each type of optic has a different balance of constraint vs flexibility, resulting in several behaviours.

**Lenses** lean closer to the constrained side of things, which means you have a lot of guarantees about their behaviour but need to prove them to make a lens; so there are fewer lenses than there are, more flexible, optics.

Lenses have the following guarantees
- a lens focuses on a single piece of data within a larger structure
- a lens must never fail to get or modify that focus

These constraints unlock actions we can perform on lenses. We can 
use a lens to *view*, *set*, *modify* the focus within a structure.

## Intro

* The `^.` operator takes a value as its first arg, a lens as its second, and returns the portion of the value focused on by the lens.

* The `^?` operator is like `^.` but it account for failure by returning Maybe.

* We compose lenses using function composition, which allows us to easily focus on part of a deeply nested structure.

* The `.~` operator turns a lens into a *setter function*, with the lens on the left and the new value on the right.

* The `param` lens focuses on the values associated with the given key in the query string. The `responseBody` lens gives us access to the body of a response. The response body is a raw lazy `ByteString`. We can use the `asJSON` function to convert a response body to a Haskell value that implements the `FromJSON` class.

```hs
{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens
r :: Response ByteString
r = get "http://httpbin.org/get"
r ^. responseStatus
-- Status {statusCode = 200, statusMessage = "OK"}
r ^. responseStatus . statusCode
-- 200


ghci> import Data.Aeson.Lens (_String, key)
ghci> let opts = defaults & param "foo" .~ ["bar", "quux"]
ghci> r <- getWith opts "http://httpbin.org/get"
ghci> r ^. responseBody . key "url" . _String
"http://httpbin.org/get?foo=bar&foo=quux"

ghci> import Data.Map as Map
ghci> import Data.Aeson (Value)
ghci> type Resp = Response (Map String Value)
ghci> r <- asJSON =<< get "http://httpbin.org/get" :: IO Resp
ghci> Map.size (r ^. responseBody)
4

-- If the response is not application/json, or we try to convert to an incompatible Haskell type, a JSONError exception will be thrown.
ghci> type Resp = Response [Int]
ghci> r <- asJSON =<< get "http://httpbin.org/get" :: IO Resp
*** Exception: JSONError "when expecting a [a], encountered Object instead"
```

The lens package provides some extremely useful functions for traversing JSON structures without having to either build a corresponding Haskell type or traverse a `Value` by hand. The first of these is `key`, which traverses to the named key in a JSON object.

hci> import Data.Aeson.Lens (key)
ghci> r <- get "http://httpbin.org/get"
ghci> r ^? responseBody . key "url"
Just (String "http://httpbin.org/get")

Notice our use of the ^? operator here. This is like ^., but it allows for the possibility that an access might fail - and of course there may not be a key named "url" in our object.
