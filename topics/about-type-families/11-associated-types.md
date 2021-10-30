# Associated types

From a code organization perspective, sometimes it makes sense to associate an open type family with a class.

Consider the notion of containers and elements:

```hs
type family Elem a
class Container a where
  elements :: a -> [Elem a]

type instance Elem [a] = a
instance Container [a] where
  elements = id

type instance Elem ByteString = Word8
instance Container ByteString where
  elements = ByteString.unpack
```

We would only use Elem with types that also have a Container instance, so it would be more clear to move it into the class. That is exactly what associated types enable us to do:

```hs
class Container a where
  type Elem a
  elements :: a -> [Elem a]

instance Container [a] where
  type Elem [a] = a
  elements = id

instance Container ByteString where
  type Elem ByteString = Word8
  elements = ByteString.unpack
```

*Associated types are mostly equivalent to open type families, and which one to prefer is often a matter of style.*

> One advantage of associated types is that they can have defaults:

```hs
type family Unwrap x where
  Unwrap (f a) = a

class Container a where
  type Elem a
  type Elem x = Unwrap x
  elements :: a -> [Elem a]
```

This way, we can avoid explicit definition of Elem in most instances:

```hs
instance Container [a] where
  elements = id

instance Container (Maybe a) where
  elements = maybeToList

instance Container ByteString where
  type Elem ByteString = Word8
  elements = ByteString.unpack
```

Current research indicates that associated types are a more promising abstraction mechanism than top-level open type families.   
See ICFP 2017 - Constrained Type Families:
https://www.youtube.com/watch?v=AGJY95Otb9U
