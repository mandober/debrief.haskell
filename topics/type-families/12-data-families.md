# Data families

> Data families can be thought of as type families, instances of which are always new, dedicated data types.

Consider the following example:

```hs
data family Vector a
newtype instance Vector () = VUnit Int
newtype instance Vector Word8 = VBytes ByteArray
data instance Vector (a, b) = VPair !(Vector a) !(Vector b)
```

A Vector is a sequence of elements, but for the unit type we can simply store the length as Int, which is way more efficient than allocating memory for each unit value.

Notice how we can decide between data and newtype on a per-instance basis.

This example can be rewritten using type families as follows:

```hs
type family VectorF a
 
type instance VectorF () = VectorUnit
data VectorUnit = VUnit Int
    
type instance VectorF Word8 = VectorWord8
data VectorWord8 = VBytes ByteArray

type instance VectorF (a, b) = VectorPair a b
data VectorPair a b = VPair (VectorF a) (VectorF b)
```

In this translation, there's a data type for every type family instance. However, even boilerplate aside, this is an imperfect translation. Data families offer us something else: the type constructor they introduce is generative, so we do not have to worry about its arity!

For example, the following code is valid:

```hs
data Pair1 f x = P1 (f x) (f x)
type VV = Pair1 Vector
```

On the other hand, Pair1 VectorF would be rejected, as this is not applied to its argument.

Data families can also be associated with a class:

```hs
class Vectorizable a where
  data Vector a
  vlength :: Vector a -> Int
```

Just as with associated types and open type families, this is mostly a matter of code organization.
