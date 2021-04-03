# Data.Hashable

```hs
import Data.Hashable

-- Data.Hashable
-- Data.Hashable.Lifted
-- Data.Hashable.Generic

-- hidden modules in the package hashable-1.3.0.0:
--      Data.Hashable.Class
--      Data.Hashable.Class.Hashed

-- Data.HashSet
-- Data.HashSet.Lens

-- Data.HashMap.Lazy
-- Data.HashMap.Strict


class Hashable a
  -- ...

data Hashed a = ETC

unhashed :: Hashed a -> a

hash     :: Hashable a => a -> Int
hashed   :: Hashable a => a -> Hashed a

hashByteArray         :: GHC.Prim.ByteArray# -> Int -> Int -> Int
hashByteArrayWithSalt :: GHC.Prim.ByteArray# -> Int -> Int -> Int -> Int

hashPtr         :: GHC.Ptr.Ptr a -> Int -> IO Int
hashPtrWithSalt :: GHC.Ptr.Ptr a -> Int -> Int -> IO Int

hashUsing    :: Hashable b => (a -> b) -> Int -> a -> Int
hashWithSalt :: Hashable a => Int -> a -> Int

mapHashed      :: Hashable b => (a -> b) -> Hashed a -> Hashed b
traverseHashed :: (Hashable b, Functor f) => (a -> f b) -> Hashed a -> f (Hashed b)
```
