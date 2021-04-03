# Data.ByteString.Base64

```hs
import Data.ByteString.Base64

import qualified Data.ByteString.Base64 as Base64 (decode, encode)


encode        :: Data.ByteString.Internal.ByteString
              -> Data.ByteString.Internal.ByteString

decode        :: Data.ByteString.Internal.ByteString
              -> Either String Data.ByteString.Internal.ByteString

decodeLenient :: Data.ByteString.Internal.ByteString
              -> Data.ByteString.Internal.ByteString

joinWith      :: Data.ByteString.Internal.ByteString
              -> Int
              -> Data.ByteString.Internal.ByteString
              -> Data.ByteString.Internal.ByteString
```
