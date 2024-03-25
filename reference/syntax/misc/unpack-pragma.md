

## UNPACK pragma and the bang pattern

```hs

data Set a = Bin {-# UNPACK #-} !Int
             a
             !(Set a)
             !(Set a)
```
