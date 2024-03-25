# OPTIONS_GHC pragma

- name: OPTIONS_GHC
- type: file-header pragma
- tag: `{-# OPTIONS_GHC opt [, opt] #-}`

```hs
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
```

The `OPTIONS_GHC` pragma is used to specify additional options that are given to the compiler when compiling this source file, see

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#source-file-options

Previous versions of GHC accepted OPTIONS (rather than OPTIONS_GHC), but that is now deprecated.
