# LANGUAGE pragma

- name: LANGUAGE
- type: file-header pragma
- tag: `{-# LANGUAGE ext [, ext] #-}`

LANGUAGE pragma allows language extensions to be enabled in a portable way. It should be used instead of `OPTIONS_GHC`. For example,

```hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
```

Every language extension also has a command-line flag, by prefixing it with '-X'. Dually, all '-X' flags can be written as LANGUAGE pragmas.

To list all supported language extensions: `ghc --supported-extensions`.

Any extension from the `Extension` type defined in `Language.Haskell.Extension` may be used. GHC will report an error if any of the requested extensions are not supported.

https://downloads.haskell.org/~ghc/latest/docs/html/libraries/Cabal-3.6.0.0/Language-Haskell-Extension.html
