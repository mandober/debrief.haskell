# Compiler extension

Enabling compiler extensions via pragmas

To use a language feature put a pragma at the BOF:

```hs
{-# LANGUAGE ForeignFunctionInterface #-}  -- enables FFI
{-# LANGUAGE FlexibleContexts #-}          -- flexible type consstraints
```
