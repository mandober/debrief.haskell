# Haskell records

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/records.html

How to use record-update syntax in Haskell (+ some dark corners)
https://www.youtube.com/watch?v=SdryTvPDhgM

- 6.5. Records
  - 6.5.1. Record field name resolution
  - 6.5.2. Traditional record syntax
  - 6.5.3. Field selectors and TypeApplications
  - 6.5.4. Record field disambiguation
  - 6.5.5. Duplicate record fields
  - 6.5.6. Field selectors
  - 6.5.7. Record puns
  - 6.5.8. Record wildcards
  - 6.5.9. Record field selector polymorphism
  - 6.5.10. Overloaded record dot
  - 6.5.11. Overloaded record update

## Haskell 98 record syntax

```hs
data T = MkT { foo :: Int, baz :: String }  -- record definition

x :: T
x = MkT { foo = 3, baz = "hi" }          -- construction

y :: T
y = x { foo = 3 } -- partial update
```

With *OverloadedRecordUpdate* the record update in this form above won't work.


## Haskell GHC2021 record syntax

- TraditionalRecordSyntax
- RecordWildCards
- OverloadedRecordUpdate
- OverloadedRecordDot
- DuplicateRecordFields
- DisambiguateRecordFields

{-# LANGUAGE NoRecordWildCards #-}
{-# LANGUAGE NoOverloadedRecordUpdate #-}
{-# LANGUAGE NoOverloadedRecordDot #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoDisambiguateRecordFields #-}
