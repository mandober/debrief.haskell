# TypeInType

## Define Kinds Without Promotion #106
https://github.com/ghc-proposals/ghc-proposals/pull/106

Eeven with `TypeInType` we still have 2 relations:
- a typing relation, that associates type-level things with value-level things
- a kinding relation, that associates type-level things with other type-level things

So we have 3 combinations:
- plain `data` modifies only the typing relation
- promoted `data` (by `DataKinds`) modifies both relations
- the proposed `data kind` modifies only the kinding relation
- the proposed `data type` acts like the plain `data` even with `DataKinds`


So the `data kind` notation still makes sense.
