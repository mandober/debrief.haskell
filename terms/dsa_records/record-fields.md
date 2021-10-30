# Records

## The Problem

Defining records in Haskell causes accessor functions for the record fields to be defined, however they do not live in a separate namespace, which means no two field names can be the same within a module, and their names will also cause overshadowing with the names of other binders in the module.

This is a problem in general, but especially for common field names (e.g. `id`), forcing the developers to invent some ornaments to distinguish each field (`_id`, `id_`) or to prefix the fields with the record name (`rec_id`), but some of the resulting names will then be incompatible with, e.g. lens library.

# The Goal

Be able to
- use records in Haskell, which share field names
- use lenses for accessing these fields
- use bindings in code with the same name as the record fields avoiding any namespace clashes
- avoid overly verbose code and boilerplate as much as possible.

# The Solution Ingredients

## `DuplicateRecordsFields`

There is the GHC extension `DuplicateRecordFields, which allows the
definition of two records sharing the same field name:

```
{-# LANGUAGE DuplicateRecordFields #-}

data User  = User  { name :: Text, uid :: Int }
data Group = Group { name :: Text, gid :: Int }
```

The extentions allows the two records to share the field name `name`.

## `makeFieldsNoPrefix`

But what about lenses? We want to be able to access the fields with
lenses. We could generate lenses with `makeLenses` after prefixing all
record fields with an underscore:

```
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Lens

data User  = User  { _name :: Text, _uid :: Int }
data Group = Group { _name :: Text, _gid :: Int }

makeLenses ''User
makeLenses ''Group
```

This doesn't work, because each `makeLenses` tries to define a lense
named `name`. More suitable for this setup would be `makeFields`,
which uses type classes for defining lenses suitable for different
records.

But, `makeFields` expects the record fields to be prefixed not only
with an underscore, but also with the data type name. Thus we would
have to write something like

```
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE NoImplicitPrelude      #-}

import Control.Lens

data User  = User  { _userName :: Text, _userUid :: Int }
data Group = Group { _groupName :: Text, _groupGid :: Int }

makeFields ''User
makeFields ''Group
```

This works, but it increases the verbosity of the code, which
contradicts with one of our stated goals. Also, in this setup, we
sacrifice short record field names for short lens names — in fact we
wouldn't need `DuplicateRecordFields` anymore.

Lens' current master branch contains a function very similar to
`makeFields`, but it doesn't require the verbose prefixing of the
field names anymore (see
https://github.com/ekmett/lens/blob/master/src/Control/Lens/TH.hs). It
is called `makeFieldsNoPrefix`. Using this function we can write:

```
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE NoImplicitPrelude      #-}

import Control.Lens

data User  = User  { _name :: Text, _uid :: Int }
data Group = Group { _name :: Text, _gid :: Int }

makeFieldsNoPrefix ''User
makeFieldsNoPrefix ''Group
```

This provides us with the lens `name` which can be used for accessing
records of both types.

## Source Organization

Generating lenses this way only works for multiple records if the lens
generations using using `makeFieldsNoPrefix` (same for `makeFields`)
are concentrated in one module scope; If we had a module `User` and a
module `Group` both defining their records and generating lenses using
`makeFieldsNoPrefix`, each module would bring their own class
definition of `HasName`, which would clash in a module dealing with
both, users and groups.

In order to prevent these issues, I suggest to put all lens
definitions in one module named e.g. `Lenses`. This module imports all
desired types and uses `makeFieldsNoPrefix` for generting the lens
type classes and instances.

## Qualified Imports

This looks promosing so far. But what we cannot do yet is having local
bindings of the name `name` — this would shadow the existing lens of
the same name. Consider these functions:

```
createUser :: Text -> IO User
createUser name = do
  uid <- allocateUserId
  return User { _name = name, _uid = uid }

lookupUser :: Text -> [User] -> Maybe User
lookupUser userName users = listToMaybe $
  filter (\ user -> user ^. name == userName) users
```

In `createUser` we would shadow the lens `name`, but this would only
cause a compiler warning — we don't really need the lens there, so
it's not much of a problem. In `lookupUser` we use the lens `name`,
thus we have decided to go with the verbose local binding `userName`
instead of the shorter `name`. Alternatives would be to call the first
parameter to the function `name_` or `name'` or `n`. Personally, I
don't like this approach of continuously working around these name
clashes by somehow adjusting my binding's names.

For a more consistent workaround, I have decided to simply import my
`Lenses` module qualified, as in:

```
import qualified Lenses as Lens

lookupUser :: Text -> [User] -> Maybe User
lookupUser name users = listToMaybe $
  filter (\ user -> user ^. Lens.name == name) users
```

What this essentially means, that I define three namespaces:
- One for raw field names, prefixed with an underscore
- One for lenses, prefixed with `Lens.`
- One for everything else (local bindings, my functions, etc.)

# Summary

To summarize the above: the Haskell namespace collisioning problem
with regards to record field names is annoying, but the following
might pose a suitable workaround for some situations:

- Use the `DuplicateRecordFields` extension which allows removing
  overly verbose prefixing of record fields
- Prefix all record field names with an underscore
- For a given project, define lenses in one dedicated module for all records
  using `makeFieldsNoPrefix`
- Import this lens-defining module qualified

# Comments?

Please feel free to leave a comment. I would be curious to learn how
other people deal with these kind of issues.
