# Haskell Classes :: IsList

- pakage: `base`
- module: GHC.Exts

- https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Exts.html
- https://hackage.haskell.org/package/base/docs/src/GHC.IsList.html
- https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.IsList.html

## Intro

This class groups the list-like data types so they can also take advantage of using the *bracket syntax* to denote their literal values.

By declaring an instance of this class, 
and enabling the *OverloadedLists* extension, 
a data type can use the bracket syntax (at the term level).

## Brackets

Normally, the brackets are reserved symbols, intended for exclusive use by the builtin list, which uses the brackets nominally, but also as a shorthand, both at type and term level.

At the type level, `[]` is the name of the list type ctor, so [a] is the type of generic lists; in fact, that type is properly writen as `[] a`, but this can often (although not always) be shortened to [a]. Alas, the type denotation [a] is not as clear as `List a`.

At the term level, the brackets are welcomed: instead of writing a list literal in its proper form as `1 : 2 : 3 : []`, we write it shorthand as [1,2,3]. At the term level, `[]` is the name of the nullary list data ctor, aka "nil".

## IsList class

- the subject is a saturated type ctor, i.e. has kind `Type`.
- the associated type, `Item`, is a type family.


```hs
class IsList l where
  type Item l

  fromList :: [Item l] -> l

  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList

  toList :: l -> [Item l]
```

* `Item l`
`Item` is an associated type, i.e. a function on types that returns the type of elements of the structure `l`.

* `fromList`
Constructs the structure `l` from the given list of its elements, i.e. from the `[Item l]`.

* `fromListN`
Takes the input list's length and potentially uses it to construct the structure `l` more efficiently compared to `fromList`. If the given number does not equal to the input list's length the behaviour of `fromListN` is undefined.

Proposition: `fromListN (length xs) xs == fromList xs`

* `toList`
Extracts a list of `Item l` from the structure `l`. It should satisfy the identity law, i.e. it should be isomorphic, `fromList . toList = id`.


## Example instance


- defining the associated type as `a` enables us to replace `Item l` with `a` in the type signatures of the methods (although both sigs are valid).

```hs
data List a = Nil | Cons a (List a)


instance IsList (List a) where
  type (Item (List a)) = a

  -- fromList :: [Item l] -> l
  fromList :: [Item l] -> List a
  fromList :: [a] -> List a
  fromList [] = Nil
  fromList (x:xs) = Cons x (fromList xs)

  -- toList :: l -> [Item l]
  toList :: List a -> [Item l]
  toList :: List a -> [a]
  toList Nil = []
  toList (Cons x xs) = x : toList xs
```

When we define the associated type, e.g. as `a` as in `type (Item (List a)) = a`, we can treat `Item l` interchangibly with `a` in the type signatures of the methods - both types are valid.

This is not the case with the variable `l` that is the subject of the class: when we declare the instance for, e.g. `List a`, we must replace all `l`'s in the signatures of the methods with `List a` (that is, if we want to write the signatures in the first place). Because explicitly specifying the signatures of the methods in an instance declaration *specializes the types*, which is subject to a bunch of rules (that can be bend).

e.g. we can use both `Item l` and `a` in the `fromList` signature:

```hs
  fromList :: [Item l] -> List a
  fromList :: [a] -> List a
```


## Instances

```hs
instance IsList [a] where
  type (Item [a]) = a
  fromList = id
  toList = id

-- @since 4.15.0.0
instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList = ZipList
  toList = getZipList

-- @since 4.9.0.0
instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList (a:as) = a :| as
  fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"
  toList ~(a :| as) = a : as

-- @since 4.8.0.0
instance IsList Version where
  type (Item Version) = Int
  fromList = makeVersion
  toList = versionBranch

-- @since 4.9.0.0
-- | Be aware that 'fromList . toList = id' holds only for unfrozen 'CallStack's, since 'toList' removes frozen-ness information.
instance IsList CallStack where
  type (Item CallStack) = (String, SrcLoc)
  fromList = fromCallSiteList
  toList   = getCallStack
```
