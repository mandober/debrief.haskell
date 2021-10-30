# Impredicative polymorphism

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/impredicative_types.html

- polymorphism
  - polymorphic type variables
    - instantiation of polymorphic type variables
      - at monomorphic types: predicative polymorphism
      - at polymorphic types: impredicative polymorphism


Generally, GHC will only instantiate *polymorphic type variables* at a *monomorphic type*, i.e. a type without `forall`s. Instantiating *polymorphic type variables* at *polymorphic types* is called **impredicative polymorphism**.


In general, GHC will only instantiate a polymorphic function at a monomorphic type (one with no foralls). For example,

```hs
runST :: (forall s. ST s a) -> a
id :: forall a. (a -> a)

foo = id runST   -- Rejected
```

The definition of `foo` is rejected because one would have to instantiate `id`'s type with `b := (forall s. ST s a) -> a`, and that is not allowed.

GHC has extremely flaky support for impredicative polymorphism, enabled with `ImpredicativeTypes`.

If it would worked properly, you could
- call a polymorphic function at a polymorphic type
- parameterise data structures over polymorphic types

For example:

```hs
-- Maybe is parameterised by a polymorphic type
f :: Maybe (forall a. [a] -> [a]) -> Maybe ([Int], [Char])
f (Just g) = Just (g [3], g "hello")
f Nothing  = Nothing
```

Notice how `Maybe` is parameterised by the *polymorphic type*, that is, by the type `forall a. [a] -> [a]`.

__WARNING__: the extension should be considered highly experimental, and certainly un-supported. You are welcome to try it, but please don't rely on it working consistently, or working the same in subsequent releases.

See this wiki page for more details:
https://gitlab.haskell.org/ghc/ghc/wikis/impredicative-polymorphism


If you want the effect of impredicative polymorphism without impredicativity, the main workaround is to use a newtype wrapper. The `id runST` example can be written using this workaround:

```hs
runST :: (forall s. ST s a) -> a
id :: forall b. b -> b

newtype Wrap a = Wrap { unWrap :: (forall s. ST s a) -> a }

foo :: (forall s. ST s a) -> a
foo = unWrap (id (Wrap runST))
      -- Here id is called at monomorphic type (Wrap a)
```


## Impredicative polymorphism

https://gitlab.haskell.org/ghc/ghc/-/wikis/impredicative-polymorphism

New plan for impredicativity, June 2015
https://gitlab.haskell.org/ghc/ghc/-/wikis/impredicative-polymorphism/impredicative-2015

See the ImpredicativeTypes label @GHC/Issues
https://gitlab.haskell.org/ghc/ghc/-/issues?label_name=ImpredicativeTypes
