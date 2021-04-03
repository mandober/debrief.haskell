# GHC contexts


https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#visible-type-application


- type signature of a function (variable)
- type signature of a class method
- type annotation on an expression
- data constructor declaration, using GADT syntax
  `data T a where MkT :: forall {k} (a :: k). Proxy a -> T a`
- existential type variable quantifications
  `data HList = HNil | forall {a}. HCons a HList`
- pattern synonym signatures
  data T a where MkT :: forall a b. a -> b -> T a
  pattern Pat :: forall {c}. () => forall {d}. c -> d -> T c
  pattern Pat x y = MkT x y
- the right-hand side of a type synonym
  type Foo = forall a {b}. Either a b
- type signatures on variables bound in RULES
  {-# RULES "parametricity" forall (f :: forall {a}. a -> a). map f = id #-}
- visible dependent quantifiers
  data T :: forall {k} -> k -> Type
- SPECIALISE pragmas or in instance declaration heads
  instance forall {a}. Eq (Maybe a) where ...
- left-hand sides of type declarations (such as classes, data types, etc.)

- left of a function arrow
  f :: Int -> (forall a. a -> a)
- right of a function arrow
  g :: Int -> Ord a => a -> a
- arg of a ctor in a data type declaration
- type of a field in a data type declaration
- the type of an implicit parameter
- pattern type signature
