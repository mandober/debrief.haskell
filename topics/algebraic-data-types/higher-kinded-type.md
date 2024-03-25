# Higher-kinded types

(HKT, Higher-kinded types, Higher-kinded datatypes)

* A **parameterized type** is a type with a type parameter, usually of the kind `Type`, such as

```hs
type Param :: Type -> Type
newtype Param a = Param { unParam :: a }
```

* An **indexed type** is a parameterized type (has a type parameter of the kind `Type`) but also another type parameter, usually of some promoted data kind like `Nat`, exemplified by the `Vector` type:

```hs
type Vec :: Nat -> Type -> Type
data Vec (n :: Nat) (a :: Type) where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a
```

In `Vec n a`, `a` is a value-type parameter, and `n` is an index (type of the kind `Nat` that represent their length), so we say that vectors are parameterized types (can contain any type as elements) indexed by their length. Indexed types are types indexed by another type; they are said to form a family of types.

* **Higher-kinded types** are also parameterized, but unlike the types above that aim to parameterize elements (range of contained values), these aim to parameterize a range of possible containers those elements could be in. HKTs are more often realized as classes, such as `Functor`, `Applicative`, `Monad`, all of which are the so-called *constructor type classes* because their type parameter targets the unary type ctors of kind `Type -> Type` (the fact that must be made explicit as a kind signature, unless it can be infered from the class' methods).

```hs
type Functor :: (Type -> Type) -> Constraint
class Functor (f :: Type -> Type) where
  fmap :: a -> b -> (f a -> f b)

-- the eligable carriers are unary type ctors:
instance Functor []
instance Functor Maybe
instance Functor (Either r)
instance Functor ((->) r)
instance Functor ((,) r)
instance Functor IO
```

HKTs may directly parameterize the goal type - the type ctor - which is usually some unary type ctor, and leave the value type as is (parameterized or not). For example, a record type may have fields as bare fields or wrapped in auxillary type like `Maybe` or `Either`. Then, parameterizing the record by the type ctor `f`, would allow us to express all very similar versions of this record by a single record type, e.g.

```hs
data HKT f = HKT { owner :: f String }
```

where `f` stands for no type at all (∅), or for `Maybe`, or for `Either Err`, or for `IO`.

>The type param `f` abstracts the container type - it ranges over different container-like types

(which are under your control, anyway).


HKTs parameterize things one level further - besides being parameterized by the type of value they may contain (generic elements), they are also parameterized by the type of containers (generic containers).

```hs
newtype Container f a = Container { unContainer :: f a }

-- its regular kind signature:
type Container :: (Type -> Type) -> Type -> Type

-- or, a more polymorphic kind signature:
type Container :: forall {k}. (k -> Type) -> k -> Type
```

## HKTs

A **higher-kinded type** is a type that abstracts over a type that, in turn, abstracts over another type. It's a way to generically abstract over entities that take type ctors. HKTs are type vars with args.
- HKTs should not be confused with *higher-order types*.
- HKT allows functions (operators) between types (from type to type), these type-level functions are called *type operators*.

When we declare a class like `Num`, then we dig into the ground types (value types like Int, Bool, Char, Float, …), declaring instances for the suitable ground types. `Num` really is ad hoc. But when we want to focus on list, we don't really care what those lists contain - we want to explore a list as a type ctor. At the type ctor level. Without getting into the details of their elements. Without bothering to specify them; e.g. asking the ghci for the kind of some puzzle of a nested list type may end up in having to specify list elements, even though they are completely irrelevant (so use `_`) - but now list this problem to the type level - we want to parameterized not only over the value type, but also over the container type - so we use HKT.

## Examples

```hs
-- binary type-ctor T2 takes a unary type-ctor (Type -> Type) and a (Type)
type T2 :: (* -> *) -> * -> *
data T2 f a = T2 (f a)

:k T2                 :: (* -> *) -> * -> *

:k T2 Maybe           :: -> * -> *
:k T2 Maybe Int       :: *
:k T2 Maybe Maybe     -- ERROR: Expected one more arg to Maybe
:k T2 Maybe (Maybe _) :: *

:k T2 []              :: -> * -> *
:k T2 [] Int          :: -> *
:k T2 [Int]           -- ERROR: Expected kind Type -> Type
:k HKT [] [Int]       :: *
```

## Polymorphic records

Consider a record type of 2 fields (e.g. `owner :: String`, `stars :: Int`). Ideally, both fields are always valid and known, but what if they aren't? One option is to have a similar record but with `Maybe` fields to account for mishaps. And an overkill with another record that wraps the fields in the `Either` type instead (maybe made before the desire to annotate the errors kicked in). So, you's have 3 very similar record types, with the fields that differ in wrappers.

```hs
-- instead of these 3 similar record typer
data Repo0 = Repo0 { owner :: String
                   , stars :: Int
                   }

data Repo1 = Repo1 { owner :: Maybe String
                   , stars :: Maybe Int
                   }

data Repo2 = Repo2 { owner :: Either Err String
                   , stars :: Either Err Int
                   }

-- we just have this single HKT
data Repo f = Repo { owner :: f String
                   , stars :: f Int
                   }
```







## References

https://en.wikipedia.org/wiki/Kind_(type_theory)
http://dev.stephendiehl.com/fun/001_basics.html
https://www.stephanboyer.com/post/115/higher-rank-and-higher-kinded-types

https://www.youtube.com/watch?v=sIqZEmnFer8&list=PLxop3c9esmBkWdwggyOASRu5kujRuRl-4&index=1

http://blog.poisson.chat/posts/2017-10-21-making-a-show.html
https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html
https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints
https://blog.poisson.chat/posts/2018-03-03-deriving-show-hkt.html
