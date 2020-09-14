# Kinds

https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/


```hs
> :k {expr}

Int   :: *
Maybe :: * -> *
[]    :: * -> *
(->)  :: * -> * -> *

((,))   :: * -> * -> *
((,,))  :: * -> * -> * -> *
((,,,)) :: * -> * -> * -> * -> *
((,,,) Bool) :: * -> * -> * -> *

Either :: * -> * -> *
Either String :: * -> *
Either String Int :: *

Show :: * -> Constraint
Show Int :: Constraint
Functor :: (* -> *) -> Constraint
Functor IO :: Constraint
StateT :: * -> (* -> *) -> * -> *
instance forall k (f :: k -> *) (a :: k). Num (f a) => Num (Alt f a)

:set -XMagicHash
import GHC.Prim
Int#  :: TYPE 'GHC.Types.IntRep
Char# :: TYPE 'GHC.Types.WordRep

'GHC.Types.WordRep :: GHC.Types.RuntimeRep
TYPE :: GHC.Types.RuntimeRep -> *
```




In Haskell, concrete types have kind `*`. For example, these types have kind `*` (aka `Type`): `Int`, `[Int]`, `Int -> String`, `Either Int Int`, `a`, `Maybe Int`, `Void`, etc. They are also called **saturated types**.

Type ctors are similar to functions

Not all types are inhabited though. `Maybe` and `Either`, for example, are uninhabited. There is no term of type `Maybe`, not even a diverging term. Maybe and Either are not inhabited types but type constructors.

Either is a type constructor that, given two types a and b of kind *, creates another type of kind *. Either is a type of kind * -> * -> *.

Just like data constructors are curried and can be partially applied, so can type constructors.

```hs
λ> :t MkPerson
MkPerson :: String -> Int -> Person
λ> :t MkPerson "Diogo"
MkPerson "Diogo" :: Int -> Person
λ> :t MkPerson "Diogo" 29
MkPerson "Diogo" 29 :: Person

λ> :k Either
Either :: * -> * -> *
λ> :k Either String
Either String :: * -> *
λ> :k Either String Int
Either String Int :: *
```

Just like GHC is able to correctly infer the types of variables, it is also able to correctly infer the kinds of type variables.

```hs
-- The inferred kind of `a` is `*`
data List a = Cons a (List a) | Nil

-- The inferred kind of `f` is `* -> *`
-- The inferred kind of `a` and `b` is `*`
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

And just like you can manually specify a variable's type, you can also manually specify a type variable's kind using the *KindSignatures* extension.

```hs
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

data List (a :: *) = Cons a (List a) | Nil

class Functor (f :: * -> *) where
  fmap :: forall (a :: *) (b :: *). (a -> b) -> (f a -> f b)
```

The *ExplicitForAll* extension allows us to define each type variable explicitly.


## HOFs and HKTs

Just like we can have higher-order functions (HOFs), functions that take other functions as arguments, we can also have higher-kinded types (HKTs), types constructors that take other type constructors as arguments.

```hs
-- map takes a function of type (a -> b) and a list of type [a]
map :: (a -> b) -> [a] -> [b]

data NonEmpty f a = MkNonEmpty { head :: a, tail :: f a }

λ> :k NonEmpty
NonEmpty :: (* -> *) -> * -> *

λ> :t MkNonEmpty True [False, True]
MkNonEmpty True [False, True] :: NonEmpty [] Bool
```

Similarly, `NonEmpty` is a type constructor that takes another type constructor of kind `(* -> *)` and a type of kind `*`.

When applied to `[]` and `Bool`, we obtain the type `NonEmpty [] Bool`, a list of Boolean values that is guaranteed to have at least one value.

We can apply this type constructor to any two types, so long as their kinds match the expected kinds, such as `NonEmpty [] Int`, `NonEmpty Tree String` or `NonEmpty Vector Char`.










---

For example, `Either` is a type with kind `Either :: * -> * -> *`, which signifies that its arity on the type level is 2, i.e. it has two type variables. By partially applying `Either` to `String` produces the partially applied type `Either String` whose kind looses an "arity level" becoming `Either String :: * -> *`. When it receives the second type parameter, e.g. `Int` it becomes complete, `Either String Int`.

```hs
String :: *

Either :: * -> * -> *

Either String :: * -> *

Either String Int :: *
```

Type constructors are applied to types in type declarations, just like functions are applied to values in function definitions.


---

Values like `3::Int`, `'c':: Char`, `Int -> Bool`, or even `a -> a` are *concrete values*, each with the concrete type. Values inhabit a *type*, like elements inhabit a set (in set theory). Only here, we also have *kinds* that types inhibit.


A `*` means that the type is a concrete type. A concrete type is a type that doesn't take any type parameters and values can only have types that are concrete types.


```hs
ghci> :k Int
Int :: *

-- Maybe is a type ctor expecting a type param:
ghci> :k Maybe
Maybe :: * -> *

-- when TP is applied:
ghci> :k Maybe Int
Maybe Int :: *

-- that's similar to a fn:
ghci> :t isUpper
isUpper :: Char -> Bool

-- but when the value is applied:
ghci> :t isUpper 'A'
isUpper 'c' :: Bool
```

`Maybe` type constructor takes one concrete type (e.g. `Int`) and then returns a concrete type, e.g. `Maybe Int`.

Just like `Int -> Int` means that a function takes an Int and returns an Int, 
alike `* -> *` means that the type constructor takes one concrete type and returns a concrete type.

When the type parameter is applied to Maybe the kind of that type is `*` again.


The kind of `Either` is `* -> * -> *`

```hs
ghci> :k Either
Either :: * -> * -> *
```

This tells us that `Either` takes 2 concrete types as type parameters to produce a concrete type.

It looks like a sog of a function that takes 2 values and returns one.


**Partially applying type constructors**   
Type constructors are curried (the same as functions), so we can partially apply them:

```hs
ghci> :k Either String
Either String :: * -> *

ghci> :k Either String Int
Either String Int :: *
```

Only types are eligable to be queried about their kind:
 
```hs
ghci> :k Bool
Bool :: *

ghci> :k Eq
Eq :: * -> Constraint

ghci> :k Functor 
Functor :: (* -> *) -> Constraint

ghci> :k Monad
Monad :: (* -> *) -> Constraint
```


## Constraints

There's also the `Constraint` kind that applies to everything on LHS of a => arrow, including typeclass constraints:

```hs
> :k Show
Show :: * -> Constraint

> :k Show Int
Show Int :: Constraint

> :k Functor
Functor :: (* -> *) -> Constraint

> :k Functor IO
Functor IO :: Constraint
```
