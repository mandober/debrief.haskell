# Datatype promotion

- `DataKinds`
- since: 7.4.1
- Allow promotion of data types to kind level

This section describes *data type promotion*, an extension to the kind system that complements kind polymorphism. It is enabled by
`DataKinds`{.interpreted-text role="extension"}, and described in more
detail in the paper [Giving Haskell a
Promotion](http://dreixel.net/research/pdf/ghp.pdf), which appeared at
TLDI 2012.



Motivation
----------

Standard Haskell has a rich type language. Types classify terms and
serve to avoid many common programming mistakes. The kind language,
however, is relatively simple, distinguishing only regular types (kind
`Type`) and type constructors (e.g. kind `Type -> Type -> Type`). In
particular when using advanced type system features, such as type
families (`type-families`{.interpreted-text role="ref"}) or GADTs
(`gadt`{.interpreted-text role="ref"}), this simple kind system is
insufficient, and fails to prevent simple errors. Consider the example
of type-level natural numbers, and length-indexed vectors: :

    data Ze
    data Su n

    data Vec :: Type -> Type -> Type where
      Nil  :: Vec a Ze
      Cons :: a -> Vec a n -> Vec a (Su n)

The kind of `Vec` is `Type -> Type -> Type`. This means that, e.g.,
`Vec Int Char` is a well-kinded type, even though this is not what we
intend when defining length-indexed vectors.

With `DataKinds`{.interpreted-text role="extension"}, the example above
can then be rewritten to: :

    data Nat = Ze | Su Nat

    data Vec :: Type -> Nat -> Type where
      Nil  :: Vec a 'Ze
      Cons :: a -> Vec a n -> Vec a ('Su n)

With the improved kind of `Vec`, things like `Vec Int Char` are now
ill-kinded, and GHC will report an error.

Overview
--------

With `DataKinds`{.interpreted-text role="extension"}, GHC automatically
promotes every datatype to be a kind and its (value) constructors to be
type constructors. The following types :

    data Nat = Zero | Succ Nat

    data List a = Nil | Cons a (List a)

    data Pair a b = Pair a b

    data Sum a b = L a | R b

give rise to the following kinds and type constructors (where promoted
constructors are prefixed by a tick `'`): :

    Nat :: Type
    'Zero :: Nat
    'Succ :: Nat -> Nat

    List :: Type -> Type
    'Nil  :: forall k. List k
    'Cons :: forall k. k -> List k -> List k

    Pair  :: Type -> Type -> Type
    'Pair :: forall k1 k2. k1 -> k2 -> Pair k1 k2

    Sum :: Type -> Type -> Type
    'L :: k1 -> Sum k1 k2
    'R :: k2 -> Sum k1 k2

Virtually all data constructors, even those with rich kinds, can be
promoted. There are only a couple of exceptions to this rule:

-   Data family instance constructors cannot be promoted at the moment.
    GHC\'s type theory just isn't up to the task of promoting data
    families, which requires full dependent types.
-   Data constructors with contexts that contain non-equality
    constraints cannot be promoted. For example: :

        data Foo :: Type -> Type where
          MkFoo1 :: a ~ Int         => Foo a    -- promotable
          MkFoo2 :: a ~~ Int        => Foo a    -- promotable
          MkFoo3 :: Show a          => Foo a    -- not promotable

    `MkFoo1` and `MkFoo2` can be promoted, since their contexts only
    involve equality-oriented constraints. However, `MkFoo3`\'s context
    contains a non-equality constraint `Show a`, and thus cannot be
    promoted.

Distinguishing between types and constructors {#promotion-syntax}
---------------------------------------------

In the examples above, all promoted constructors are prefixed with a
single quote mark `'`. This mark tells GHC to look in the data
constructor namespace for a name, not the type (constructor) namespace.
Consider :

    data P = MkP    -- 1

    data Prom = P   -- 2

We can thus distinguish the type `P` (which has a constructor `MkP`)
from the promoted data constructor `'P` (of kind `Prom`).

As a convenience, GHC allows you to omit the quote mark when the name is
unambiguous. However, our experience has shown that the quote mark helps
to make code more readable and less error-prone. GHC thus supports
`-Wunticked-promoted-constructors`{.interpreted-text role="ghc-flag"}
that will warn you if you use a promoted data constructor without a
preceding quote mark.

Just as in the case of Template Haskell (`th-syntax`{.interpreted-text
role="ref"}), GHC gets confused if you put a quote mark before a data
constructor whose second character is a quote mark. In this case, just
put a space between the promotion quote and the data constructor: :

    data T = A'
    type S = 'A'   -- ERROR: looks like a character
    type R = ' A'  -- OK: promoted `A'`

Promoted list and tuple types {#promoted-lists-and-tuples}
-----------------------------

With `DataKinds`{.interpreted-text role="extension"}, Haskell\'s list
and tuple types are natively promoted to kinds, and enjoy the same
convenient syntax at the type level, albeit prefixed with a quote: :

    data HList :: [Type] -> Type where
      HNil  :: HList '[]
      HCons :: a -> HList t -> HList (a ': t)

    data Tuple :: (Type,Type) -> Type where
      Tuple :: a -> b -> Tuple '(a,b)

    foo0 :: HList '[]
    foo0 = HNil

    foo1 :: HList '[Int]
    foo1 = HCons (3::Int) HNil

    foo2 :: HList [Int, Bool]
    foo2 = ...

For type-level lists of *two or more elements*, such as the signature of
`foo2` above, the quote may be omitted because the meaning is
unambiguous. But for lists of one or zero elements (as in `foo0` and
`foo1`), the quote is required, because the types `[]` and `[Int]` have
existing meanings in Haskell.

::: {.note}
::: {.admonition-title}
Note
:::

The declaration for `HCons` also requires
`TypeOperators`{.interpreted-text role="extension"} because of infix
type operator `(':)`
:::

Promoting existential data constructors {#promotion-existentials}
---------------------------------------

Note that we do promote existential data constructors that are otherwise
suitable. For example, consider the following: :

    data Ex :: Type where
      MkEx :: forall a. a -> Ex

Both the type `Ex` and the data constructor `MkEx` get promoted, with
the polymorphic kind `'MkEx :: forall k. k -> Ex`. Somewhat
surprisingly, you can write a type family to extract the member of a
type-level existential: :

    type family UnEx (ex :: Ex) :: k
    type instance UnEx (MkEx x) = x

At first blush, `UnEx` seems poorly-kinded. The return kind `k` is not
mentioned in the arguments, and thus it would seem that an instance
would have to return a member of `k` *for any* `k`. However, this is not
the case. The type family `UnEx` is a kind-indexed type family. The
return kind `k` is an implicit parameter to `UnEx`. The elaborated
definitions are as follows (where implicit parameters are denoted by
braces): :

    type family UnEx {k :: Type} (ex :: Ex) :: k
    type instance UnEx {k} (MkEx @k x) = x

Thus, the instance triggers only when the implicit parameter to `UnEx`
matches the implicit parameter to `MkEx`. Because `k` is actually a
parameter to `UnEx`, the kind is not escaping the existential, and the
above code is valid.

See also `7347`{.interpreted-text role="ghc-ticket"}.
