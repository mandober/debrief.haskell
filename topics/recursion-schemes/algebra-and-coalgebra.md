# Algebra and coalgebra

The notion of algebra and coalgebra in Haskell is imported from category theory.

```hs
type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
```



## List

The standard list type, `List a` or `[a]` is a recursive type since the right hand side of the declaration mentions the type being defined, i.e. `List`:

```hs
data []   a = Nil | (:)  a ([]   a)
data List a = Nil | Cons a (List a)
```

The type parameter `a` here signifies the type of list elements. The `Cons` data ctor actually takes a pair of values of two different types, `a` and `r`, where `r` happens to be the type `List a`. Since `a` stands for any type, we might as well instantiate it to the type `List a` (we substitute each `a` in the declaration of list with `List a`):

```hs
data List a               = Nil | Cons a               (List a)
data List (List a)        = Nil | Cons (List a)        (List (List a))
data List (List (List a)) = Nil | Cons (List (List a)) (List (List (List a)))
-- ...
```

We cannot write such type, but it suggest it is going to infinity. Going back from infinity to the beginning we find that the regular `List` is the least fixpoint of the `ListF` functor.






-- | A non-recursive list as the initial algebra ListF.
--   Regular List is the least fixpoint of the ListF functor.
--   It is as if we instantiated the type param `a` in List with
--   the type `List a` getting an infinite type.
data List a r = Nil | Cons a r deriving (Show, Functor)

-- | `project` converts a regular list [a] into a `ListF a r`, where `r`
--   is instantiated with the type [a], so from [a] into `ListF a [a]`.
--
--   Generically, this is a coalgebra,  a  -> f        a
--   where a = [a], f = ListF a, thus, [a] -> ListF a [a]
--   which is why we needed a Functor instance for ListF.
--
--   project is a way to view list-likes structure ListF as
--   a canonical (initial) structure (i.e. as a List) for this type.
project :: [a] -> List a [a]
project []     = Nil
project (x:xs) = Cons x xs

-- | `embed` converts a `ListF a r`, where `r` is instantiated
--   with the type [a], so from `List a [a]`, into a regular list [a].
--
--   Generically, this is an algebra, f a -> a,
--   which is why we needed a Functor instance for ListF.
-- 
--   `embed` is dual of `project`. `project` and `embed` are isomorphisms.
embed :: List a [a] -> [a]
embed Nil         = []
embed (Cons x xs) = x:xs
