# Indexed list

The indexed list, more commonly and misleadingly (since vector is usually supported by an array on the heap) called *vector*, is the plain old linked list adorned with a type index, which tracks its length.

The length of a vector is recorded at the type level only, using a suitable type as the index. We say that a vector is *parameterized* by a type `a` and *indexed* by a type `n`, where both `a` and `n` are type variables. However, while `a` can range over any (inhabited) type, the type variable `n` should range over a type that can be confortably used as index. This implies that we need means to manipulate that type at the type level, to easily construct a new instance of that type based on the old one - therefore, the indexing type is really a type family.

The ideal type for this purpose are the natural numbers, more so because the length of a vector cannot ever be negative. We'd like to somehow "lift" the naturals to the type level.


at the type level.


Lists are parameterized by any type, which is represented by a type variable like `a` in the type `List a`. 

However, `a` cannot really be any type - it can range only over the inhabited types, which are classified by the `Type` kind (ex `*`) as specified (although often implicitly, in which case it depends on the enabled extensions) in the standalone kind signature.

```hs
type List :: Type -> Type
data List a = Nil | Cons (List a)

-- or, as a GADT:
type List :: Type -> Type
data List a where
  Nil  ::                List a
  Cons :: a -> List a -> List a
```

To augment the list with a type that can track its length, we need a suitable type as the index, and the type of natural numbers are ideal since the length of a list (vector) cannot possibly be negative.
