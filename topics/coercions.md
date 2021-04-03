# Coercions
(Thinking with types)

In Haskell, newtypes are guaranteed to be a zero-cost abstraction. What this means is that, under the hood, a newtype has exactly the same memory representation as the type it wraps. At runtime, there is no difference between a newtype and its wrapped type. The distinction between the two is made up, and exists only in the type system.

```hs
newtype ZipList a = ZipList { getZipList :: [a] }
newtype Sum a = Sum { getSum :: a }

-- all these values are representationally equal
x1 = [54, 46]
x2 = [Sum 54, Sum 46]
x3 = ZipList [54, 46]
x4 = ZipList [Sum 54, Sum 46]
```

The last example shows that representational equality is transitive.

The zero-cost property of newtypes has profound implications for performance as it gives us the ability to reinterpret a value of one type as a value of another type, in O(0) time. This can be performed via the `coerce :: Coercible a b => a -> b` function, which is the method of the `Coercible` class.

```hs
class Coercible a b where
  coerce :: a -> b
```

The `Coercible` class is automatically implemented by the compiler for an underlying type `a` and its newtype wrapper, `b`. Unless explicitly prevented, a newtype is always coercible with its underlying type.

The `coerce` can be used to massage data from one type into another without paying any runtime cost. As an example, if we wanted to sum a list of Ints, we could use the `Sum Int` Monoid instance. But first we'd need to map the entire list with `Sum` function (ctor) to get monoids, before we can get their sum, and that is an O(n) cost operation. On the other hand, we can instead use `coerce` to transform `[Int]` into `[Sum Int]` in O(0) time, which will give us access to the right Monoid instance for free.

```hs
slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce
```

As a general rule, if you ever find yourself mapping with newtype's data ctor, it should be replaced with `coerce`.

Coercible representational equality should follow the axioms of equality:
* *Reflexivity*: `∀a. Coercible a a`
* *Symmetry*: `Coercible a b` <=> `Coercible b a`
* *Transitivity*: `Coercible a b` ∧ `Coercible b c` -> `Coercible a c`

By this line of reasoning, we expect we can coerce a `Sum` into a `Product`.

```hs
import Data.Monoid
import Data.Coerce

-- Int ≅ Sum Int ≅ Product Int
n1 = 43 :: Int
n2 = coerce n1 :: Sum Int
n3 = coerce n2 :: Product Int
n4 = coerce n3 :: Int
```

> However, representationally equal types are not always safely interchangeable.
