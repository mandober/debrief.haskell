# Haskell :: Concepts :: Functor :: Functor laws

Like all the other classes based on abstract algebra, the `Functor` class also has a set of intrinsic laws that an implementing instance must satisfy. GHC doesn't enforce the class laws, so it's up to the programmer to make sure they are upheld.

Functor class has 2 laws and both are related to the characteristic property of functors, namely that they are a *structure-preserving maps*.

Preserving a structure when mapping it means that the mapping does not change the shape of the structure - only its contents. That is, the elements of the structure may change value and even type, but the overall shape of the structure must remain the same. The overall shape is characterized by the number of elements.

The structure corresponds to the type param `f` which is a unary type ctor of kind `* -> *`. So whatever the type ctor `f` is, it remains so before and after the mapping, `f a -> f b`. This means a list remains a list, even though it is free to change the type of args, e.g. from [String] to [Int], but it must keep the same number of elements.

In Haskell, unlike in category theory, the concept of functor boils dowm to the mapping function, called `fmap`.

The `fmap` is a HOF that takes a function argument, `f :: a -> b`, and uses it to map a structure `f a`, returning the mapped structure `f b`. For example

```hs
map (+1) [1,2,3] -- [2,3,4]
map length ["abc", "de", "f"] -- [3,2,1]
```


>A functor may change the type of the structure's elements, but it must not change the type of the structure itself - the "size" of the structure must remain the same; in Haskell, the type ctor `f` remain the same.


```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b

fmap id === id                    -- preserves identity
fmap g . fmap f === fmap (g . f)  -- preserves composition


Law 1: F 1ₘ = 1ₘ
Law 2: F (f ∘ g) = F f ∘ F g

LawHs 1: fmap id = id
LawHs 2: fmap (f ∘ g) = fmap f ∘ fmap g
```
