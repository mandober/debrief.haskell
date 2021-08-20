# Parametricity

* Parametricity is just restated *Reynold's abstraction theorem*: 
terms evaluated in related environments produce related values.

* *The Parametricity Theorem*. If `a: t` is a closed term, 
then `(a, a) ∈ Relₜ`, i.e. every term is related to itself.

* *Theorems for free* refers to deriving theorems about functions from their types only. An easy theorem-for-free asserts that the type `∀(X)X → Bool` i.e. `∀a. a -> a -> Bool` necessarily contains only constant functions.

* The results that allows theorems to be derived from types is referred to as *the parametricity result* because it depends on parametric polymorphism, i.e. types of the form `∀a.T`


Parametricity constrains possible behaviours of a function: the more types a function accepts, the less it can do.

The canonical example is the `id` function which has only one reasonable implementation. What unreasonable implementations are possible?

```hs
foo :: a -> a

-- reasonable implementation:
foo x = x

-- unreasonable implementations:
foo = undefined
foo = error "error"
foo = foo
foo _ = undefined
foo x = const undefined x
foo x = const (foo x) x
foo x = fix (foo)
-- etc.

-- unsafe implementations:
foo a = unsafePerformIO $ do
  x <- stike
  putStrLn $ "Strike!" ++ x
  return x
```
