# Laws


## Semigroup

Semigroup instances must uphold the axioms of:
- Closure
- Associativity


## Monoids

Monoid instances must uphold the axioms of:
- Closure
- Associativity
- Identity

```hs
-- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z
mconcat = foldr mappend mempty
```

## Functions

```hs
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c infixl 0
```

`on b u x y` runs the binary function `b` on the results of applying unary function `u` to two arguments `x` and `y`.

From the opposite perspective, it transforms two inputs and combines the outputs.

```hs
((+) `on` f) x y = f x + f y


(g `on` f) x y = (f x) `g` (f y)

-- (g `on` f) x y = (f x) `g` (f y)
-- mapOn :: 
mapOn g f x y = (f x) `g` (f y)

((:) `on` map) [] [[]] = (map x) : (map y)

```

Typical usage: sortBy (compare `on` fst)

**Algebraic properties**

```hs
(*) `on` id ≡ (*)
-- (if (*) ∉ {⊥, const ⊥})

((*) `on` f) `on` g ≡ (*) `on` (f . g)

flip on f . flip on g ≡ flip on (g . f)
```
