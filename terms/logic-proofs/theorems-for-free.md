# Theorems For Free
(Theorems For Free - Philip Wadler, 1989)
https://www.youtube.com/watch?v=TGLmbl9x7s0&feature=emb_logo


* *Theorems for free* refers to deriving theorems about functions from their types only. An easy theorem-for-free asserts that the type `∀(X)X → Bool` i.e. `∀a. a -> a -> Bool` necessarily contains only constant functions.

* Given `f :: [a] -> [a]`, we can derive the theorem `f . map g = map g . f`. This is because `f` cannot do much, it cannot look at the elements, so it can only rearrange them structurally. f may be 'reverse', 'tail', 'init', (++), or some such function that works on the structure without involving elements.Therefore, mapping a function over a list and then rearranging its elements, is the same as rearranging the elements first and then mapping a function.

```hs
-- if
f :: [a] -> [a]
-- then
f . map g = map g . f
-- i.e.
f (map g xs) == map g (f xs)

-- examples:
-- 1) f = reverse, g = code :: Char -> Int, xs = ['a','b']
reverse (map code ['a','b']) == map code (reverse ['a','b'])

-- 2) f = tail, g = (+3), xs = [1,2,3]
tail (map (+3) [1,2,3]) == map (+3) (tail [1,2,3])

-- but f cannot be odds that filters odd integers
-- because it would be too specific, odds :: [Int] -> [Int]
odds . inc /= inc . odds
```

* Similar theorems can be derived for every type.

* The results that allows theorems to be derived from types is referred to as *the parametricity result* because it depends on parametric polymorphism, i.e. types of the form `∀a. T`

```hs
-- the function f is written
f :: [a] -> [a]
-- but actually, it is universally quantified
f :: forall a. [a] -> [a]
```

* The crucial matter is that types may be considered as relations (types are sets, but relations are also sets).

* Parametricity is just restated *Reynold's abstraction theorem*: 
terms evaluated in related environments produce related values.

* *The Parametricity Theorem*. If `a: t` is a closed term, 
then `(a, a) ∈ Relₜ`, i.e. every term is related to itself.

* *Strong normalization* ≝ every term has a normal form, and every term has a reduction sequence that leads to that normal form. A corollary of this theorem is that the fixpoint operator, `fix :: (a -> a) -> a`, cannot be defined as a term in the type systems of Hindley-Milner and Girard-Raynolds.
