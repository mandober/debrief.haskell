# Haskell :: Category theory and Haskell :: Identities

## Identity morphisms

CT demands that each object has a unique *identity morphism*

`∀x ∈ C. ∃!1ₓ : x → x`

and this is true in Hask - each type has the identity function.

However, we usually talk about a single identity function, called `id`, which has the parameterically polymorphic type:

```hs
id :: forall a. a -> a
id x = x
```

*Parameteric polymorphism* is universal polymorphism without any constraints. A single definition relates to any and all types. It is similar to the formula above - the universally quantified variable `x`, that ranges over all objects of a category `C`, is both the source and the target of the single unique morphism, called the **identity morphism**, usually denoted by `1` or `id` and subscripted with the carrier object, `1ₓ` or `idₓ`.

A morphism that has identical objects for its source and target is called an **endomorphism**. There may be many endomorphisms on an object, but only one of them is the identity morphism.

A *small category* is one where the collection of all its objects and the collection of all its morphisms form sets. A *locally small category* is one where the collection of all morphisms forms a set. The category `Set`, which has sets as objects and functions on sets as morphisms, is a small category, and so is the `Hask` category. These two categories are very similar to each other and often compared.

The collection of arrows from an object `a` (source object) to an object `b` (target object) in a category `C` is called a **homset** and denoted `Hom(a,b)`. So, all endomorphisms on an object `a` form a homset `Hom(a,a)`, and the identity arrow `1ₐ` is an element in it, `1ₐ ∈ Hom(a,a)`. This also means that any collection of endomorphisms, i.e. any homeset like `Hom(a,a)`, always has at least one element.

Haskell's `id` function can be instantiated at any type to get the concrete identity morphism (function) at that type. This specializes the polymorphic `id` function to a monomorphic one on that type:

```hs
-- Identity arrow at Void, id_𝟘: 𝟘 → 𝟘
idVoid :: Void -> Void
idVoid = id @Void

-- Identity arrow at Unit, id_𝟙: 𝟙 → 𝟙
idUnit :: () -> ()
idUnit = id @()

-- Identity arrow at Bool, id_𝔹: 𝔹 → 𝔹
idBool :: Bool -> Bool
idBool = id @Bool

-- Identity arrow at Int, id_ℤ: ℤ → ℤ
idInt :: Int -> Int
idInt = id @Int

-- Identity arrow at [Int], id_ℤ⃰: ℤ⃰ → ℤ⃰
idListInt :: [Int] -> [Int]
idListInt = id @[Int]

-- Identity arrow at Int → Int, id_ℤᶻ: ℤ → ℤ
idIntInt :: (Int -> Int) -> (Int -> Int)
idIntInt = id @(Int -> Int)

-- Identity arrow at Int → Bool, id_𝔹ᶻ: ℤ → 𝔹
idIntBool :: (Int -> Bool) -> Int -> Bool
idIntBool = id @(Int -> Bool)
```
