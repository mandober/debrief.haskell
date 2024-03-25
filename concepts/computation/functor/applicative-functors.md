# Applicative functors

```hs
-- covariant functors
fmap  :: Functor           f =>   (a -> b)   -> (f a -> f b)
(<*>) :: Applicative       f => f (a -> b)   -> (f a -> f b)
(=<<) :: Monad             f =>   (a -> f b) -> (f a -> f b)
-- contravariant functors
contramap :: Contravariant f =>   (b -> a)   -> (f a -> f b)
divide    :: Divisible     f => f (b -> a)   -> (f a -> f b)

-- Applicative: actual and fundamental definitions
pure     :: Applicative f => a  -> f a
pureUnit :: Monoidal    f => () -> f ()
(<*>)    :: Applicative f => f (a -> b)    -> (f a -> f b)
fzipWith :: Monoidal    f => ((a, b) -> c) -> ((f a, f b) -> f c)
divide   :: Divisible   f => (a -> (b, c)) -> ((f b, f c) -> f a)
```

Since `->` is also a type (object in category Hask), morphisms of Hask are objects of it too. In light of this, it seems an applicative functor is a functor that also can map "arrow-objects" of source category into morphisms of the destination one.

Applicatives translate less straightforwardly than Functors or Monads, but in essence, it is the class of *monoidal functors*.


https://stackoverflow.com/questions/35013293/what-is-applicative-functor-definition-from-the-category-theory-pov

The key to understanding applicative functors is to figure out what structure they preserve. Regular functors preserve the basic categorical structure: they map objects and morphisms between categories, and they preserve the laws of the category (associativity and identity).

But a category may have more structure; for instance, it may allow the definition of mappings that are like morphisms but take multiple arguments. Such mappings are defined by currying (e.g. a function of two arguments is defined as a function of one argument returning another function). In general, this is possible if you can define an object that represents a function type, called *exponential object* (in Haskell, it is just the function type `a -> b`). We can then have morphisms from an object to an exponential object and call it a two-argument morphism.

The traditional definition of an applicative functor in Haskell is based on the idea of mapping n-ary functions (with n > 1). But there is an equivalent definition that splits the multi-argument function along a different boundary. You can look at such a function as a mapping of a product to another type:   
`a -> (b -> c)` ≅ `(a, b) -> c`

This allows us to consider applicative functors as functors that preserve the product; but, a product is just one example of what is called a monoidal structure.

In general, a *monoidal category* is a category equipped with a *tensor product* and a *unit object*. In Haskell, this could be, for instance, the cartesian product (a pair) and the unit type, `()`. Notice, however that monoidal laws (associativity and unit laws) are valid only up to an isomorphism. For instance, `(a, ())` ≅ `a`.

An **applicative functor** could then be defined as a functor that preserves monoidal structure. In particular, it should preserve the unit and the product.

It should not matter whether we do the "multiplication" before or after applying the functor. The results should be isomorphic.

However, we don't really need a full-blown monoidal functor. All we need is two morphisms (as opposed to isomorphisms) - one for multiplication and one for unit. Such a functor that half-preserves the monoidal structure is called a **lax monoidal functor**. Hence the alternative definition:

```hs
class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a, b)
--  (**) :: (f a, f b) -> f (a, b)               (2)
--  (**) :: ((a, b) -> c) -> (f a, f b) -> f c   (3)

-- (2) uncurryied form
-- (3) perhaps even "deeper" variant reflecting that the functor
--     translates the monoidal structure of one category to that of another
--     (though both categories are Hask here)
```

It's easy to show that Monoidal is equivalent to Applicative. For instance, we can get `pure` from `unit` and vice versa:

```hs
unit = pure ()
pure x = fmap (const x) unit
fs <*> xs = fmap (uncurry ($)) (fs ** xs)
```

The applicative laws follow naturally from the preservation of monoid laws (associativity and unit laws).

In category theory, preservation of monoidal structure is related to tensorial strength, so an applicative functor is also known as a *strong lax monoidal functor*. However, in Hask, every functor has canonical strength with respect to the product, so this property doesn't add anything to the definition.

Now, if you're familiar with the definition of a monad as a monoid in the category of endofunctors, you might be interested to know that, similarly, *applicatives are monoids in the category of endofunctors* where the tensor product is the Day convolution (but that's much harder to explain…).
