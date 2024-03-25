# Covariant functor

## Rotten Bananas, by Edward Kmett 2008

Encoding a language that binds variables in higher order abstract syntax (HOAS) generally involves constructing an abstract data type that contains functions. A functor for representing expressions from Berendregdt's lambda cube in HOAS goes something like (ignoring any consolidation of binders and sorts)

```hs
data F a
    = Lam a (a -> a)
    | Pi a (a -> a)
    | App a a
    | Star
    | Box
```

There are a number of mathematical functors that are not instances of Haskell's Functor class, such as the one above.

If you don't believe me that `F` above is not a functor, try deriving the instance for it that satisfies the functor laws:
- fmap id = id
- fmap (f . g) = fmap f . fmap g

The reason it cannot be, is that fmap can really only be defined for *covariant endofunctors on the category of types* (Hask).

Most covariant functors used in Haskell are among the so-called *polynomial functors*, meaning they may be built out of sums, products and constants.

```hs
data Maybe a = Just a | Nil     -- covariant in a
data ListF t a = Cons t a | Nil -- covariant in a
```

That said, polynomial functors are not the only covariant functors, because you can also have some functions in the type, as long as the type which you are parameterized over only occurs in *positive position*.

The informal way of thinking about it is that every time you have a parameter on the left of an arrow, the occurrence switches signs, starting positive, so for some Functors, you can have functions, as long as the parameter occurs only in positive positions.

Some instances of Functor that are covariant but not polynomial:

```hs
newtype Reader e a = Reader (e -> a)    -- covariant in a
newtype Cont r a = Cont ((a -> r) -> r) -- covariant in a
```

On the other hand the following functors are not covariant, because the parameter occurs in negative position somewhere in the type.

```hs
data ToInt a = ToInt (a -> Int) -- contravariant
data Endo a = Endo (a -> a)     -- invariant
```

The class `Contravariant` is exactly for such contravariant functors:

```hs
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
```

But for HOAS you tend to need terms like `Lam (a -> a)` that have both positive and negative occurrences of `a` to handle variable binding, so we'll skip to a definition for an *invariant functor*, which we choose to call an *exponential functor* in contrast to a *polynomial functor*, because in category theory we refer to functions as exponentials and use the notation `bᵃ` to denote a function type `a -> b`.

```hs
class Exponential f where
  expmap :: (a -> b) -> (b -> a) -> f a -> f b
```

Now, every Functor is trivially an `Exponential`, witnessed by the default definition:

```hs
expmapF :: (Exponential f, Functor f) => (a -> b) -> (b -> a) -> f a -> f b
expmapF = const . fmap
```

And just as people are wont to do with `Functor` and `Monad` you could argue that in an ideal world the definition for Functor should change to class `Exponential f => Functor f`, but since not that many people use these things, I doubt anyone would be interested in the change.

This is a sufficiently general definition that you can construct instances for exponential data types such as:

```hs
instance ExpFunctor F where
  expmap f g (Lam t k) = Lam (f t) (f . k . g)
  expmap f g (Pi t k)  = Pi (f t) (f . k . g)
  expmap f g (App a b) = App (f a) (f b)
  expmap f g Star      = Star
  expmap f g Box       = Box
```

As an aside we can define exponential functor composition, just like functor composition if we want to:

```hs
newtype O f g e = Comp { deComp :: f (g e) }

instance (Functor f, Functor g) => Functor (f `O` g) where
  fmap f = Comp . fmap (fmap f) . deComp

instance (ExpFunctor f, ExpFunctor g) => ExpFunctor (f `O` g) where
  expmap f g = Comp . expmap (expmap f g) (expmap g f) . deComp
```


Typically we'd like to represent the recursive part of a functor with another ADT. This makes it easier to go through and apply things like catamorphisms and anamorphisms to them (see "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire" for more information). Catamorphisms are sometimes called bananas because of the notation from that paper.

A typical newtype used for explicit isorecursion is:

```hs
newtype Nu f = Nu { old :: f (Nu f) } -- so its not funny
-- i.e.
newtype Fix f = Fix { unFix :: f (Fix f) }
```

Now if `f` is a good old fashioned Functor, we can define a straightforward idea of a catamorphism over `Nu f`. I want to be able to handle `Exponential` later, so we'll leave the `Functor` constraint off of the class and move it to the instance.

```hs
class Cata f t | t -> f where
  cata :: (f a -> a) -> t -> a

instance Functor f => Cata f (Nu f) where
  cata f = f . fmap (cata f) . old
```


(...)



## Refs

http://comonad.com/reader/2008/rotten-bananas/
