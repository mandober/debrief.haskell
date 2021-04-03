# Traversable class

http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Traversable.html

**Functor** gives us a way to transform any values embedded in structure.

**Applicative** is a *monoidal functor*, and gives us a way to transform any values contained within a structure using a function that is also embedded in structure. This means that each application produces the effect of adding structure which is then applicatively combined.

**Foldable** gives us a way to process values embedded in a structure as if they existed in a sequential order (e.g. list folding).

**Traversable** allows us to transform elements inside a structure like a Functor, producing Applicative effects along the way, and lift those (potentially multiple) instances of Applicative structure outside of the Traversable structure. Traversable depends on Applicative (and thus Functor) and is also superclassed by Foldable.

Traversable is commonly described as a way to traverse a data structure without changing it, mapping a function inside the structure while accumulating the applicative contexts along the way.

Traversable represents data structures which can be traversed while perserving the shape. This is why there is no filter or concatMap, since Traversable only defines a way to move through the data structure, but not a way to change it.


## Traversable class definition

```hs
-- defined in Data.Traversable
class (Functor t, Foldable t) => Traversable t where
    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f =>    t (f b)        -> f (t b)

    mapM      :: Monad       f => (a -> f b) -> t a -> f (t b)
    sequence  :: Monad       f =>    t (f b)        -> f (t b)
-- MINIMAL: traverse | sequenceA
```


## Traversable class default implementation

Class of data structures that can be traversed from left to right, performing an action on each element.

```hs
-- defined in Data.Traversable
class (Functor t, Foldable t) => Traversable t where

    -- Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results.
    -- For a version that ignores the results see: Data.Foldable.traverse_
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    -- Evaluate each action in the structure from left to right, and
    -- collect the results. For a version that ignores the results see:
    -- Data.Foldable.sequenceA_
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

-- MINIMAL traverse | sequenceA
```

## Papers about Traversable

* "Applicative Programming with Effects" by Conor McBride and Ross Paterson, 
   [Journal of Functional Programming](http://www.soi.city.ac.uk/~ross/papers/Applicative.html)
 * "The Essence of the Iterator Pattern" by Jeremy Gibbons and Bruno Oliveira, 
   in [Mathematically-Structured Functional Programming](http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator), 2006
 * "An Investigation of the Laws of Traversals" by Mauro Jaskelioff and Ondrej Rypacek, in [Mathematically-Structured Functional Programming](http://arxiv.org/pdf/1202.2919), 2012


## sequenceA

```hs
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```

From the signature of `sequenceA` we see that its effect is flipping two contexts or structures. It doesn't by itself allow you to apply any function to the `a` value inside the structure - it only flips the layers of structure around.
