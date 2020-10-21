# Optics by Example
from the book: Optics by Example

## Optics

**Optics** are a family of inter-composable combinators for building bidirectional data transformations.

Optics, in its most general sense, is a field of study. In a slightly more concrete sense, optics are a family of tools which are interoperable with one another. Lenses, Folds, Traversals, Prisms and Isos are all types of optics.

Advatanges:
- Composition
- Separation of concerns
- Adaptability
- Conciseness
- Enforcing interface boundaries
- A principled and mature ecosystem

Disatvantages:
- cryptic type error messages
- complex Implementation
- vast collection of combinators

Optics help us perform actions over portions of data. Simple actions we can perform involve: viewing, modifying or traversing the selected data.

## Lenses

Optics allows us to separate concerns; i.e. split up the action
we perform on data from the selection of data we want to perform it on.


We'll refer to operations which can be performed on data as *actions*, whereas the *data selectors* are the actual optics. Each type of optic comes with a set of compatible actions. Each type of optic has a different *balance constraint vs flexibility*, moving to and fro on this spectrum results in several different but useful behaviours.

Lenses lean closer to the constrained side of things, which means you have a lot of guarantees about their behaviour, but also means that you need to prove those guarantees to make a lens, so there are fewer lenses in the world than there are of the more flexible optics.

Lenses have the following concrete guarantees:
- A Lens focuses (selects) a single piece of data within a larger structure
- A Lens must never fail to get or modify that focus.

We can use a lens to **view**, **set** and **modify** the focus within a structure.

```hs
   Action                 Structure
    ┌─┴─┐          ┌──────────┴──────────┐
>>> view (_2 . _1) (42, ("hello", False)))
         └───┬───┘       └──┬──┘
           Path           Focus
```

**Action** executes some operation over the focus of a path. E.g. `view` is an action which gets the focus of a path from a structure. Actions are often written as an infix operator; e.g. `%∼`, `^.` or even `<<%@=!`. Focused is passed to the action. Some actions accept parameters other than the focus.

**Path** indicates which data to focus and where to find it within the structure. A path can be a single optic, or several optics chained together through composition. If you consider dot-notation from most OO you will see similarities.

**Structure** is the hunk of data that we want to work with. The path selects data from within the structure, and that data will be passed to the action.

**Focus** is a smaller piece of a structure indicated by the path. Focus is passed to the action; e.g. we may `get`, `set` or `modify` the focus. Certain optics allow multiple focuses.

### Examples

```hs
x1 = view (_2 . _1)          (42, ("hello", False))    -- "hello"
x2 = view (_1 . _2)          ((1, 2), 3)               -- 2
x3 = set  (_2 . _Left) "new" (False, Left "old")       -- (False, Left "new")
x4 = set  (_2 . _Just) 42    (True, Just 13)           -- (True, Just 42)

x5 = over (taking 2 worded . traversed) toUpper "testing one two three"
-- "TESTING ONE two three"

x6 = foldOf (both . each) (["super", "cali"],["fragilistic", "expialidocious"])
-- "supercalifragilisticexpialidocious"
```


### Signatures

Signatures of functions we met so far:

```hs
_1, _2 :: (Field1 s t a b, Functor f) => (a -> f b) -> s -> f t
set    :: ASetter s t a b       -> b  -> s -> t
over   :: ASetter s t a b -> (a -> b) -> s -> t
view   :: MonadReader s m => Getting a s a -> m a
foldOf :: Getting a s a -> s -> a

taking    :: (Conjoined p, Applicative f) =>
             Int -> Traversing p f s t a a -> Over p f s t a a

worded    :: (Indexable Int p, Applicative f) =>
             p String (f String) -> String -> f String

traversed :: (Indexable Int p, Traversable f1, Applicative f2) =>
             p a (f2 b) -> f1 a -> f2 (f1 b)

_Just     :: (Choice p, Applicative f) =>
             p a (f b) -> p (Maybe a) (f (Maybe b))

_Left     :: (Choice p, Applicative f) =>
             p a (f b) -> p (Either a c) (f (Either b c))

both      :: (Data.Bitraversable.Bitraversable r, Applicative f) =>
             (a -> f b) -> r a a -> f (r b b)

each      :: (Each s t a b, Applicative f) =>
             (a -> f b) -> s -> f t
```



## View

The `_1` lens is the one provided by the lens library. It focuses on the first slot of a *tuple*, allowing us to get or set the value underneath.

```hs
-- usually, structure is s and focus is a

--    Lens' struct focus
_1, _2, _3 :: Lens' (a, b) a

-- Lens' struct focus -> struct -> focus
view :: Lens' s a -> s -> a
```





The `Lens'` type has two parameters:
- the first indicates the type of the structure
- the second indicates the type of the focus within it

Here, the structure is the 2-tuple and the focus is on its first slot. `_1` actually works on tuples of any size. The lenses for other slots of tuples are `_2`, `_3`, `_4`, etc.

The `view` action retrieves the path's focus from within the structure.
