# Functor

- Functor typeclass is a generalization of a mappable container type.
- container types that can be mapped: [a], (a), Maybe a, etc.

>:t Functor
```hs
class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
instance Functor []
instance Functor ((,) a)
instance Functor Maybe
instance Functor (Either a)
instance Functor ((->) r)
instance Functor IO
```

- Functor class prototypes 2 funcs: `fmap` and `<$`
- The sig of fmap is: `fmap :: (a -> b) -> f a -> f b`
- `f` is a type constructor that takes one type parameter.


## Functor

http://learnyouahaskell.com/making-our-own-types-and-typeclasses

Functor typeclass is basically for things that can be mapped over. The list type is part of the Functor typeclass. Let's see how it's implemented:

```hs
class Functor f where  
    fmap :: (a -> b) -> f a -> f b
```

In the definitions of typeclasses seen so far, the type variable that played
the role of the type in the typeclass was a *concrete type*, like `a` in:
    `(==) :: (Eq a) => a -> a -> Bool`
But now, the `f` is not a concrete type (a type that a value can hold: Int, Bool, Maybe String), but a **type constructor** that takes one type parameter.

A quick refresher example: `Maybe Int` is a concrete type, but `Maybe` is a type constructor that takes one type as the parameter.

We see that `fmap` takes a function from one type to another and a functor applied with one type and returns a functor applied with another type.

The type declaration for `fmap` should remind you of something, the type signature of map:

```hs
fmap :: (a -> b) -> f a -> f b
map  :: (a -> b) -> [a] -> [b]
-- desugared:
map  :: (a -> b) -> a : [] -> b : []
```

It takes a function from one type to another and a list of one type and returns a list of another type. I think we have ourselves a functor. 
In fact, `map` is just a `fmap` that works only on lists. 

Here's how the list is an instance of the `Functor` typeclass.

```hs
instance Functor [] where
    fmap = map
```

Notice how we didn't write instance 
`Functor [a] where` 
because from 
`fmap :: (a -> b) -> f a -> f b` 
we see that `f` has to be a type constructor that takes one type.

[a] is already a concrete type (of a list with any type inside it), 
while `[]` is a type constructor that takes one type and can produce types such as [Int], [String], [[String]]


Since for lists, fmap = map, we get the same results:

```hs
map :: (a -> b) -> [a] -> [b]

ghci>  map (*2) [1..3] -- [2,4,6]
ghci> fmap (*2) [1..3] -- [2,4,6]

ghci>  map (*2) []     -- []
ghci> fmap (*2) []     -- []
```

when we `map` or `fmap` over an empty list we get an empty list. It just converts an empty list of type [a] into an empty list of type [b].

Types that can act like a box can be functors. You can think of a list as a box that has an infinite amount of little compartments and they can all be empty, one can be full and the others empty or a number of them can be full. So, what else has the properties of being like a box? For one, the Maybe a type. In a way, it's like a box that can either hold nothing, in which case it has the value of Nothing, or it can hold one item, like "HAHA", in which case it has a value of Just "HAHA". Here's how Maybe is a functor.

```hs
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

Note how we wrote `instance Functor Maybe where` instead of 
`instance Functor (Maybe m) where`.

Functor wants a *type constructor that takes one type* and not a concrete type. 

If you mentally replace the `f`s with `Maybe`s, 
`fmap` acts like a 
`(a -> b) -> Maybe a -> Maybe b` 
for this particular type, which looks OK.

But if you replace `f`s with `(Maybe m)` 
then it would seem to act like a 
`(a -> b) -> Maybe m a -> Maybe m b`, 
which doesn't make any sense because 
`Maybe` takes just one type parameter.


The `fmap` implementation is simple: 
if it's an empty value of `Nothing`, 
  then return `Nothing`. 
If it's not an empty, but a single value wrapped in a `Just`, 
  then we apply the fn on the unwrapped `Just`.

```hs
ghci> fmap (++ "me") (Just "Some ")  -- Just "Some me"
ghci> fmap (++ "me") Nothing         -- Nothing
ghci> fmap (*2) (Just 200)           -- Just 400
ghci> fmap (*2) Nothing              -- Nothing
```
