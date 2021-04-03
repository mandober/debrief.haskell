# Existential types

https://wiki.haskell.org/Existential_type
https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
https://markkarpov.com/post/existential-quantification.html


We define a new custom type using a TND (type/newtype/data) construct. The definition may contain TPs (type params) but each TP must be first declared (on the LHS) before it can be mentioned on the RHS of the definition. Failing to declare it before the use is an error.

Haskell universally quantifies all TPs; that is, the programmer doesn't have to write the `forall` quantifier since it gets added by the compiler implicitly. Any declaration involving TPs is as if TPs were introduced with a `forall` quantifier.

Pretty much all the places where TPs are introduced (instance declaration, type definition, function signature) are game for the universal quantification of TPs.

Random sample of the places where the `forall` appears:

```hs
maybe :: forall b a. b -> (a -> b) -> Maybe a -> b
data W a b = forall e. Buffer e => W e a b
type instance forall a. GHC.Generics.Rep [a]
instance forall a. Eq a => Eq [a]
```

∃T (existential types) are a way to lift this restriction. When the `ExistentialQuantification` pragma is enabled, we are allowed to use a TP without declaring it. In a way, this allows us to "hide" a TP.






```hs
-- ERROR: using a TP on the RHS without declaring it:
data Worker x y = Worker { buffer :: b, input :: x, output :: y }


-- the above is an error, you would have to write:
data Worker b x y = Worker { buffer :: b, input :: x, output :: y }

-- now suppose that a Worker can use any type `b` so long as it belongs to
-- a particular class; then every fn using it will have a class constraint:
buf :: (Buffer b) => Worker b Int Int


-- In particular, failing to write an explicit type signature will invoke the the monomorphism restriction). Using existential types, we can avoid it:
{-# LANGUAGE ExistentialQuantification #-}

data Worker x y = forall b. Buffer b =>
     Worker { buffer :: b, input :: x, output :: y }

-- and write related fns without constraints:
buf :: Worker Int Int
```

The type of the `buffer` now doesn't appear in the `Worker` type at all.

This has a number of consequences:
1. It is now impossible for a function to demand a `Worker` having a specific type of buffer.
2. The type of `buf` can now be derived automatically without needing an explicit type signature (no monomorphism restriction).
3. Finally, since the code now has no idea what type the `buffer` accessor function returns, you are more limited in what you can do to it.


In general, when you use a *hidden type* in this way, you will usually want that type belonging to some specific class, or you'll want to pass some functions along that can work on that type. Otherwise you'll have some value belonging to a random unknown type, and you won't be able to do anything to it!

Note: You can also use existential types to convert a more specific type into a less specific one (there is no way to perform the reverse conversion).


## Example 1: heterogeneous list

This example illustrates creating a *heterogeneous list*, all of whose members implement `Show`. We'll traverse through that list to show the items.

```hs
data Obj = forall a. (Show a) => Obj a

xs :: [Obj]
xs = [Obj 1, Obj "foo", Obj 'c']

doShow :: [Obj] -> String
doShow [] = ""
doShow ((Obj x):xs) = show x ++ doShow xs


doShow xs -- "1\"foo\"'c'"
```

## Example 2: heterogeneous list

Problem statement: in a raytracer, a requirement is to be able to render several different objects (like a ball, mesh or whatever). The first step is a type class for Renderable like so:

```hs
class Renderable a where
    boundingSphere :: a -> Sphere
    hit :: a -> [Fragment] -- returns the "fragments" of all hits with ray
    -- etc.


hits :: Renderable a => [a] -> [Fragment]
hits xs = sortByDistance $ concatMap hit xs
```

To solve the problem, we must be able to apply the `hits` function to different objects, e.g. a sphere and polygon. However, as written, it would't work since the elements of a list need to to homogenuous. Existential types to the rescue:

```hs
{-# LANGUAGE ExistentialQuantification #-}
-- ... snipp ...
data AnyRenderable = forall a. Renderable a => AnyRenderable a

instance Renderable AnyRenderable where
    boundingSphere (AnyRenderable a) = boundingSphere a
    hit (AnyRenderable a) = hit a
    -- etc. snip ...

-- now, create lists with type [AnyRenderable], for example,
-- where x, y, z can be from different instances of Renderable
    [ AnyRenderable x
    , AnyRenderable y
    , AnyRenderable z ]
```


## Example 3: Emulating OOP dynamic dispatch

Existential types in conjunction with type classes can be used to emulate the dynamic dispatch mechanism of OOP. We illustrate this concept with a classic example from OOP encoded in Haskell.

```hs
{-# LANGUAGE ExistentialQuantification #-}

class Shape_ a where
    perimeter :: a -> Double
    area      :: a -> Double

data Shape = forall a. Shape_ a => Shape a

type Radius = Double
type Side   = Double

data Circle    = Circle    Radius
data Rectangle = Rectangle Side Side
data Square    = Square    Side


instance Shape_ Circle where
    perimeter (Circle r) = 2 * pi * r
    area      (Circle r) = pi * r * r

instance Shape_ Rectangle where
    perimeter (Rectangle x y) = 2*(x + y)
    area      (Rectangle x y) = x * y

instance Shape_ Square where
    perimeter (Square s) = 4*s
    area      (Square s) = s*s

instance Shape_ Shape where
    perimeter (Shape shape) = perimeter shape
    area      (Shape shape) = area      shape

-- Smart ctors

circle :: Radius -> Shape
circle r = Shape (Circle r)

rectangle :: Side -> Side -> Shape
rectangle x y = Shape (Rectangle x y)

square :: Side -> Shape
square s = Shape (Square s)

shapes :: [Shape]
shapes = [circle 2.4, rectangle 3.1 4.4, square 2.1]
```
