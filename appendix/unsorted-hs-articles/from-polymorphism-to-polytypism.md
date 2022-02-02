# From Polymorphism To Polytypism

> by Nicolas Wu

by Nicolas Wu

* * *

Posted on 10 December 2010

Tags: [Haskell](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/tags/Haskell.html)

* * *

This post serves as an introduction to polytypic programming, and draws heavily from [Generics for the Masses](http://www.comlab.ox.ac.uk/ralf.hinze/publications/index.html) by Ralf Hinze, which I can heartily recommend if you enjoy this post. While most functional programmers will have heard of polymorphism, polytypic programming is less well known. In polytypic programming the focus is on structural induction to define a collection of types and functions. Ultimately, it means that functions which are similar in structure need only be defined in their base cases and construction, rather than in their specific instances.

Polymorphism
------------

Before we look at polytypic programming, let’s look at polymorphic programming. Polymorphic programming allows us to be more expressive with our type system by writing functions which generalise over different datatypes. For instance, the `foldr` function is polymorphic over the type of list in question:

    foldr :: (a -> b -> b) -> b -> [a] -> b

Here we have an implicit universal quantification which indicates that the function works for all values of type `a` and `b`, thus making this function polymorphic.

We can use `foldr` to obtain another polymorphic function like the `length` function, which works out the length of a list:

    > length :: [a] -> Int
    > length = foldr (const succ) 0

While this function is generic in the sense that its definition is solely in terms of the structure of lists, it lacks genericity in the sense that it applies only to lists.

### Foldability

The example of `length` can be generalised by writing a specific version that can be used for all the different container datastructures we are interested in. This generalised function, which we’ll call `size`, can be defined explicitly for different structures:

    > size_Maybe :: Maybe a -> Int
    > size_Maybe Nothing  = 0
    > size_Maybe (Just _) = 1

Another version of `size` can be written for lists:

    > size_List :: [a] -> Int
    > size_List []     = 0
    > size_List (_:xs) = 1 + size_List xs

These definitions could be brought together into a single `size` function by making a new class definition, `Sizeable`, which provides an interface for a `size` function that is instantiated according to the datastructure in question.

But something has been lost along the way: neither of these definitions make use of the foldable properties of their respective container types. To fix this, we can make use of the `Foldable` class, defined in `Data.Foldable`, which has the following minimal interface:

    > class Foldable t where
    >   foldMap :: Monoid m => (a -> m) -> t a -> m

The key point is that a type is considered to be foldable so long as there is a sensible conversion from that class to a monoidal datatype. A monoidal datatype instantiates the following class:

    > class Monoid m where
    >   mempty  :: m
    >   mappend :: m -> m -> m

This allows us to rewrite and unify our `size` functions by making the most of the underlying monoidal property of size:

    > data Measure = Measure { measure :: Int }
    > instance Monoid Measure where
    >   mempty                            = Measure 0
    >   (Measure x) `mappend` (Measure y) = Measure (x + y)

Then we can define size in a different way:

    > size_Foldable :: Foldable t => t a -> Int
    > size_Foldable = measure . foldMap (const (Measure 1))

While this example has shown us how `size` can be thought of as a specific instance of a monoidal function applied to a foldable datatype, the solution has merely moved the problem of genericity away from the `Sizeable` class, and into the `Foldable` class, where an instance must still be written for each datastructure of interest: the value of `t` cannot be universally quantified, and the result must be monoidal. The problem here is that the implicit structure of the data is not being exploited to its full effect.

Polytypism
----------

With polytypic programming, the aim is to provide a generic definition of a function by using induction on the structure of types. In order to achieve this, we decompose our data constructions into their fundamental building blocks: every new datatype is composed of _primitive_ types (like `Int`, `Char`, or `Float`) which are composed together using _elementary_ types: the unit type, sums, and products.

### Representation

In this post, the elementary types are represented using the types `()`, `Either a b` and `(a, b)`. Together, these types can be used to represent other algebraic types. For example, the usual definition for a `List` is as follows:

    data List a = Nil | Cons a (List a)

Roughly speaking, the following equivalence holds:

    Nil | Cons a (List a) == Either () (a, List a)

The isomorphism between these two representations of lists can be made explicit by providing the following two methods which witness that isomorphism:

    > fromList :: [a] -> Either () (a, [a])
    > fromList []     = Left ()
    > fromList (x:xs) = Right (x, xs)

    > toList :: Either () (a, [a]) -> [a]
    > toList (Left ())       = []
    > toList (Right (x, xs)) = (x:xs)

This notion of isomorphism can be captured by using the following structure:

    > data Iso a b = Iso { fromData :: b -> a, toData :: a -> b }

The representation in terms of elementary types provides us with a means of decomposing the structure of lists, and this allows us to define a polytypic version of `size` which follows the structure of its argument. We do this by recursively constructing a function that can operate on any representable datatype. These construction functions are bundled together as instances of the class `Rep`:

    > class Rep a where
    >   rep :: (Generic g) => g a

The idea here is to construct a generic function `g` for each representable type `a`. The class `Generic` is instantiated by a datatype that corresponds to each generic definition we require, and provides a family of functions, one for each primitive and elementary type, that constructs the generic function type.

All the datatypes which can be represented are members of the `Rep` class, so we provide instances for each of the elementary and primitive types that could be encountered, where the responsibility of evaluating each type is delegated to a function given by the `Generic` class that builds the generic function.

    > instance Rep () where
    >   rep = unit
    > instance (Rep a, Rep b) => Rep (Either a b) where
    >   rep = plus
    > instance (Rep a, Rep b) => Rep (a, b) where
    >   rep = pair
    > instance Rep Char where
    >   rep = char
    > instance Rep Int where
    >   rep = int

The tricky part is to provide an instance for each algebraic datatype of interest. To do so, we delegate to a function `datatype` which makes use of the isomorphism between the datatype in question and the elementary representation:

    > instance (Rep a) => Rep [a] where
    >   rep = datatype (Iso fromList toList)

This single instance declaration is enough to provide an interface for a multitude of polytypic function definitions. While this is similar to the `Foldable` class we saw earlier on, in that an instance must be declared for each type we wish to evaluate, the difference is that `Rep` can be used to recursively pick apart a datatype in terms of its isomorphism with its elementary type, and we are not restricted to returning monoidal types.

### Generics

As mentioned above, the `Generic` class is used to provide a collection of functions which construct generic types that can consume each of the primitive and elementary types.

    > class Generic g where
    >   unit     :: g ()
    >   plus     :: (Rep a, Rep b) => g (Either a b)
    >   pair     :: (Rep a, Rep b) => g (a, b)
    >   char     :: g Char
    >   int      :: g Int
    >   datatype :: (Rep a) => Iso a b -> g b

For example, the type `Size a` is used to turn an arbitrary type `a` into an `Int` that represents the size of `a`:

    > newtype Size a = Size { appSize :: a -> Int }

The generic instance of `Size` demonstrates how the size of each primitive and elementary type can be calculated:

    > instance Generic Size where
    >   unit         = Size (const 0)
    >   plus         = Size (either size size)
    >   pair         = Size (uncurry (+) . (size × size))
    >   char         = Size (const 1)
    >   int          = Size (const 1)
    >   datatype iso = Size (size . fromData iso)

Here we make use of the `(×)` operator:

    > (×) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
    > (f × g) (a, b) = (f a, g b)

The interesting case is the `datatype` function which uses the `fromData` part of the datatype isomorphism.

Finally, the generic version of `size` is defined as follows:

    > size :: (Rep a) => a -> Int
    > size = appSize rep

This curious definition brings together the represented datatype, through the explicit type class constraint, `Rep a`, as well as the generic function definition, found implicitly in the datatype which `appSize` operates on.

Since this post is actually a literate Haskell file we can use it to evaluate, say, the size of a list of characters in ghci:

    *Main> size "Hello World!"
    12

Conclusion
----------

This article has discussed the differences between polymorphism and polytypism. In the literature polytypic programming has been given several different names:

*   Data-generic programming
*   Structural polymorphism
*   Type parametric programming
*   Shape polymorphism
*   Intensional polymorphism

Examples of polytypicity in practice can be found in the derivable operators in Haskell. For example, a function such as `(==)` is polytypic:

    class Eq a where
      (==) :: a -> a -> Bool

This can be considered polytypic since we can ask the compiler to derive an instance of `Eq` for an arbitrary datatype, and this is done based on the structure of the type. While such polytypic functionality exists in Haskell, it can only be accessed by using the `deriving` keyword, and there is no way to contribute to the family of classes which can be derived.

Our discussion has detailed one of the many frameworks that can be used to achieve polytypicity. I particularly like this incarnation because it is possible in ordinary Haskell without making use of any extensions. The downside is that the isomorphism between datatypes and their elementary representation must be given explicitly.


[Source](https://zenzike.com/posts/2010-12-10-from-polymorphic-to-polytypic.html)