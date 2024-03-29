# Observable Sharing Using Data.Reify

https://github.com/leonidas/codeblog/blob/master/2013/2013-01-14-observable-sharing.md

## Notion of object identity

When I started learning about functional programming after years of writing object oriented code, one difference that stood out for me is how, unlike many FPLs, 
>OOPLs have the builtin notion of object identity separate from object equality.

Consider these two examples, one in Python and the other in Haskell.

```py
# define a class
class Person:
  def __init__(self, name, children):
    self.name = name
    self.children = children

# instantiate the class
bob = Person("Bob", [])
```

```hs
-- define a record
data Person = Person
  { name     :: String
  , children :: [Person]
  }

-- instantiate the record
bob = Person "Bob" []
```

Now in Python, if we were to define another person that is also called "Bob", we could determine we are dealing with a different Bob using the `is` operator.

```py
bob1 = Person("Bob", [])  # Bob one
bob2 = Person("Bob", [])  # Bob two
assert bob1 is not bob2   # diff Bobs
```

In Haskell, though, if we define

```hs
bob1 = Person "Bob" []
bob2 = Person "Bob" []
```

we don't have any builtin way to determine whether two Bobs are the same person. In fact deriving `Eq` will make two Bobs equal.

```hs
_ = bob2 == bob2 -- True
```

Actually, since all data is immutable, some compiler might, in theory, use the *common subexpression elimination* to optimize the above to:

```hs
bob1 = Person "Bob" []
bob2 = bob1
```

Of course, when dealing with immutable data it shouldn't matter at all whether two variables actually point to the exact same data in memory or not, but there are some cases where we want to know *whether two data structures actually share data or if they merely have an equal value*. For example,

```hs
mary  = Person "Mary"  []
bob   = Person "Bob"   [mary]
alice = Person "Alice" [mary]
```

The way `Person` is defined, we have no way of knowing wether "Mary" is the daughter of "Bob" and "Alice", or whether "Bob" and "Alice" just happen to have a child with the same name, "Mery".

## Data.Unique

If we care about identity, we have to provide it manually.

```hs
import Data.Unique
import Data.Function (on)

data UniquePerson = UniquePerson
  { name     :: String
  , children :: [UniquePerson]
  , identity :: Unique
  }

-- comparing two UniquePersons for equality is done only on the `identity` fld
is :: UniquePerson -> UniquePerson -> Bool
is = (==) `on` identity
```

However, this comes with the limitation that we can only "instantiate" new persons in the IO monad, which makes sense as a **unique identity is actually an observable side-effect**.

```hs
import Control.Applicative ((<$>))

main = do
  mary  <- UniquePerson "Mary"  []     <$> newUnique
  bob   <- UniquePerson "Bob"   [mary] <$> newUnique
  alice <- UniquePerson "Alice" [mary] <$> newUnique
  print $ (children bob !! 0) `is` (children alice !! 0)
```

There's one problem with having to initialize a person in the IO monad:
>We are now dependent on the order of initialization.
~~why, isn't this solved using `mdo`, i.e. *the recursive do notation*?~~


This will cause us trouble if we change the data type from a parent-child hierarchy to something with *circular linking*.

```haskell
data UniquePerson = UniquePerson
  { name     :: String
  , friends  :: [UniquePerson]
  , identity :: Unique
  }

main = do
  bob  <- UniquePerson "Bob"  []    <$> newUnique
  fred <- UniquePerson "Fred" [bob] <$> newUnique
```

Now Fred can be a friend of Bob or Bob can be the friend of Fred, but there's no immediately obvious way to model the relationship both ways. We could change the data type to only refer to the friend using his ID, as in:


```haskell
data UniquePerson = UniquePerson
    { name     :: String
    , friends  :: [Unique]
    , identity :: Unique
    }

main = do
    bobId  <- newUnique
    fredId <- newUnique

    bob  = UniquePerson "Bob"  [fredId] bobId
    fred = UniquePerson "Fred" [bobId]  fredId
```

But then we'd have to keep a lookup map of ID->Person mappings and dereference the id's through that whenever we want to operate on the data and where's the fun in that?


It's worth noting, that for non-unique persons we don't have the same problem.

```haskell
data Person = Person
    { name    :: String
    , friends :: [Person]
    }

bob  = Person "Bob"  [fred]
fred = Person "Fred" [bob]
```

As pure expressions aren't necessary evaluated in order, the order of declarations doesn't matter and we can refer to a variable above its actual declaration.

Wouldn't it be nice if we could somehow use the pure data type to declare relationships between the persons, but still attach each person with a unique identity? This is where the [`data-reify`](http://hackage.haskell.org/package/data-reify) package comes in handy.


`data-reify` is based on the research paper [Type-Safe Observable Sharing in Haskell](http://www.cs.uu.nl/wiki/pub/Afp/CourseLiterature/Gill-09-TypeSafeReification.pdf) and it uses [GHC-specific tricks](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-Mem-StableName.html) to identify shared nodes in a data structure, essentially giving us the uniqueness property for free.

To use `data-reify` you need to have a recursive data type (such as our `Person` type) and a mirror type where the points of recursion are replaced by abstract references.


```haskell
data Person      = Person  String [Person]
data Person' ref = Person' String [ref]
```

Now we can derive a [`MuRef`](http://hackage.haskell.org/packages/archive/data-reify/latest/doc/html/Data-Reify.html#t:MuRef) instance that maps from `Person` to `Person'`.

```haskell
{-# LANGUAGE TypeFamilies #-}

import Data.Reify
import Data.Traversable (traverse)

instance MuRef Person where
    type DeRef Person = Person'

    mapDeRef f (Person name friends) =
        Person' name <$> traverse f friends
```

The type of [`mapDeRef`](http://hackage.haskell.org/packages/archive/data-reify/latest/doc/html/Data-Reify.html#v:mapDeRef) is a bit cryptic, but the gist of it is that it gets as a parameter a function `f` and a value of the original data type (`Person`) and the implementation has to return a value of the mirror type (`Person'`) where each recursive field is mapped through `f`.

Now that we have a `MuRef` instance, we can use the [`reifyGraph`](http://hackage.haskell.org/packages/archive/data-reify/latest/doc/html/Data-Reify.html#v:reifyGraph) function to build a list of `(ID, Person')` pairs and then initialize a `UniquePerson` based on that.

```haskell
makeUnique :: Person -> IO UniquePerson
makeUnique p = do
    Graph nodes rootId <- reifyGraph p

    let deref identity = UniquePerson name uniqueFriends identity where
            uniqueFriends = map deref friends
            Person' name friends = fromJust $ lookup identity nodes

    return $ deref rootId
```

Note that this uses the [`Unique`](http://hackage.haskell.org/packages/archive/data-reify/latest/doc/html/Data-Reify-Graph.html#t:Unique) type from [`Data.Reify.Graph`](http://hackage.haskell.org/packages/archive/data-reify/latest/doc/html/Data-Reify-Graph.html) which is different from [`Data.Unique`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Unique.html), but with small changes to `makeUnique` we can, again, use `Data.Unique.Unique` to ensure the uniqueness of the identities throughout our program.

```haskell
makeUnique :: Person -> IO UniquePerson
makeUnique p = do
    Graph nodes rootId <- reifyGraph p

    let makeIdentity (uid,_) = do
            uniq <- newUnique
            return (uid, uniq)

    identities <- mapM makeIdentity nodes

    let deref uid = UniquePerson name uniqueFriends identity where
            uniqueFriends = map deref friends
            identity      = fromJust $ lookup uid identities
            Person' name friends = fromJust $ lookup uid nodes

    return $ deref rootId
```

Now we can test that the identities are mapped correctly:

```haskell
main = do
    let bob   = Person "Bob"   [fred, alice]
        fred  = Person "Fred"  [bob]
        alice = Person "Alice" [fred]

    bob' <- makeUnique bob
    let [fred', alice'] = friends bob'

    -- Test that Bob and Alice know the exact same Fred
    print $ fred' `is` (friends alice' !! 0)
```

Finally, it's worth noting that `reifyGraph` doesn't in any way cache values from previous calls, so calling `makeUnique bob` and `makeUnique fred` will produce two separate `UniquePerson` graphs with no shared identities.


<hr/>
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>
