# Existential types

Existential types are a way of "squashing" a group of types into a single type. Existentials are part of GHC extensions with ExistentialQuantification.
The `forall` keyword explicitly brings fresh type variables into scope.
It quantifies the types.

For example, explicitly quantifying the type params:

```hs
map :: forall a b. (a -> b) -> [a] -> [b]
```

> Any introduction of a type parameter implicitly begins with a forall keyword.

```hs
id :: a -> a
id :: forall a . a -> a
```

What makes life really interesting and the forall so useful is that you can apply additional constraints on the type variables ∀ introduces.

Such constraints, P(x), serve to guarantee certain properties of the type variable, x, as a kind of ad-hoc interface restriction, similar to ∃x.P(x) or
∀x.P(x).


## Heterogeneous lists

The premise behind Haskell's type class system is grouping types that all share a common property. If you know that a type is a member of some particular class, you know certain things about that type.

Suppose we have a group of values and we don't know if they are all the same type, but we do know they are all members of some class, so they all have a certain property in common. It might be useful to throw all these values into a list, which we can't normally do for lists are homogeneous. However, existential types allow us to loosen this requirement by defining a **type hider** or **type box**.

```hs
data ShowBox = forall s. Show s => SB s
-- SB :: forall s. Show s => s -> ShowBox

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]
```

We are calling the ctor on 3 values of different types: `SB ()`, `SB 5`, `SB True`, yet we are able to place them all into a single list, so we must somehow have the same type for each one.

This is because our use of the forall keyword gives our constructor the type

```hs
SB :: forall s. Show s => s -> ShowBox
```

If we were to write a function that takes a heteroList, we couldn't do it, we can't apply a function such as 'not' to the values inside the SB because their type might not be Bool. The only thing we know about the elements is they they can be converted to a string via 'show'.

```hs
-- deriving instance Show ShowBox

instance Show ShowBox where
    show (SB s) = show s        -- (1)

f :: [ShowBox] -> IO ()
f xs = mapM_ print xs

main = f heteroList
```

We know that the type is an instance of Show due to the constraint on the SB constructor, so it's legal to use show on `s`.

```hs
print :: Show s => s -> IO ()
-- print x = putStrLn (show x)

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ :: (a -> m b) -> [a] -> m ()

mapM_ print :: (Foldable t, Show a) => t a -> IO ()
mapM_ print :: Show s => [s] -> IO ()
```

As we declared ShowBox an instance of Show, we can print the values in the list


## forall

One way to think about forall is to think about types as a set of possible values, e.g. Bool = {True, False, ⊥} and bottom, ⊥, is a member of every type.

forall serves as a way to assert a commonality or *intersection of the specified types* (i.e. sets of values).

`forall a. a` is the intersection of all types.

This subset turns out to be the set whose sole element is bottom, {⊥}, since it is an implicit value in every type. It is the type whose only available value is bottom.

Since every Haskell type includes bottom this quantification in fact stipulates all Haskell types. However, the only permissible operations on it are those operations available to a type whose only element is bottom.

Examples:

`[forall a. a]`
    type of list whose elements all have the type 'forall a. a'
    i.e. a list of bottoms.

`forall a. [a]`
    type of list whose elements all have the same type `a`. Since we cannot presume any particular type at all, this too is a list of bottoms.

`[forall a. Show a => a]`
    type of list whose elements all have the type 'forall a. Show a => a'
    The Show class constraint requires the possible types to also be a member of the class, Show. However, ⊥ is still the only value common to all these types, so this too is a list of bottoms.

`[forall a. Num a => a]`
    requires each element to be a member of the class, Num. Consequently, the possible values include numeric literals, which have the specific type
    'forall a. Num a => a', as well as bottom.


We see that most intersections over types just lead to bottoms because types generally don't have any values in common and so presumptions cannot be made about a union of their values.

However, we have managed to define a heterogeneous list using a "type hider", which was just a type wrapper that made some guarantees (about the existence of certain facilities) by implying a predicate or constraint on the permissible types.

In general, the purpose of 'forall' seems to be to impose type constraint on the permissible types within a type declaration thereby guaranteeing the existence of certain facilities with such types.

```hs
-- declaring an existential type
data T = forall a. MkT a

-- It defines a polymorphic ctor, or a family of ctors for T
:t MkT :: forall a. (a -> T)

-- We can pass any type `a` to MkT and it will create a `T`
:t MkT 4 :: T
```

But what happens when we deconstruct a `T` value with pattern matching?

Pattern matching on our existential constructor

```hs
foo (MkT x) = ... -- what is the type of x?
```

As we've just stated, x could be of any type. That means it's a member of some arbitrary type, so has the type 'forall a. a'. In other words the set whose only available value is bottom.

However, we can make a heterogeneous list:

```hs
hList = [MkT 5, MkT (), MkT True, MkT map] :: [T]
```


Of course, when we pattern match on hList we cannot presume any features about its elements. However, we can apply them to functions whose type is:

forall a. a -> R

for some arbitrary R, as these accept values of any type as params.

Examples of such functions: 'id', 'seq', 'const k' for any k.

```hs
map id hList         :: [T]
map (const 34) hList :: forall {b}. Num b => [b]
map seq hList        :: forall {b}. [b -> b]

id         :: forall {a}. a -> a
seq        :: forall {a} {b}. a -> b -> b
const      :: forall {a} {b}. a -> b -> a
(const 42) :: forall {a} {b}. Num a => b -> a
```


So technically, we can't do anything useful with its elements, except reduce them to WHNF (because all we know is that they have some arbitrary type).
However, if we introduce class constraints:

```hs
-- a new existential data type, with a class constraint
data T' = forall a. Show a => MkT' a
```

The class constraint serves to limit the types we are intersecting over, such that we now have values inside a T' which are elements of some arbitrary type that are members of Show. The implication of this is that we can apply show to a value of type `a` upon deconstruction. It doesn't matter exactly which type it turns out to be.

```hs
hList2 = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"]

main2 = mapM_ (\(MkT' x) -> print x) hList2
-- 5  ()   True  "Sartre"
```

To summarize, the interaction of the universal quantifier with data types produces a qualified subset of types guaranteeing certain facilities as described by one or more class constraints.
