# Existential types

Existential types are a way of "squashing" a group of types into a single type (hmm?).

Existentials are part of GHC extensions with `ExistentialQuantification`.

The `forall` keyword explicitly brings fresh type variables into scope.
It quantifies the types. In Haskell, type params are implicitly universally quantified:

```hs
id :: a -> a
-- is actually
id :: forall a . a -> a
```

What makes the `forall` keyword so useful is that you can apply additional constraints on the type variables it introduces. Such constraints, `P(x)`, serve to guarantee certain properties of the type variable, `x`, (as a kind of ad-hoc interface restriction) similarly to predicates `∃x.P(x)` or `∀x.P(x)`.

## Heterogeneous lists

The premise behind type classes is grouping of types that share a set of common properties. If we know that a type is a member of some particular class, we know certain things about it. For example, `Int` is a member of class `Ord`, so we know that we can compare integers by ordering (<, ≤, ≥, >); moreover, since `Eq` is a superclass of `Ord`, we also know we can compare integers for equality (`==`, `/=`). A type being a `Monad` implies it is an `Applicative`, which in turn implies it is also a `Functor`.

Suppose we have a bunch of values, and we don't know if they all have the same type, but we do know they are all members of some class, so they all have a certain property in common. It might be useful to put all these values into a list, which we cannot normally do because lists are homogeneous. However, existential types allow us to loosen this requirement by defining a *type hider* or *type box*, i.e. an existential type.

Basically, existential types allow us to pack a data type along with a set of operations over the type in a single "package". We cannot do much with such an *abstract type*, but we can use the provided operations on it.

To build a heterogeneous list, we can define an abstract type `ShowBox`, along with the `show` method by constraining the existential type param `s` with the `Show` class; this means that any type `s` can be a member of this list as long as it has the `show` method, i.e. if it can be showed. Now, we can make a heterogeneous list - a list of different types, but we can do fuck all with it except `show` its elements.

```hs
-- as GADT
data ShowBox where
  ShowBox :: Show s => s -> ShowBox

heteroList :: [ShowBox]
heteroList = [ShowBox (), ShowBox 5, ShowBox True, ShowBox (1, "abc")]
```

Since we are able to place these items (types) into a single list, they must somehow have the same type.

The use of the `forall` keyword gives this type to the `ShowBox` data ctor:

```hs
-- as ADT
data ShowBox = forall s. Show s => ShowBox s

ShowBox :: forall s. Show s => s -> ShowBox
```


We cannot write a function that takes this list as arg. We cannot apply, e.g. the function `not` to the elements of this list because their type might not be `Bool`. In fact, the only thing we can do with the elements is to show them.

```hs
data ShowBox = forall s. Show s => ShowBox s

heteroList :: [ShowBox]
heteroList = [ShowBox (), ShowBox 5, ShowBox True, ShowBox (1, "abc")]

instance Show ShowBox where
  show (ShowBox s) = show s -- (1)

foo :: [ShowBox] -> IO ()
foo xs = mapM_ print xs

main :: IO ()
main = foo heteroList
```

In the definition of `show` for `ShowBox` - the line marked with (1) - we don't know the type of `s`. But we do know that the type `s` is an instance of `Show` due to the constraint on the `ShowBox` data ctor, that is, on the `s` type param (`forall s. Show s =>`). Therefore, it is legal to use the function `show` on `s`.


## forall

One way to think about `forall` is to think about types as sets (of possible values), e.g. `Bool = {True, False, ⊥}` (`⊥` is a member of every type).

- `∀x.Px` ≈ `Px₀ ∧ Px₁ ∧ … ∧ Pxₙ` ≈ `⋀{x ∈ D}Px` ≈ `Π{x ∈ D}Px`
  - i.e. all `x`'s in the domain (of discourse) must satisfy the predicate `P`
  - ∀a.a ≈ ⋀a ≈ ⋂a
- `∃x.Px` ≈ `Px₀ ∨ Px₁ ∨ … ∨ Pxₙ` ≈ `⋁{x ∈ D}Px`≈ `Σ{x ∈ D}Px`
  - i.e. at least one item, `x`, in the DOD, `D`, must satisfy the predicate `P`
  - ∃a.a ≈ ⋁a ≈ ⋃a

The `forall` serves as a way to assert a **commonality** or **intersection of the specified types** (sets of values). So, for instance, `forall a. a` is the intersection of all types, `a`. This subset turns out to be the set whose sole element is bottom, `{⊥}`, since it is an implicit value in every type.

>Intersection of all types is the type whose only value is bottom, since bottom is the only value any pair of types has in common.

Since every Haskell type includes bottom this quantification in fact stipulates all Haskell types. However, the only permissible operations on such an intersection type are those operations available to a type whose only element is bottom (i.e. fuckall).


## Some examples

`forall a. a`   
In a sense, this is the most general type in Haskell. The type param `a` stands for any and all types. But since all types share nothing, this type is the same as bottom.

`forall a. Show a => a`   
The type param `a` stands for any and all types, but only if those types are members of the Show class. That is, the type param `a` stands for any and all showable types. But since the only common behavoir of showable types is that they can be shown, that is exactly what we can do with them.

`forall a. (Num, Show a) => a`   
The plot thikens. Now we can do arithmetic ops and show the results (and the types themselves).

`forall a. (Capability1, Capability2, …) => a`   
By accruing the capabilities a type must support, we can work with only those types that do support the specified capabilities.

`[forall a. a]`   
The type of list whose elements all have the type `forall a. a`, i.e. it is a list of bottoms.

`forall a. [a]`   
The type of list whose elements all have the same (any) type `a`. Since we cannot presume any particular type at all, this too is a list of bottoms.

`[forall a. Show a => a]`   
The type of list whose elements all have the type `forall a. Show a => a`. The `Show` constraint requires that the types be members of the `Show` class. However, ⊥ is still the only value common to all such types, so this too is a list of bottoms.

`[forall a. Num a => a]`   
The type of list whose elements are required to be members of `Num` class. Consequently, the possible values include numeric literals, which have the specific type `forall a. Num a => a` (as well as bottom).

## Intersection types

We see that most intersections over types just lead to bottoms because types generally don't have any values in common and so no presumptions cannot be made.


However, we have managed to define a heterogeneous list using a "type hider", which was just a type wrapper that made some guarantees (about the existence of certain facilities) by forcing a constraint (predicate) on the permissible types.

In general, the purpose of `forall` seems to be to impose type constraint on the permissible types within a type declaration, thereby guaranteeing that the certain facilities exist at member types.

```hs
-- declaring an existential type
data T = forall a. MkT a

-- existential defines a polymorphic data ctor (or, a family of ctors for T)
>>> :t MkT :: forall a. (a -> T)

-- We can pass any type `a` to MkT and it will create a `T`
>>> :t MkT 4 :: T
```

>What happens when we deconstruct a `T` value with pattern matching?

Pattern matching on the existential constructor

```hs
foo (MkT x) = ... -- what is the type of x?
```

As we stated, `x` could be "any type". That means it is a member of some arbitrary type, so `x` has the type `forall a. a`. In other words, the set whose only available value is bottom.

However, we can make a heterogeneous list:

```hs
hList :: [T]
hList = [MkT 5, MkT (), MkT True, MkT map]
```

Of course, when we pattern match on `hList` we cannot presume any features about its elements.

(However, we can apply them to functions whose type is `forall a. a -> r`, for some arbitrary `r`, as these accept values of any type as params. Examples of such functions are `id`, `seq`, `const k`, for some/any `k`).

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
-- existential data type with a class constraint
data T' = forall a. Show a => MkT' a
```

The class constraint serves to limit the types we are intersecting over, such that we now have values inside a `T'` which are elements of some arbitrary type that are members of `Show`. The implication of this is that we can apply `show` to a value of type `a` upon deconstruction. It doesn't matter exactly which type it turns out to be - they all support `show`.

```hs
hList2 = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"]
x1 = mapM_ (\(MkT' x) -> print x) hList2
-- 5  ()   True  "Sartre"
```

>To summarize, the interaction of the universal quantifier with data types produces a qualified subset of types guaranteeing certain facilities as described by one or more class constraints.
