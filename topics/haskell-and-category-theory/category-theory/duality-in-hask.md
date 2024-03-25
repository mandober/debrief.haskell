# Duality in Hask

http://blog.ezyang.com/2012/10/duality-for-haskellers/
http://blog.ezyang.com/2010/07/flipping-arrows-in-coburger-king/

In category theory, an informal definition of duality would be saying that "it is the same thing, but with all the arrows reversed". But knowing what arrows to reverse is an important aspect. Here, we take a few familiar structures and describe them in term of arrows, and then show the dual concept we get when the arrows are reversed. The dual structures (or classes, or, more generally, concepts) may be presented as pairs (that may have the same name apart from one of them bearing the "co-" prefix - derived from Latin and meaning "with"; it appears in several similar forms, as "co", "con", "com").

Duals
- product/coproduct
- data/codata
- monad/comonad
- (Functors do not have duals per se, they are their own duals)
- bottom/top, i.e. terminal/initial object

## Constructors and deconstructors

All data types have two fundamental groups of operations, constructors and deconstructors. *Constructors* (ctors) are functions used to construct values of some datatype by supplying them with the individual datatype's components. Dually, *deconstructors* (dtors) are a group of functions used to decompose a datatype into its constituent components.

The data types manipulated by ctor and dtor functions are standard algebraic data types, of which the two primary groups are product and sum types.

To build a value of a product datatype, we need to supply all the constituent components (individual values) to the ctor function. Thus, the product types usually have a single ctor function. The canonical product data type is a pair, `(a, b)` and the other product data types are isomorphic to it.
- `Pair a b` ≅ `(a, b)`
- List a = … | `Cons a (List a)` ≅ `(Cons a, List a)`

To build a value of a sum data type, we need to supply only one component value, out of the set of all constituent components, to the ctor function. Thus, the sum types usually have as many ctor functions as there are components, i.e. variants, in the type. The canonical sum (coproduct) data type is `Either a b` and the other sum data types are isomorphic to it.
- `Maybe a` ≅ `Either () a`

## Product and coproduct

Consider the type `Either a b` and its associated ctor and dtor functions.

```hs
data Either a b = Left a | Right b

Left  :: a -> Either a b
Right :: b -> Either a b
```

The `Either` is a sum data type with 2 variants, so it has 2 ctor functions, i.e. two data constructors, which are the functions `Left` and `Right`:
- `Left :: a -> Either a b`
- `Right :: b -> Either a b`

Each data ctor takes a value of the appropriate type, producing a value of the `Either` type. However, sum types are actually disjoint unions, which means they are indexed types.

Considering types as sets, union of two sets `A` and `B` is a set containing all the elements (values) of both sets, `A ⋃ B = { x | x ∈ A ⋁ x ∈ B }`. However, a disjoint union, `⨄`, involves, apart from the two initial sets `A` and `B`, an indexing set as well. An indexing set may be an arbitrary set provided it has the sufficient cardinality.

For example,

```js
Both sets are the same, A = B = Bool

1) Union: Bool ⋃ Bool
  = { True, False } ⋃ { True, False }
  = { True, False, True, False }
  = { True, False }
  // and the type of the resulting set is still Bool:
  `Bool ⋃ Bool :: Bool`

2) Disjoint_union: Bool ⨄ Bool
  // The indexing set is `{Left, Right}`
  Bool ⨄ Bool
  = { Left True, Left False } ⨄ { Right True, Right False }
  = { Left True, Left False, Right True, Right False }
  // these ↑ are all the possible elements
  // the type of the resulting set is
  `Bool ⨄ Bool :: Either Bool Bool`
  // The two variants are: `Left { True, False }`, `Right { True, False }`
```

## Product

```hs
Product:

         c
       ↗ ↑ ↖
      /  |  \
     /   |   \
    /    |    \
 f /     |     \ g
  /      |<f,g> \
 /       |       \
/  Left  |  Right \
a ------>.<------- b
        a+b
    Either a b

-- 2 data ctors
Left  :: a -> Either a b
Right :: b -> Either a b

-- 1 dtor
either :: (a -> r) -> (b -> r) -> Either a b -> r

h = <f,g> = either f g
f = either f g . Left
g = either f g . Right
```

All-in-one deconstructor, `either :: (a -> r) -> (b -> r) -> Either a b -> r`, is a hof that accepts 3 args, the first 2 being functions that handle their respective cases. The third arg is the actual `Either a b` value that undergoes case analysis.

The first arg function, `a -> r`, handles the `Left` case, and the second arg function, `b -> r`, handles the `Right` case when the case analysis of a value of the `Either a b` type happens. The case analysis itself is usually performed by pattern matching, where an unknown value is scrutinezed just enough to reveal the data ctor underneath.

## Coproduct

```hs
Coproduct: dual of product (all arrows reversed)

         c
       / | \
      /  |  \
     /   |   \
    /    |    \
 f /     |     \ g
  /      |<f,g> \
 /       |       \
↙   fst  ↓  snd   ↘
a <------.-------> b
       (a,b)

-- 2 dtors
fst :: (a,b) = a
snd :: (a,b) = b

-- 1 ctor
pair :: forall a b x. (x -> a) -> (x -> b) -> x -> (a, b)
pair f g = \x -> (f x, g x)

f = const a
g = const b
<f,g> = (const a, const b) = const (a,b)
-- actually,
<f,g> = (f x, g x) = (const a x, const b x) == const (a,b) x
```

The dual of product is coproduct and its diagram is the same only all arrows have been reversed. The 2 constructor functions `Left` and `Right` have become the 2 deconstructor functions `fst` and `snd`.

But what about the `f` and `g` functions and the new, perhaps unexpected constructor function `pair f g = \x -> (f x, g x)`? This ctor is a generalized version of the standard pair constructor. We can retrieve the traditional pair data ctor by setting `f = const a` and `g = const b`. The specification of a pair as an arrow may look weird: `<f,g> = <const a, const b> = const (a, b)`.

Note: this presentation is backwards, which was done to avoid dropping the function `\x -> (f x, g x)` seemingly out of nowhere.

## Top and bottom

The unit type, `()`, also referred to as top type, and the uninhabited type `Void` (also known as the bottom type, although in Haskell, bottom is usually denoted by `⟘`) exhibit a duality between each other.

This presents itself as follows: for any Haskell type, we can trivially construct a function which takes a value of that type and produces the unit type/value, i.e. the function `unit :: a -> ()`, or more isomorphicly unique (with isomorphic pretensions), `const ()`.

- `Void` is uninhabited type, *type 0*, also a terminal object
- unit, `()` is a singleton type, *type 1*, an initial object

```hs
anyToVoid1 :: forall a. a -> ()
anyToVoid1 a = ()

anyToVoid2 :: forall a. a -> ()
anyToVoid2 a = const () a = ()
```

Furthermore, ignoring laziness, `const ()` is the only function which does this, i.e. it is unique.

If we flip the arrows: is there a type `A` for which, for any type `B`, there is a function `A -> B`? At first glance, this would seem impossible. `B` could be anything, including an uninhabited type, in which case we'd be hard pressed to produce an appropriate value. However, since `A` is uninhabited, we need not be concerned with the argument to this function - since its arg has the type that is uninhabited, the function can never be called (it's just for shit n' giggles). Top and bottom are dual to each another.

In fact, top (unit) is the *terminal object*, and bottom (`Void` as uninhabited data type, or `undefined` as value) is the *initial object* in the 𝓗𝓪𝓼𝓴 category.

A note about terminal objects: is `Int` a terminal object? Certainly, there are functions of the type `forall a. a -> Int`, like `const 2`. However, this function is not unique: there is also `const 0`, `const 1`, etc. So `Int` is not terminal. There is a theorem that states that all terminal objects are isomorphic to one another; or dualized: all initial objects are isomorphic to one another - but `Int` and `()` are obviously not isomorphic.

There is an alternative presentation of top as `∃x.x` and bottom `∀x. x`.

```hs
-- initial object
bottom :: forall a. a
bottom = undefined

-- terminal object
top :: (forall a. a) -> ()
top _ = ()
```

which doesn't require `const`, which, ignoring some other requirements for top, would work for many other types as long as they are inhabited.


## Folds and unfolds

The diagram for a fold is a bit involved, so we'll derive it from scratch by considering the most common fold - the fold on lists.

```hs
data List a = Cons a (List a) | Nil

foldr :: (a -> r -> r) -> r -> List a -> r
```

The first two arguments "define" the fold (handler args) while the third argument provides the actual list to fold. If we draw the diagram immediately

```
List a ----------> R
        fold f z
```

we get a somewhat boring one because the pair `(a -> r -> r, r)` doesn't really have any good interpretation as an arrow. What we'd really want is a single function that encodes all of the information that the pair encodes. That is, a single hof that accepts two args, each of which is a function that handles a specific case of list, i.e. `Cons` and `Nil` handler functions.

```hs
hCons :: (a -> r -> r) -> r
hNil  :: r             -> r
```

The `g` function could be `Maybe (a, r) -> r`. Supposing we originally had the pair `(f, z)`, then we can define `g` in terms of `f`, `z` and the list `xs`.

```hs
g :: Maybe (a, r) -> r
g Nothing        == z
g (Just (x, xs)) == f x xs
```

Intuitively, we've jammed the folding function and the initial value into one function by replacing the input argument with a sum type. To run `f`, we pass a `Just …`. To get a `z`, we pass `Nothing`.

Generalizing, any fold can be specified with a function `g :: F a r -> r` where `F a` is a functor suitable for the underlying data type. In the case of lists, `type F a r = Maybe (a, r)`.

We have used `Maybe` lest define a new data type, but we can also rename them into the more suggestive names:

```hs
data F a r = …

-- generalized fold handler: `F a` is a functor
g :: (Functor f a) => F a r -> r

-- in the case of lists:
type F a r = Maybe (a, r)

-- initial definition of List is recursive:
data List a = Cons a (List a) | Nil

-- now we have a non-recursive definition:
data ListF a r = ConsF a r | NilF
```

The last definition is almost identical to the initial List definition, only here, the recursion is replaced with the type variable `r`. Instead of having a recursive call, `(List a)`, in the definition for the `Cons` data ctor, as `Cons a (List a)`, we now define it as `ConsF a r`.

This allows us to improve the diagram a bit:

```
○                   ∙ ListF a R
                    |
                    |
                    | g
                    |
      fold g        ↓
∙------------------>∙ R
List a
```

The last step is to somehow relate `List a` and `ListF a r`.

What if we had `ListF a (List a)` by literally substituting `List a` back into the functor. We'd expect this to be related to `List a`, and indeed there is a unique function which converts between the two list representations:

```hs
-- f-algebra
--  alg :: f a -> a
alg :: ListF a (List a) -> List a
alg (ConsF x xs) = Cons x xs
alg NilF = Nil
```

And the diagram develops further:

```
ListF a (List a)
∙                   ∙ ListF a R
|                   |
|                   |
| alg               | g
|                   |
↓     fold g        ↓
∙------------------>∙ R
List a
```

There is one last piece to the puzzle: how do we convert from `ListF a (List a)` to `ListF a r`? Well, we already have a function `fold g :: List a -> r`, so all we need to do is lift it with `fmap`.

```
ListF a (List a)
∙------------------>∙ ListF a r
|  fmap (fold g)    |
|                   |
| alg               | g
|                   |
↓        fold g     ↓
∙------------------>∙ r
List a
```

`fold g . alg` == `g . fmap (fold g))`


* Dualization only ever flips the arrows (functions), not the objects (types)



## Flipping arrows in coBurger King

Category theory crash course for the working Haskell programmer    
http://blog.ezyang.com/2010/07/flipping-arrows-in-coburger-king/

A frequent question that comes up when discussing the dual data structures, and most frequently *comonad*, is "What does the co- mean?" The snippy category theory answer is: "Flipping the arrows around." This is confusing, because if you look at one variant of the monad and comonad typeclasses:

```hs
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

class Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  extract :: w a -> a
```

there are a lot of arrows, and only a few of them flipped. This article will make precise what it means to "flip arrows" and use the dual category.

In the follwoing diagrams you can read any node (aka object) as a Haskell type, and any solid arrow (aka morphism) as a Haskell function between those types.

Functors

The Functor typeclass is familiar to the working Haskell programmer:

```hs
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

While the typeclass seems to imply that there is only one part to an instance of Functor (i.e. the implementation of `fmap`), there is another, almost trivial part: `f` is a type function of kind `Type -> Type` that takes a type, `a` and outputs a new type, `f a`.

```
a         f         b
∙------------------->∙
|     '              |
|ηₐ   'F           F |ηᵦ
|     '              |
↓     ↓   F f        ↓
∙------------------->∙
F a     fmap f     F b
```

Functor is a mapping between two categories, `F : C -> D`, that maps all objects in C to objects in D, and all arrows in C to arrows in D, while preserving identity and composition of arrows.

In Haskell, many types are instances of the `Functor` class: `Maybe`, `[]`, `Either e`, `(,) a`, `(->) a`, etc. And the categories are various subsets of Haskells' types. The category theoretic `η` function is the `pure :: a -> f a` function in Haskell, and the `F f` translates to `fmap f`. The identity functions like `1ₐ` are all covered by the Haskell's polymorhic `id` function.

```hs
-- 2 different functors, F and G
fF = fmap @[]    :: forall a b. (a -> b) -> [a] -> [b]
fG = fmap @Maybe :: forall a b. (a -> b) -> Maybe a -> Maybe b

-- component of natural transformation
α :: forall f g a. (Functor f, Functor g) => f a -> g a

-- naturality condition
αᵦ . F    f = G    f . αₐ
α  . fmap f = fmap f . α

-- List (F) to Maybe (G)
headMaybe :: [a] -> Maybe a
headMaybe xs = case xs of
  []   -> Nothing
  x:xs -> Just x

-- check if naturality condition holds:
headMaybe . fmap f == fmap f . headMaybe

-- it does
headMaybe . fmap f $ xs == fmap f . headMaybe $ xs

-- because, when the functors are specialized to List and Maybe:
headMaybe . fF f $ xs == fG f . headMaybe $ xs

-- we have the List functor on the LHS
headMaybe . fmap @[] f $ xs

-- and the Maybe functor on the RHS
fmap @Maybe f . headMaybe $ xs

-- and the produced result is the same!

-- i.e.
-- first mapping a list, then taking the headMaybe is
-- the same as taking the headMaybe and then mapping it.
headMaybe . fmap f $ xs == fmap f . headMaybe $ xs
```


While the red arrows represents concrete function a -> b (the first argument of fmap), the dashed blue arrow does not claim that a function a -> t a exists: it's simply indicating how the functor maps from one type to another. It could be a type with no legal values! We could also posit the existence of a function of that type; in that case, we would have a pointed functor:

class Functor f => Pointed f where
  pure :: a -> f a -- aka return
