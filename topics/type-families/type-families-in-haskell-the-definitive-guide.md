# Type Families in Haskell: The Definitive Guide

Learn about type families, one of the most powerful type-level programming features in Haskell.

(...)

## Type constructor arity

The arity of a type constructor is the number of arguments it requires at use sites. It comes into play when we use higher-kinded types:

    type S :: (Type -> Type) -> Type
    data S k = MkS (k Bool) (k Integer)
    

Now, what constitutes a valid argument to `S`? One might be tempted to think that any type constructor of kind `Type -> Type` could be used there. Let's try a few:

*   `MkS (Just True) Nothing :: S **Maybe**`
*   `MkS (Left "Hi") (Right 42) :: S (**Either String**)`
*   `MkS (Identity False) (Identity 0) :: S **Identity**`

So `Maybe`, `Either String`, and `Identity` have all worked fine. But what about a type synonym?

    type Pair :: Type -> Type
    type Pair a = (a, a)
    

From the standalone kind signature, we see that it has the appropriate kind `Type -> Type`. GHCi also confirms this:

    ghci> :kind Pair
    Pair :: Type -> Type
    

And yet, any attempt to use `S Pair` is unsuccessful:

    ghci> MkS (True, False) (0, 1) :: S Pair
    <interactive>:6:29: error:
        • The type synonym 'Pair' should have 1 argument,
          but has been given none
    

Due to certain design decisions in GHC's type system, type synonyms cannot be partially applied. In the case of `Pair`, we say that its arity is 1, as it needs one argument: `Pair Bool`, `Pair Integer`, and `Pair String` are all fine. On the other hand, `S Pair` or `Functor Pair` are not. The use of a type constructor where its arity requirements are met is called _saturated_, and _unsaturated_ otherwise.

Note that we only need the notion of arity for type constructors that can reduce to other types when applied to an argument. For instance, `Pair Bool` is equal not only to itself but also to `(Bool, Bool)`:

*   `Pair Bool ~ Pair Bool -- reflexivity`
*   `Pair Bool ~ (Bool, Bool) -- reduction`

On the other hand, `Maybe Bool` is only equal to itself:

*   `Maybe Bool ~ Maybe Bool -- reflexivity`

We thus call `Maybe` a _generative_ type constructor, while `Pair` is _non-generative_.

Non-generative type constructors have arities assigned to them and must be used saturated. Generative type constructors are not subject to such restrictions, so we do not apply the notion of arity to them.


Type family applications can also reduce to other types:

```hs
Append [1,2] [3,4] ~ Append [1,2] [3,4]  -- reflexivity
Append [1,2] [3,4] ~ [1, 2, 3, 4]        -- reduction
```

Therefore, they are non-generative and have arities assigned to them. The arity is determined at definition site by taking into account the kind signature and the header:

    type Append :: forall a. [a] -> [a] -> [a]
    type family Append xs ys where
    

In the header, we have `Append xs ys` rather than `Append xs` or simply `Append`. So, at first glance it may seem that the arity of `Append` is 2. However, we must also account for the forall-bound variable `a`. In fact, even if you write `Append [1,2] [3,4]`, internally it becomes `Append @Nat [1,2] [3,4]`. Hence the arity of `Append` is 3.

That would also be true even if we didn't write out the `forall` explicitly:

    type Append :: [a] -> [a] -> [a]
    type family Append xs ys where
    

OK, so why is a header important? Couldn't we deduce the arity by counting the quantifiers in the kind signature? Well, that might work in most cases, but here's an interesting counter-example:

    type MaybeIf :: Bool -> Type -> Type
    type family MaybeIf b t where
      MaybeIf True  t = Maybe t
      MaybeIf False t = Identity t
    

This definition is assigned the arity of 2, and we can use it by applying it to two arguments:

    data PlayerInfo b =
      MkPlayerInfo { name  :: MaybeIf b String,
                     score :: MaybeIf b Integer }
  

This could be useful when working with a database. When reading a player record, we would expect all fields to be present, but a database update could touch only some of the fields:

    dbReadPlayerInfo :: IO (PlayerInfo False)
    dbUpdatePlayerInfo :: PlayerInfo True -> IO ()
    

In `PlayerInfo False` the fields are simply wrapped in Identity, e.g. `MkPlayerInfo { name = Identity "Jack", score = Identity 8 }`. In `PlayerInfo True` the fields are wrapped in Maybe and therefore can be Nothing, e.g. `MkPlayerInfo { name = Nothing, score = Just 10 }`.

However, `MaybeIf` cannot be passed to `S`:

    ghci> newtype AuxInfo b = MkAuxInfo (S (MaybeIf b))
    <interactive>:33:21: error:
        • The type family 'MaybeIf' should have 2 arguments,
          but has been given 1
        • In the definition of data constructor 'MkAuxInfo'
          In the newtype declaration for 'AuxInfo'
    

Fortunately, this problem is solved by a minor adjustment to the definition of `MaybeIf`:

    type MaybeIf :: Bool -> Type -> Type
    type family MaybeIf b where
      MaybeIf True  = Maybe
      MaybeIf False = Identity
    

Notice how the kind signature is unchanged, but the `t` parameter is removed from the header and the clauses. With this tweak, the arity of `MaybeIf` becomes 1 and the definition of `AuxInfo` is accepted.

Exercise: determine the arity of `Not`, `FromMaybe`, and `Fst`.


## The synergy with GADTs

The need for closed type families arises most often when working with GADTs. Here is a definition of heterogeneous lists:

    type HList :: [Type] -> Type
    data HList xs where
      HNil :: HList '[]
      (:&) :: x -> HList xs -> HList (x : xs)
    infixr 5 :&
    

It can be used to represent sequences of values of different types:

    h1 :: HList [Integer, String, Bool]
    h1 = 42 :& "Hello" :& True :& HNil
    
    h2 :: HList [Char, Bool]
    h2 = 'x' :& False :& HNil
    

Just as with normal lists, we can define operations such as computing the length:

    hlength :: HList xs -> Int
    hlength HNil = 0
    hlength (_ :& xs) = 1 + hlength xs
    
    ghci> hlength h1
    3
    
    ghci> hlength h2
    2
    

However, even for something as trivial as concatenation we need type-level computation:

    happend :: HList xs -> HList ys -> HList ??
    

What shall be the type of `happened h1 h2`? Well, it must include the elements of the first list and then the elements of the second. That is precisely what the `Append` type family implements:

    happend :: HList xs -> HList ys -> HList (Append xs ys)
    

And that is the typical reason one would reach for closed type families: to implement operations on GADTs.


## Evaluation order, or lack thereof

Haskell is a lazy language, and its evaluation strategy enables us to write code such as the following:

    ghci> take 10 (iterate (+5) 0)
    [0,5,10,15,20,25,30,35,40,45]
    

Let us now attempt a similar feat at the type level. First, we define type families that correspond to `take` and `iterate (+5)`:

    type IteratePlus5 :: Nat -> [Nat]
    type family IteratePlus5 k where
      IteratePlus5 k = k : IteratePlus5 (k+5)
        
    type Take :: Nat -> [a] -> [a]
    type family Take n a where
      Take 0 xs = '[]
      Take n (x : xs) = x : Take (n-1) xs
    

We can see that `Take` works as expected:

    ghci> :kind! Take 3 [0, 1, 2, 3, 4, 5]
    Take 3 [0, 1, 2, 3, 4, 5] :: [Nat]
    = '[0, 1, 2]
    

On the other hand, `IteratePlus5` sends the type checker into an infinite loop:

    ghci> :kind! Take 10 (IteratePlus5 0)
    ^CInterrupted.
    

Clearly, the evaluation of type families is not lazy. In fact, it is not eager either - the rules are not defined at all. Even when working with finite data, reasoning about time or space complexity of algorithms implemented as type families is impossible. [#18965](https://gitlab.haskell.org/ghc/ghc/-/issues/18965) is a GHC issue that offers a solution to this problem. In the meantime, it is a pitfall one must be aware of.


## Open type families

Let's say we want to assign a textual label to some types, possibly for serialization purposes:

    type Label :: Type -> Symbol
    type family Label t where
      Label Double = "number"
      Label String = "string"
      Label Bool   = "boolean"
      ...
    

We can reify the label at the term level using the `KnownSymbol` class:

    label :: forall t. KnownSymbol (Label t) => String
    label = symbolVal (Proxy @(Label t))
    
    ghci> label @Double
    "number"
    

But what if the user defines their own type `MyType` in another module? How could they assign a label to it, such that `label @MyType = "mt"`?

With closed type families, this is not possible. That is where open type families enter the picture. To make a type family open, we must omit the `where` keyword in its header:

    type Label :: Type -> Symbol
    type family Label t
    

The instances are no longer indented. Instead, they are declared at the top level, possibly in different modules, and prefixed with the `type instance` keyword sequence:

    type instance Label Double = "number"
    type instance Label String = "string"
    type instance Label Bool   = "boolean"
    

Now a user can easily define an instance of `Label` for their own type:

    data MyType = MT
    type instance Label MyType = "mt"
    
    ghci> label @MyType
    "mt"
    

At this point, one might start wondering why anybody would ever prefer closed type families if open type families seem to be more powerful and extensible. The reason for this is that extensibility comes at a cost: the equations of an open type family are not allowed to overlap. But overlapping equations are often useful!

## Overlapping equations

The clauses of a closed type family are ordered and matched from top to bottom. This allows us to define logical conjunction as follows:

    type And :: Bool -> Bool -> Bool
    type family And a b where
      And True True = True
      And _    _    = False
    

If we were to reorder them, the `And _ _` equation would match all inputs. But it comes second, so the `And True True` clause gets a chance to match. This is the key property of closed type families as opposed to open type families: the equations may be overlapping.

An open type family would need to enumerate all possibilities, leading to a combinatorial explosion:

    type And' :: Bool -> Bool -> Bool
    type family And' a b
    
    type instance And' True  True  = True
    type instance And' True  False = False
    type instance And' False True  = False
    type instance And' False False = False
    


## Compatible equations

To say that overlapping equations are disallowed in open type families and allowed in closed type families would be an oversimplification. In practice, the rules are a bit more intricate.

Open type family instances must be _compatible_. Type family instances are compatible if at least one of the following holds:

1.  Their left-hand sides are apart (i.e. not overlapping)
2.  Their left-hand sides unify with a substitution, under which the right-hand sides are equal.

The second condition enables GHC to accept more programs. Consider the following example:

    type family F a
    type instance F a    = [a]
    type instance F Char = String
    

While the left-hand sides clearly overlap (`a` is more general than `Char`), ultimately it makes no difference. If the user needs to reduce `F Char`, both equations will result in `[Char]`. The mathematically inclined readers will recognize this property as [confluence](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)).

Here's a more interesting example with several type variables:

    type family G a b
    type instance G a    Bool = a -> Bool
    type instance G Char b    = Char -> b
    

The left-hand sides unify with a substitution `a=Char`, `b=Bool`. The right-hand sides are equal under that substitution:

    type instance G Char Bool = Char -> Bool
    

It is therefore safe to accept both of them: they are compatible.

Instance compatibility also plays a role in closed type families. Consider `FInteger` and `FString`:

    type family FInteger a where
      FInteger Char = Integer
      FInteger a    = [a]
      
    type family FString a where
      FString Char = String
      FString a    = [a]
    

Now, for an unknown `x`, could GHC reduce `FInteger x` to `[x]`? No, because the equations are matched top-to-bottom, and GHC first needs to check whether `x` is `Char`, in which case it would reduce to `Integer`.

On the other hand, the equations in `FString` are compatible. So if we have `FString x`, it doesn't matter whether `x` is `Char` or not, as both equations will reduce to `[x]`.

## Injective type families

Some type families are [injective](https://en.wikipedia.org/wiki/Injective_function): that is, we can deduce their inputs from their outputs. For example, consider boolean negation:

    type Not :: Bool -> Bool
    type family Not x where
      Not True = False
      Not False = True
    

If we know that `Not x` is `True`, then we can conclude that `x` is `False`. By default, the compiler does not apply such reasoning:

    s :: forall x. (Not x ~ True, Typeable x) => String
    s = show (typeRep @x)
    
    ghci> s
    <interactive>:7:1: error:
        • Couldn't match type 'Not x0' with ''True'
            arising from a use of 's'
          The type variable 'x0' is ambiguous
    

Even though the compiler could instantiate `x` to `False` based on the fact that `Not x` is `True`, it did not. Of course, we could do it manually, and GHC would check that we did it correctly:

    ghci> s @False
    "'False"
    
    ghci> s @True
        <interactive>:12:1: error:
        • Couldn't match type ''False' with ''True'
            arising from a use of 's'
    

When we instantiate `x` to `False`, the `Not x ~ True` constraint is satisfied. When we attempt to instantiate it to `True`, the constrained is not satisfied and we see a type error.

There's only one valid way to instantiate `x`. Wouldn't it be great if GHC could do it automatically? That's exactly what injective type families allow us to achieve. Change the type family header of `Not` as follows:

    type family Not x = r | r -> x where
    

First, we give a name to the result of `Not x`, here I called it `r`. Then, using the syntax of functional dependencies, we specify that `r` determines `x`. GHC will make use of this information whenever it needs to instantiate `x`:

    ghci> s
    "'False"
    

This feature is enabled by the `TypeFamilyDependencies` extension. As with ordinary functional dependencies, it is only used to guide type inference and cannot be used to produce equalities. So the following is, unfortunately, rejected:

    not_lemma :: Not x :~: True -> x :~: False
    not_lemma Refl = Refl
        
        
    

That is a known limitation.

## Associated types


From a code organization perspective, sometimes it makes sense to associate an open type family with a class.

Consider the notion of containers and elements:

    type family Elem a
    class Container a where
      elements :: a -> [Elem a]
      
    type instance Elem [a] = a
    instance Container [a] where
      elements = id
    
    type instance Elem ByteString = Word8
    instance Container ByteString where
      elements = ByteString.unpack
    

We would only use Elem with types that also have a Container instance, so it would be more clear to move it into the class. That is exactly what associated types enable us to do:

    class Container a where
      type Elem a
      elements :: a -> [Elem a]
      
    instance Container [a] where
      type Elem [a] = a
      elements = id
     
    instance Container ByteString where
      type Elem ByteString = Word8
      elements = ByteString.unpack
    

Associated types are mostly equivalent to open type families, and which one to prefer is often a matter of style.

One advantage of associated types is that they can have defaults:

    type family Unwrap x where
      Unwrap (f a) = a
      
    class Container a where
      type Elem a
      type Elem x = Unwrap x
      elements :: a -> [Elem a]
    

This way, we can avoid explicit definition of `Elem` in most instances:

    instance Container [a] where
      elements = id
      
    instance Container (Maybe a) where
      elements = maybeToList
      
    instance Container ByteString where
      type Elem ByteString = Word8
      elements = ByteString.unpack
    

Current research indicates that associated types are a more promising abstraction mechanism than top-level open type families. See [ICFP 2017 - Constrained Type Families](https://www.youtube.com/watch?v=AGJY95Otb9U).

## Data families


Data families can be thought of as type families, instances of which are always new, dedicated data types.

Consider the following example:

    data family Vector a
    newtype instance Vector () = VUnit Int
    newtype instance Vector Word8 = VBytes ByteArray
    data instance Vector (a, b) = VPair !(Vector a) !(Vector b)
    

A `Vector` is a sequence of elements, but for the unit type we can simply store the length as `Int`, which is way more efficient than allocating memory for each unit value.

Notice how we can decide between `data` and `newtype` on a per-instance basis.

This example can be rewritten using type families as follows:

    type family VectorF a
     
    type instance VectorF () = VectorUnit
    data VectorUnit = VUnit Int
        
    type instance VectorF Word8 = VectorWord8
    data VectorWord8 = VBytes ByteArray
    
    type instance VectorF (a, b) = VectorPair a b
    data VectorPair a b = VPair (VectorF a) (VectorF b)
    

In this translation, there's a data type for every type family instance. However, even boilerplate aside, this is an imperfect translation. Data families offer us something else: the type constructor they introduce is generative, so we do not have to worry about its arity!

For example, the following code is valid:

    data Pair1 f x = P1 (f x) (f x)
    type VV = Pair1 Vector
    

On the other hand, `Pair1 VectorF` would be rejected, as this is not applied to its argument.

Data families can also be associated with a class:

    class Vectorizable a where
      data Vector a
      vlength :: Vector a -> Int
    

Just as with associated types and open type families, this is mostly a matter of code organization.

## Non-parametric quantification

In terms, `forall` is a parametric quantifier, and this fact can be used to reason about functions. For example, consider the type signature of the identity function:

    id :: forall a. a -> a
    

There's just one thing it can do with its argument: return it untouched. It could not, say, return 42 when given an integer:

    id :: forall a. a -> a
    id (x :: Int) = 42      
    id x = x
    

This is not only important for reasoning about code, but also to guarantee type erasure.

However, none of that applies to type families, which have their own interpretation of what `forall` is supposed to mean:

    type F :: forall a. a -> a
    type family F a where
      F (a :: Nat) = 42
      F a = a
    

This code is accepted and works without error:

    ghci> :kind! F 0
    F 0 :: Nat
    = 42
    
    ghci> :kind! F "Hello"
    F "Hello" :: Symbol
    = "Hello"
    


On the one hand, this hinders our ability to reason about type families. On the other hand, this basically amounts to Π-types at the kind level, so it can be put to good use.

## Non-linear patterns


In term-level functions, a variable can't be bound more than once:

    dedup (x : x : xs) = dedup (x : xs)   
    dedup (y : xs) = y : dedup xs
    dedup [] = []
    

If we want to check that two inputs are equal, we must do so explicitly with the `==` operator:

    dedup (x1 : x2 : xs) | x1==x2 = dedup (x1 : xs)
    dedup (y : xs) = y : dedup xs
    dedup [] = []
    

On the other hand, in type family instances the former style is also allowed:

    type family Dedup xs where
      Dedup (x : x : xs) = Dedup (x : xs)
      Dedup (y : xs) = y : Dedup xs
      Dedup '[] = '[]
    

The feature happens to be called non-linear patterns, but do not confuse it with linear types, which are not related.

## Conclusion

Type families are a powerful and widely used ([20%](https://mail.haskell.org/pipermail/ghc-steering-committee/2020-November/001876.html) of Hackage packages) feature. They were introduced in 2005 in the form of [associated type synonyms](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/at-syns.pdf), and remain a subject of active research to this day, with innovations such as [closed type families](https://richarde.dev/papers/2014/axioms/axioms-extended.pdf) (2013), [injective type families](https://richarde.dev/papers/2015/injective/injective-type-families-extended.pdf) (2015), and [constrained type families](https://richarde.dev/papers/2017/partiality/partiality-extended.pdf) (2017).

While a useful tool, type families must be used with great care due to open issues such as [#8095](https://gitlab.haskell.org/ghc/ghc/-/issues/8095) ("TypeFamilies painfully slow") and [#12088](https://gitlab.haskell.org/ghc/ghc/-/issues/12088) ("Type/data family instances in kind checking"). However, there are ongoing efforts to address these issues. Serokell's GHC department is committed to improving Haskell's facilities for type-level programming.

[Let us know](https://twitter.com/serokell) if you use type families and what you think of them!


[Source](https://serokell.io/blog/type-families-haskell)
