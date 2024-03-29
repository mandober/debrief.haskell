# Fixed-Length Vector Types 2015

*Fixed-length vector type* is a vector types that indicate the length of the vector in the type itself. It is one of the more straightforward applications of the various GHC type extensions. There's a lot of magic you can do with GHC's advanced type mechanisms, but fixed length vectors are a good first step to beginning to understand several extensions, including:

- ConstraintKinds
- DataKinds
- GADTs
- KindSignatures
- TypeFamilies
- TypeOperators
- OverloadedLists
- UndecidableInstances

We'll discuss two different ways to implement it
- using type-level nats
- using `GHC.TypeLits` to manipulate the numeric literals in types

In the wild, there is the `V` type in the [linear](http://hackage.haskell.org/package/linear-1.18.0.1/docs/Linear-V.html) package.

Most of the code in this article can be [found](https://github.com/mstksg/inCode/blob/master/code-samples/fixvec) here.


## Implementation

The basic idea is we'll have a type:

`Vec a n`

which is a vector with items of type `a`, whose length is somehow encoded in the `n`. The `n` can really only be a "kind" of thing that encodes the vector's length. We can represent this by giving it a "kind signature":

`data Vec :: Nat -> * -> *`

which says that the `Vec` type ctor takes 2 args: a type of kind `Nat` (it cannot be just any old type), and another type `a` that is the type of vector's elements.


## Type-Level Nats

(The code in this section for this type is [available online](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec/FVTypeNats.hs).

There are a couple of ways to find something for that `n` of the `Nat` kind, and one way is to define your own `Nat`:

```hs
data Nat = Z | S Nat deriving Show
```

This declaration gives you:
- type `Nat`
- value ctor `Z :: Nat`
- value ctor `S :: Nat -> Nat`

And with enabled _DataKinds_ you also get
- the kind `Nat`
- type `Z :: Nat` (`Z`, of the kind `Nat`)
- type ctor `S :: Nat -> Nat`, which takes a type of kind `Nat` and returns a type of kind `Nat`

We can check this out in GHCi:

```hs
ghci> :set -XDataKinds
ghci> data Nat = Z | S Nat
ghci> :k Z
Nat
ghci> :k S Z
Nat
ghci> :k S (S Z)
Nat
```

Now we can define our `Vec` data type, with the _GADTs_ on:

```hs
data Vec :: Nat -> * -> * where
    Nil  :: Vec Z a
    (:#) :: a -> Vec n a -> Vec (S n) a

infixr 5 :#

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)
```

We also used the _KindSignatures_ extension to be able to give a kind signature to `Vec`…this is important because we want to make sure the first argument has to be a `Nat`. That is, we can't have anything silly like `Vec Bool Int`. We also have to put a separate _StandaloneDeriving_ extension standalone deriving clause instead of just having `deriving Show` because `Vec` isn't a type that can be expressed in "normal Haskell".

Note that our type is basically like a list:

```hs
data [] :: * -> * where
  []  :: [a]
  (:) :: a -> [a] -> [a]
```

Except now our type constructor actually has a new `Nat`.

This means that, because of *type erasure*, everything about our new type is basically going to be identical to `[]` at runtime. In-memory, this new type is essentially exactly `[]`, but its type has an extra tag that is erased at compile-time.

Okay, let's define some useful methods:

```hs
headV :: Vec (S n) a -> a
headV (x :# _)  = x

tailV :: Vec (S n) a -> Vec n a
tailV (_ :# xs) = xs
```

With these two, unlike those from Prelude, runtime errors cannot happen! We can only use `headV` and `tailV` on non-empty lists/vecs. The empty list won't typecheck because the empty list has the type `Vec Z a`. But `headV` and `tailV` only take things of type `Vec (S n) a`, for any `n :: Nat`.

The return of `tailV` is a vector of a length one less than the input vector. 


If you tried implementing this yourself, you might notice that you actually get an _error_ from GHC if you dare to try to handle the `Nil` case for `tailV` or `headV`. GHC knows when you've handled all possible cases, and gets mad if you try to handle a case that doesn't even make sense!


## Type families and appending

We can also "append" vectors. But we need a way to add `Nat`s together first. For that, we can use a type family, using the _TypeFamilies_ extension (with `TypeOperators`):

    type family (x :: Nat) + (y :: Nat) where
        'Z   + y = y
        'S x + y = 'S (x + y)

A "type family" is like a type level function. Compare this to defining `(+)` on the value level to the `Nat` _data_ type:

    (+#) :: Nat -> Nat -> Nat       -- types!
    Z   +# y = y
    S x +# y = S (x +# y)

Basically, we're defining a new type-level function `(+)` on two types `x` and `y`, both of kind `Nat`…and the result is their "sum". Convince yourself that this "addition" is actually addition. Now, let's use it for `appendV`:

    appendV :: Vec n a -> Vec m a -> Vec (n + m) a
    appendV Nil       ys = ys
    appendV (x :# xs) ys = x :# appendV xs ys

    ghci> let v1 = 1 :# 2 :# 3 :# Nil
    ghci> let v2 = 0 :# 1 :# Nil
    ghci> v1 `appendV` v2
    1 :# 2 :# 3 :# 0 :# 1 :# Nil
    ghci> :t v1 `appendV` v2
    v1 `appendV` v2 :: Vec (S (S (S (S (S Z))) Int


## Generating

It'd be nice to have type-safe methods of _generating_ these things, too…functions like `iterate`, or `enumFrom`. One of the ways to do this is by using a typeclass. (Available in a [separate file](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec/Unfoldable.hs) to try out).

    class Unfoldable v where
        unfold :: (b -> (a, b)) -> b -> v a

We're going to call `v` an `Unfoldable` if you can build a `v` from an "unfolding function" and an "initial state". Run the function on the initial value and get the first item and a new state. Run the function on the new state and get the second item and the next state.

The list instance should make it more clear:

    instance Unfoldable [] where
        unfold f x0 = let (y, x1) = f x0
                      in  y : unfold f x1

    ghci> take 5 $ unfold (\x -> (x `mod` 3 == 2, x^2 - 1)) 2
    [True, False, True, False, True]

Note that we can have an instance for any fixed-length vector type…where the thing "cuts off" after it's filled the entire vector:

    instance Unfoldable (Vec Z) where
        unfold _ _ = Nil
    
    instance Unfoldable (Vec n) => Unfoldable (Vec (S n)) where
        unfold f x0 = let (y, x1) = f x0
                      in  y :# unfold f x1

Take a moment to think about what these instances are doing.

You can create a `Vec Z a` from an unfolding function pretty easily, because the only thing with type `Vec Z a` is `Nil`. So just ignore the function/initial state and return `Nil`.

The instance for `Vec (S n)` is slightly more involved. To make a `Vec (S n) a`, you need an `a` and a `Vec n a`. You can get the `a` from the unfolding function…but where will you get the `Vec n a` from? Well, you can use `unfold` to make a `Vec n a`! But that only makes sense if `Vec n` is an `Unfoldable`.

So, that's why in the instance for `Vec (S n)`, we constrain that `Vec n` must also be an `Unfoldable`. We make our result by using our function to create an `a` and `unfold` to create a `Vec n a` (provided `Vec n` is an `Unfoldable`).

Note that this style of declaration looks a lot like induction. We define our instance for zero…and then we say, "if `n` is an instance, then so is `S n`". Induction!

Let's see this in action.

```hs
replicateU :: Unfoldable v => a -> v a
replicateU = unfold (\x -> (x, x))

iterateU :: Unfoldable v => (a -> a) -> a -> v a
iterateU f = unfold (\x -> (x, f x))

fromListMaybes :: Unfoldable v => [a] -> v (Maybe a)
fromListMaybes = unfold $ \l -> case l of
                                  []   -> (Nothing, [])
                                  x:xs -> (Just x , xs)

ghci> replicateU 'a'       :: Vec (S (S (S Z))) Char
'a' :# 'a' :# 'a' :# Nil
ghci> replicateU 'a'       :: Vec Z Char
Nil
ghci> iterateU succ 1      :: Vec (S (S (S (S Z)))) Int
1 :# 2 :# 3 :# 4 :# Nil
ghci> fromListMaybes [1,2] :: Vec (S (S (S Z))) (Maybe Int)
Just 1 :# Just 2 :# Nothing :# Nil
ghci> tailV (iterateU succ 1 :: Vec (S Z) Int)
Nil
```

Note that `replicateU` doesn't need to take in an `Int` parameter, like the on in Prelude, to say how many items to have. It just replicates enough to fill the entire vector we want!

### Common Typeclasses

We can go in and implement common typeclasses, too. All the ones you'd expect.

We can actually use the _DeriveFunctor_ extension to write a `Functor` instance, but let's write one on our own just for learning purposes:

    instance Functor (Vec n) where
        fmap _ Nil       = Nil
        fmap f (x :# xs) = f x :# fmap f xs

For `Applicative`, it isn't so simple. The Applicative instance is going to be the "ZipList" instance…so we have to be able to make a `pure` that depends on the type, and a `(<*>)` that depends on the type, too.

    instance Applicative (Vec Z) where
        pure _    = Nil
        Nil <*> _ = Nil
    
    instance Applicative (Vec n) => Applicative (Vec (S n)) where
        pure x = x :# pure x
        (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)

For `Vec Z`, it's just `Nil`. For `Vec (S n)`…for pure, you need `x :#` something…and that something has to be a `Vec n a`. That's just `pure` for `Vec n`! Remember, we can't assume that `Vec n` is an `Applicative` just because `Vec (S n)` is. So we need to add a constraint, that `Vec n` an Applicative. Induction, again!

For `(<*>)`, we can get the first item easily, it's just `f x`. But for the next item, we need a `Vec n a`. Luckily…we have exactly that with the `(<*>)` for `Vec n`!

Remember, at the end, we're saying "We have an `Applicative` instance for _any_ type `Vec n`". The instance for `Vec Z` has `pure _ = Nil`. The instance for `Vec (S Z)` has `pure x = x :# Nil`. The instance for `Vec (S (S Z))` has `pure x = x :# x :# Nil`, etc. etc.

    ghci> fmap (*2) (1 :# 2 :# 3 :# Nil)
    2 :# 4 :# 6 :# Nil
    ghci> pure 10 :: Vec (S (S Z)) Int
    10 :# 10 :# Nil         -- like replicateV!
    ghci> liftA2 (+) (1 :# 2 :# 3 :# Nil) (100 :# 201 :# 302 :# Nil)
    101 :# 203 :# 305 :# Nil

I'll leave the `Monad` instance as an exercise, but it's in the source files for this post. `join` for this instance should be a "diagonal" - the first item of the first vector, the second item of the second vector, the third item of the third vector, etc.

We can define `Foldable` and `Traversable` the same way. Like for `Functor`, GHC can derive these with _DeriveFoldable_ and _DeriveTraversable_…but we'll do it again here just to demonstrate.

    instance Foldable (Vec Z) where
        foldMap _ Nil = mempty
    
    instance Foldable (Vec n) => Foldable (Vec (S n)) where
        foldMap f (x :# xs) = f x <> foldMap f xs
    
    instance Traversable (Vec Z) where
        traverse _ Nil = pure Nil
    
    instance Traversable (Vec n) => Traversable (Vec (S n)) where
        traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)

Note that we can only use `foldMap f xs` on `xs :: Vec n a`, if `Vec n` is a `Foldable`. So that's why we add that constraint.

Again, `liftA2 (:#) :: Applicative f => f a -> f (Vec n a) -> f (Vec (S n) a)`…so this only makes sense if `traverse f s` gives us a `Vec n a`. So we have to add that as a constraint.

    ghci> toList $ 1 :# 2 :# 3 :# Nil
    [1,2,3]
    ghci> traverse Identity $ 1 :# 2 :# 3 :# Nil
    Identity (1 :# 2 :# 3 :# Nil)
    ghci> sequence_ $ putStrLn "hello" :# putStrLn "world" :# Nil
    "hello"
    "world"
    ghci> sequence $ Just 1 :# Just 2 :# Nil
    Just (1 :# 2 :# Nil)
    ghci> sequence $ Just 1 :# Nothing :# Nil
    Nothing

`Traversable` of course opens a whole lot of doors. For example, we can write a "safe `fromList`":

    fromListU :: (Unfoldable v, Traversable v) => [a] -> Maybe (v a)
    fromListU = sequence . fromListMaybes

    ghci> fromListU [1,2,3] :: Maybe (Vec (S Z) Int)
    Just (1 :# Nil)
    ghci> fromListU [1,2,3] :: Maybe (Vec (S (S (S Z))) Int)
    Just (1 :# 2 :# 3 :# Nil)
    ghci> fromListU [1,2,3] :: Maybe (Vec (S (S (S (S Z)))) Int)
    Nothing

And, if you're on GHC 7.8+, you have access to the _OverloadedLists_ language extension, where you can interpret list literals as if they were other structures.

We've already already implemented both `fromList` and `toList`, in a way, already, so this should be a breeze. The only trick you might see is that the `IsList` typeclass asks for a type family to return the _type of the element in the container_ from the container type.

    instance (Unfoldable (Vec n), Traversable (Vec n)) => L.IsList (Vec n a) where
        type Item (Vec n a) = a
        fromList xs = case fromListU xs of
                        Nothing -> error "Demanded vector from a list that was too short."
                        Just ys -> ys
        toList      = Data.Foldable.toList

    ghci> :set -XOverloadedLists
    ghci> [1,2,3] :: Vec (S (S Z)) Int
    1 :# 2 :# Nil
    ghci> [1,2,3] :: Vec (S (S (S (S Z)))) Int
    *** Exception: Demanded vector from a list that was too short.
    ghci> [1,3..] :: Vec (S (S (S (S Z)))) Int
    1 :# 3 :# 5 :# 7 :# Nil

Neat! All of the benefits of list literals that _OverloadedLists_ offers is now available to us.[2](#fn2) Unfortunately, you now open yourself up to runtime errors, so…it's actually a really bad idea for safety purposes unless you stick to only using it with infinite lists or are very disciplined. (Unless you really want to use list syntax, `fromListU` is probably a safer choice for finite lists!)

### Indexing[top](#title)

It'd be nice to be able to index into these, of course. For type-safe indexing, we can take advantage of a trick using the `Proxy` type.

Many might remember having to get a `TypeRep` for a `Typeable` instance by doing something like `typeOf (undefined :: IO Double)`. That's because `typeOf :: Typeable a => a -> TypeRep`. If you wanted to get the `typeRep` for an `IO Double` using `typeOf`, you have to pass in an `IO Double`. But if you don't have one at hand, you can just use `undefined` with a type annotation. It's a bit of a dirty hack, but it works because `typeOf` doesn't care about the first argument's value…just its type.

These days, we like to be a bit less embarrassing and use something called `Proxy`:

`Proxy a` is a bit like `()`. It only has one constructor, and doesn't take any arguments. But we can use the type signature to "pass in types" to functions, as "arguments".

We have a couple of options here. One is to make a typeclass for type level nats to turn them into an `Integer` or a value-level `Nat`, and then do an "unsafe indexing" after verifying, through types, that the index is smaller than the length.

However, this is a little bit silly because we're just doing an unsafe indexing in the end anyway, so the compiler can't help us at all. Wouldn't it be nice if we could get the compiler on our side and write a _real_ safe index?

There are many ways to approach this problem, but one way is to make a specific `Index` typeclass: (or make another typeclass like `Take`, and write `index` in terms of it)

    class Index (n :: Nat) v where
        index :: Proxy n -> v a -> a

Here, we can say that `n` and `v` are instances of `Index n v` if and only if you can safely (totally) index into `v a` at index `n`. That is, if every value of type `v a` ever has an index at `n`, a `Nat`. (By the way, we need _MultiParamTypeClasses_ to be able to make a type class with two parameters)

So, `n ~ S Z` and `v ~ Vec (S (S Z)) a` has an instance, because you can get the element (the second element) from _any_ value of type `Vec (S (S Z)) a` (a length-two vector).

But `n ~ S Z` and `v ~ Vec (S Z) a` does _not_. There are actually _no_ length-1 vectors that have a index (second element).

Note that we use the `Proxy` trick we discussed, so that we can indicate somehow what index we really want. It is a trick that basically allows us to pass a _type_ (`S Z`, `S (S Z)`, etc.) as a "value".

Let's write our instances - but only the instances that _make sense_.

    instance Index Z (Vec (S n)) where
        index _ (x :# _) = x
    
    instance forall n m. Index n (Vec m) => Index (S n) (Vec (S m)) where
        index _ (_ :# xs) = index (Proxy :: Proxy n) xs

The first case instance makes sense. We can definitely index at index `Z` (zero) of _any_ `Vec (S n) a` - the only thing we can't index `Z` into is `Vec Z a`. So, if our vector is of length 1 or higher, we can index at position 0.

The second case says that, if we can index into `n` of a `Vec m a`, then of course we can index into an `S n` of a `Vec (S m) a`. To index into `S n` of a `Vec (S m) a`, all we need to do is index into `n` of the `Vec m a` tail!

We have to use the _ScopedTypeVariables_ extension to enable us to use, with the `forall` statement, the `n` in our instance when we are writing our type for `Proxy`. If we didn't, the `n` in `Proxy n` in our `index` definition would be considered unrelated by GHC to the `n` in the instance statement, `Index (S n) (Vec (S m))`. (This is the only reason we need the `forall`)

In any case, note the similarity of this algorithm to the actual indexing function on lists:

    0 !! (x:_ ) = x
    n !! (_:xs) = (n - 1) !! xs

trying it out…

    ghci> index (Proxy :: Proxy (S (S Z))) (1 :# 2 :# 3 :# Nil)
    3
    ghci> index (Proxy :: Proxy (S (S Z))) (1 :# 2 :# Nil)
    *** Compile error!

It's an error, but remember, it's a _compiler_ error, that happens before any code is ever even run! No more indexing errors at runtime! Kiss your days of hunting segfault errors in C goodbye!

This is something I haven't really been able to find a good answer too. But notice that we actually could have written a "bad" instance of the second instance of `Index`:

    instance Index (S n) (Vec (S m)) where
        index _ (x :# _) = x

And this compiles fine…but gives the wrong behavior, or at least the behavior we don't want!

Does anybody know a way to state the type of `Index` or `index` in a way that implementations like this are impossible?

There's a "fundamental" problem here, it seems, because we can't really demand or specify anything by the return type, like we could in the other examples. In the other examples, we sort of restricted the implementation by choosing our return type carefully…but for here, it's just `a`. I'd love to hear if anyone has any thoughts on this.

You might notice that it's a bit of a plain to write `S (S (S (S Z)))`, etc., especially for large numbers. And I wouldn't even think about writing it for the hundreds.

We'll "fix" this in the next section. However, even before this, you actually can generate these "automatically" with template haskell, using techniques from [Functional Pearls: Implicit Configurations](http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf), and the [linear](http://hackage.haskell.org/package/linear-1.18.0.1/docs/Linear-V.html) package does just this. (This path slipped my mind before I posted because I didn't really consider template Haskell, and I think I'll edit in a section here soon). With this in mind, I still don't really consider Template Haskell an optimal or clean approach :)

Using TypeLits and Type Checker Plugins[top](#title)
----------------------------------------------------

(This next section uses code that is [also available online](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec/FVTypeLits.hs), as well!)

Using a custom `Nat` kind and _DataKinds_ is nice and all, but it's a bit of a hassle to express large numbers like 100, 1000, etc. However, as of GHC 7.8, we've had the ability to actually _use_ numeric (integer) literals in our types. Instead of writing `S (S Z)`, we can write `2`.

GHC can't yet quite work with that well by default. It has trouble proving statements about variables, like `(n + 1) ~ (1 + n)` (that `n + 1` is "the same as" `1 + n`). Fortunately for us, since GHC 7.10, we have a way to "extend" the type checker with custom plugins that _can_ prove things like this for us. (Note that this `+` is the one from `GHC.TypeLits`…not the one we defined earlier.)

The _[ghc-typelits-natnormalise](https://hackage.haskell.org/package/ghc-typelits-natnormalise)_ package is a package providing such a plugin. We can have GHC use it to extend its type checking by passing in `-fplugin GHC.TypeLits.Normalise` when we execute our code, or by adding a pragma:

    {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

to the top of our file, along with our `LANGUAGE` pragmas. (Assuming, of course, a GHC 7.10+)

    ghci> :set -XDataKinds -XTypeOperators -XTypeFamilies
    ghci> import GHC.TypeLits
    ghci> Proxy :: ((n + 1) ~ (1 + n)) => Proxy n
    *** Compile error: Cannot match `1 + n` with `n + 1`
    ghci> :set -fplugin GHC.TypeLits.Normalise
    ghci> Proxy :: ((n + 1) ~ (1 + n)) => Proxy n
    Proxy   -- success!

GHC now uses the plugin to prove that the two are really equivalent.

If you wanted to play along or try out the code samples, I recommend you use a sandbox:

    # in directory of your choice
    $ cabal sandbox init
    $ cabal install ghc-typelits-natnormalise
    $ cabal exec bash
    # now the package is in scope, when you use ghci or runghc

With that in mind, let's start restating everything in terms of _TypeLits_ and see what it gains us.

    data Vec :: Nat -> * -> * where
        Nil  :: Vec 0 a
        (:#) :: a -> Vec (n - 1) a -> Vec n a
    
    infixr 5 :#
    
    deriving instance Show a => Show (Vec n a)
    deriving instance Eq a => Eq (Vec n a)

A little nicer, right? `Nil` is a `Vec 0 a`, and `x :# xs` is an element with a `Vec (n - 1) a`, which overall is a `Vec n a`. Let's go over everything again to see how it'd look in the new regime. (Note that the kind of the type number literals is also called `Nat`…unrelated to our `Nat` we used before.)

A new look[top](#title)
-----------------------

First of all, we're going to have to define _TypeLit_ comparison operators, as they aren't built in in a useful way.

We have the type family (remember those?) `CmpNat x y`, which returns an `Ordering` (`LT`, `EQ`, or `GT`) type (of kind `Ordering`, using _DataKinds_…lifting a type and its value constructors to a kind and its types), which is provided and defined for us by GHC in `GHC.TypeLits`.

So defining a `x > y` constraint is pretty straightforward:

    type x > y = CmpNat x y ~ 'GT

Note that we need the _ConstraintKinds_ extension for this to work, as `1 > 2` is now a _constraint_, of kind `Constraint`.

Given this, let's do our favorite list functions, `headV` and `tailV`:

    headV :: (n > 0) => Vec n a -> a
    headV (x :# _)  = x
    
    tailV :: (n > 0) => Vec n a -> Vec (n - 1) a
    tailV (_ :# xs) = xs

Magnificent!

    ghci> headV (Nil :: Vec 0 ())
    -- Error!  Cannot unite 'EQ with 'GT

Neat! The error, remember, is at _compile time_, and not at runtime. If we ever tried to do an unsafe head, our code wouldn't even _compile_! The error message comes from the fact that we need , but we have instead. We have `EQ`, but we need `GT`.

There is one problem here, though - GHC gives us a warning for not pattern matching on `Nil`. But, if we do try to pattern match on `Nil`, we get a type error, like the same one we got when using our custom type nats. I think this is probably something that a plugin or sufficiently smart `CmpNat` might be able to handle…but I'm not totally sure. Right now, the best thing I can think of is just to do a wildcard match, `headV _ = error "What?"`, knowing that that case will never be reached if your program compiles successfully.

Moving on, we see that we don't even have to do any extra work to define our own type family `x + y`…because `GHC.TypeLits` already defines it for us! So, we can instantly write….

    appendV :: Vec n a -> Vec m a -> Vec (n + m) a
    appendV Nil       ys = ys
    appendV (x :# xs) ys = x :# appendV xs ys

    ghci> let v1 = 1 :# 2 :# 3 :# Nil
    ghci> let v2 = iterateU succ 0 :: Vec 2 Int
    ghci> v1 `appendV` v2
    1 :# 2 :# 3 :# 0 :# 1 :# Nil
    ghci> :t v1 `appendV` v2 :: Vec 5 Int
    v1 `appendV` v2 :: Vec 5 Int

And our list generating typeclasses -

    instance Unfoldable (Vec 0) where
        unfold _ _ = Nil
    
    instance (Unfoldable (Vec (n - 1)), n > 0) => Unfoldable (Vec n) where
        unfold f x0 = let (y, x1) = f x0
                      in  y :# unfold f x1

The translation is pretty mechanical, but I think that this new formulation looks…really nice, and really powerful. "If you can build a list from and , then you can build a list for !

Note that because our definitions of `replicateU`, `iterateU`, and `fromListMaybes` was polymorphic over all `Unfoldable`, we can actually re-use them from before:

    ghci> iterateU succ 1 :: Vec 3 int
    1 :# 2 :# 3 :# Nil
    ghci> iterateU succ 1 :: Vec 10 Int
    1 :# 2 :# 3 :# 4 :# 5 :# 6 :# 7 :# 8 :# 9 :# 10 :# Nil
    ghci> replicateU 'a' :: Vec 4 Char
    'a' :# 'a' :# 'a' :# 'a' :# Nil

The actual types are much nicer, too - we can write `Vec 10 Int` instead of `Vec (S (S (S (S (S (S (S (S (S (S Z)))))))))) Int` without resorting to template haskell.

Going through all of our other typeclasses/functions and making the adjustments… (remembering that we can also derive `Functor`, `Traversable`, and `Foldable` using GHC)

    instance Functor (Vec n) where
        fmap _ Nil       = Nil
        fmap f (x :# xs) = f x :# fmap f xs
    
    instance Applicative (Vec 0) where
        pure _    = Nil
        Nil <*> _ = Nil
    
    instance (Applicative (Vec (n - 1)), n > 0) => Applicative (Vec n) where
        pure x = x :# pure x
        (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)
    
    instance Foldable (Vec 0) where
        foldMap _ Nil = mempty
    
    instance (Foldable (Vec (n - 1)), n > 0) => Foldable (Vec n) where
        foldMap f (x :# xs) = f x <> foldMap f xs
    
    instance Traversable (Vec 0) where
        traverse _ Nil = pure Nil
    
    instance (Traversable (Vec (n - 1)), n > 0) => Traversable (Vec n) where
        traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)
    
    class Index (n :: Nat) v where
        index :: Proxy n -> v a -> a
    
    instance (m > 0) => Index 0 (Vec m) where
        index _ (x :# _) = x
    
    instance forall n m. (Index (n - 1) (Vec (m - 1)), n > 0, m > 0) => Index n (Vec m) where
        index _ (_ :# xs) = index (Proxy :: Proxy (n - 1)) xs
    
    instance (Unfoldable (Vec n), Traversable (Vec n)) => L.IsList (Vec n a) where
        type Item (Vec n a) = a
        fromList xs = case fromListU xs of
                        Nothing -> error "Demanded vector from a list that was too short."
                        Just ys -> ys
        toList      = Data.Foldable.toList

(Remember, we use the `forall` here with _ScopedTypeVariables_ to be able to say that the `n` in the type signature is the same `n` that is in the type of `Proxy`)

    ghci> fromListU [1,2,3,4] :: Vec 10 Int
    Nothing
    ghci> fromListU [1,2,3,4] :: Vec 3 Int
    Just (1 :# 2 :# 3 :# Nil)
    ghci> index (Proxy :: Proxy 2) (1 :# 2 :# 3 :# Nil)
    3
    ghci> index (Proxy :: Proxy 2) (1 :# 2 :# Nil)
    *** Type Error: Couldn't match 'EQ with 'GT
    ghci> :set -XOverloadedLists
    ghci> [1,2,3] :: Vec 2 Int
    1 :# 2 :# Nil
    ghci> [1,2,3] :: Vec 4 Int
    *** Exception: Demanded vector from a list that was too short.
    ghci> [1,3..] :: Vec 5 Int
    1 :# 3 :# 5 :# 7 :# 9 :# Nil

I think, overall, this formulation gives a much nicer interface. Being able to just write is pretty powerful. The usage with _OverloadedLists_ is pretty clean, too, especially when you can do things like `[1,3..] :: Vec 10 Int` and take full advantage of list syntax and succinct vector types. (Minding your runtime errors, of course)

However, you do again get the problem that GHC is not able to do real completeness checking and asks for the `Nil` cases still of everything…but adding a `Nil` case will cause a type error. The only solution is to add a `_` wildcard chase, but…again, this isn't quite satisfactory.[3](#fn3) If anybody has a way to get around this, I'd love to know :)

Alternative Underlying Representations[top](#title)
---------------------------------------------------

Recall that our `Vec` was basically identically the normal list type, with an extra field in the type. Due to type erasure, the two are represented exactly the same in memory. So we have appends, indexing, etc. Our type is essentially equal to

    newtype Vec :: Nat -> * -> * where
        VecList :: [a] -> Vec n a

For this type, though, we'd need to use "smart constructors" and extractors instead of `1 :# 2 :# Nil` etc.

We could, however, chose a more efficient type, like `Vector` from the _[vector](http://hackage.haskell.org/package/vector-0.10.12.2/docs/Data-Vector.html#t:Vector)_ package:

    newtype Vec :: Nat -> * -> * where
        VecVector :: Vector a -> Vec n a

And, if you made sure to wrap everything with smart constructors, you now have _type safe_ random indexing!

(This is representation is similar to the one used by the _[linear](http://hackage.haskell.org/package/linear-1.18.0.1/docs/Linear-V.html)_ package.)

More Operations[top](#title)
----------------------------

One really weird quirk with this is that many functions you'd normally write using pattern matching you'd now might start writing using typeclasses. One example would be our implementation of indexing, using an `IndexV` typeclass.

A bunch of one-shot typeclasses is sort of unideal, as typeclasses are sort of ugly and non-first-class. Ideally you'd only have a few typeclasses for as generic an interface as possible, and then be able to do everything from those. Sometimes this just isn't practical. I did mention one way around it, which was to make a typeclass to "reify" or turn your type into actual data, and then manipulate your data in an "unsafe" way knowing that the type checker checked that the data matched.

We'll demonstrate with `SomeNat` from `GHC.TypeLits`, but you can also make our own for our inductive `Nat` type we used in the first half, too.

If we use our "wrapped `Vector` approach", we can just do:

    newtype Vec :: Nat -> * -> * where
        Vec :: Vector a -> Vec n a
    
    index :: (KnownNat n, m > n) => Proxy n -> Vec m a -> a
    index p (Vec v) = v ! fromInteger (natVal p)

That is, `index` internally uses `(!)`, an unsafe operator…but only after we assure properly that it's safe to use by stating `m > n` in the constraint. We can be sure that GHC will catch any instance where someone tries to index into a `Vec m a` whose `m` is _not_ greater than the index desired.

The rest is up to you, though - to prove that indexing into a number smaller than `m` will always provide an answer. We have to make sure our smart constructors are okay and that `(!)` behaves like we think it does.

Singletons[top](#title)
-----------------------

Another answer to these sort of ad-hoc typeclasses is to use techniques involving singletons. Going all into how to use singletons to work with these is an article on its own…luckily, this article has already been written as [Part 1: Dependent Types in Haskell](https://www.fpcomplete.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell) by Hiromi ISHII. A major advantage is that you replace typeclasses with type families and more parameterized types. You'll have to work with an understanding of how singletons work, and accept using some template haskell to generate singleton types for your data types (or write them yourself!). But it's a powerful way to bring something like dependent types into Haskell, and there's already a lot of infrastructure of support on it on hackage and in the haskell dev ecosystem in general. I recommend looking at the linked article!

Conclusion[top](#title)
-----------------------

Hopefully you'll see that we are able to apply the full type-safety of the Haskell compiler to our programs regarding lists by encoding the length of the list in its type and limiting its operations by specifically typed functions and choice of instances. I also hope that you've been able to become familiar with seeing a lot of GHC's basic type extensions in real applications :)

Feel free to [download and run](https://github.com/mstksg/inCode/blob/master/code-samples/fixvec) any of the samples

Please let me know if I got anything wrong, or if there are any techniques that I should mention here that are out and in the wild today :)

* * *

1.  Can we get them out of Prelude? Please? :)[↩︎](#fnref1)
    
2.  By the way, the GHC wiki seems to claim that [using _OverloadedLists_ this way is impossible](https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists#Length-indexedobservedVectors). Anyone know what's going on here? Did we move fast and break everything?[↩︎](#fnref2)
    
3.  Interestingly enough, I think this is something where you could have the best of both situations with the Template Haskell method. But I'd hope for something that works on the beautiful TypeLits :'([↩︎](#fnref3)


[Source](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell-2015.html)
