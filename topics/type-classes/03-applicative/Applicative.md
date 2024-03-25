# Applicative

## Applicative functors

Just like the notion of `Functor` can be understood as a generalization of the map function for lists to other type constructors, the notion of *applicative functors* is a generalization of the `zip`/`zipWith` functions for lists.

However, for the sake of generality, it requires a particular variant of the `zip` and `zipWith` functions. We explain that setup first before we dive into the general notion of applicative functors.

## A family of zip functions

### zip and zipWith

The standard library provides several zip-like functions to merge lists. The basic one, `zip`, is defined as follows:

```hs
zip :: [a] -> [b] -> [(a,b)]
zip []     ys     = []
zip xs     []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
```

This merges the two given lists into a new list by combining the elements at the corresponding positions into a tuple. The new list comes to an end when one of the two lists runs out of elements. But what if we want to combine more lists? The standard library provides a number of similar functions for those occasions, but only up to 7 list args.

And if we are not interested in a tuple, but want to combine elements using a different function, then we use `zipWith`. There is, again, a family of such functions, but only up to 7 args.

```hs
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f []     ys     = []
zipWith f xs     []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
```

### zip vs zipWith

We don't need both `zip` and `zipWith` since they can be defined in terms of each other.

`zip` is `zipWith` where the function is the tuple data constructor. Conversely, `zipWith` can be defined in terms of `zip` and `map`.

```hs
zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f xs ys = fmap (\ (x, y) -> f x y) (zip xs ys)
zipWith' f xs ys = fmap (uncurry f) (zip xs ys)
```

`zipWith'` is less interesting than the other way around because it creates an intermediate list of tuples that is immediately discarded.

It is impossible to write a variant of `zipWith` for every number of lists. Fortunately, we can work around that with a special case of `zipWith`, that is, with a function `zipApply`.

### zipApply

`zipApply` takes a list of functions and a list of values and applies each function to the corresponding value by position - one function applied to one element. That is, it zips the two list with the function application, `$`.

```hs
zipApply :: [b -> c] -> [b] -> [c]
-- zipApply []     bs     = []
-- zipApply _      []     = []
-- zipApply (f:fs) (b:bs) = f b : zipApply fs bs
zipApply = zipWith ($)

_ = zipApply [(+10),succ] [1,2] -- [11,3]
```

`zipApply` can be defined as `zipWith ($)`and, more importantly, `zipWith` may be defined in terms of `zipApply`.

```hs
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys = zipApply (map f xs) ys
```

Note: normally, `map` takes a unary function to map a list with, but if we give it a binary function `f`, the result is a list of unary functions, i.e. a list of partial applications of the functions `f` to list elements. This is *a quick way to make a list of unary functions*.

```hs
map :: (a -> b) -> [a] -> [b]
map (+1) ::        [a] -> [a]
map (+)  :: [a] -> [a -> a]

x1 = map (+ 1)   [5, 6] -- [6, 7]
fs = map (+)     [5, 6] -- [(5+), (6+)]
x2 = zipApply fs [1, 2] -- [6, 7]
```

So `map f xs` creates a list of unary function (partially applied function `f` to each element), then `zipApply` applies that function list over another list.

```hs
zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys = zipApply maps ys
  where
  maps :: [b -> c]
  maps = map f xs
  zipApply :: [b -> c] -> [b] -> [c]
  zipApply = zipWith ($)

fs = map (+) [1,2]    -- [(+1), (+2)]
x1 = zipApply fs [3,4] -- [4,6]
```

### zipWith3 as zipApply

Similarly, we can define `zipWith3` in terms of `zipApply`:

```hs
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f xs ys zs = zipApply (zipApply (map f xs) ys) zs
```

Now, first `map f xs` combines a ternary function `f` and the first list into a list of binary (partially applied) functions of `[b -> c -> d]` type. Next, `zipApply` combines that with `ys` into a list of functions, `[c -> d]`. The final `zipApply` incorporates the `zs` list and yields the final result.

```
map (+1) [1,2,3] = [2,3,4]
  1  2  3
 +1 +1 +1
= 2  3  4

map (+) [1,2,3] = [(1+) (2+) (3+)]
    1    2    3
    +    +    +
= (1+) (2+) (3+)

zipApply (map (+) [1,2,3]) [3,4,5] = [4,6,8]
 (1+) (2+) (3+)
  $    $    $
  3    4    5
= 1+3 2+4 3+5
= 4    6    8

zipApply (zipApply (map (+) [1,2,3]) [3,4,5]) [6,7,8] = [10,13,16]
    1       2       3
    +       +       +
= (1+_+_) (2+_+_) (3+_+_)
    $       $       $
    3       4       5
= (1+3+_) (2+4+_) (3+5+_)
  (4+)    (6+)    (8+)
    $       $       $
    6       7       8
=  10      13      16
```

### Further generalization

This approach works more generally: if we want to zip together `n` lists, we need 1 call to `map` and `n-1` calls to `zipApply`. We can interpret `map` as `zipWith1`.

Finally, there is also `zipWith0 :: a -> [a]`, which, given a function with zero inputs and zero lists, produces a list; it is called `repeat` and generates an infinite list.

Together, these 3 functions form a minimal interface.

```hs
repeat   :: a -> [a]
map      :: (a -> b) -> [a] -> [b]
zipApply :: [a -> b] -> [a] -> [b]
```

The `Functor` type class already generalizes map as `fmap`. We now do the same with `repeat` and `zipApply`.

## Applicative class

The `Applicative` class provides an abstraction for type ctors that support a `zipApply`-like function:

```hs
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

The type class has the `pure` and `<*>` methods, which generalize `repeat` and `zipApply`. Moreover, it is a subclass of `Functor`, which means it also has the `fmap`.

The canonical implementation of the `Applicative` class is for lists, i.e. when `f = []` so the signature of `<*>` becomes

```hs
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*>) :: Applicative f => [a -> b] -> [a] -> [b]
```

### ZipList

However, because the class allows for some leeway, a second list instance is also possible. Since a type can only have a single instance of some class, in order to make the same type an instance of an already implemented class, we need to wrap that type in a newtype. So, the data type `ZipList` will be a new name for plain list type.

```hs
-- Control.Applicative
newtype ZipList a = ZipList { getZipList :: [a] }
  deriving Functor
```

The `ZipList` data type has an Applicative (zip-based) instance:

```hs
-- Control.Applicative
instance Applicative ZipList where
  pure :: a -> ZipList a
  pure a = ZipList (repeat a)

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  ZipList fs <*> ZipList xs = ZipList (zipApply xs ys)
```

Before we see a range of more interesting instances, we review the laws that applicative functors must satisfy.


## The applicative laws

The `Applicative` class comes with 4 laws. These laws capture the analogy with ordinary function application, `$`, between a function of `a -> b` type and arg of `a` type. The applicative counterpart is the `<*>` operator for application, and the `f (a -> b)` function, and the `f a` arg live inside the type ctor, `f`. The name applicative functor comes precisely from this analogy.

```hs
 ($)  ::   (a -> b) ->   a ->   b
(<*>) :: f (a -> b) -> f a -> f b
```

### The identity law

**The identity law** lifts the definition of the identity function, `id`, to the law at the level of applicative functors. It states that `pure id` is the left identity of the `<*>` operator:

>pure id <*> v = v

This explains the choice of `repeat` for `pure` in the case of `ZipList`: when zipping an infinite list with another list, the infinite list always has enough elements to line up with the other list, and hence the result has the same shape as the other list.

### The composition law

**The composition law** lifts the composition to the applicative level:

>pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

### The homomorphism law

**The homomorphism law** states that `pure` relates ordinary function application and the Applicative operator, `<*>`:

>pure f <*> pure x = pure (f x)

We can either perform a pure function application and then wrap the result in an applicative functor, or we can first wrap the function and its input and then apply (<*>).

### The interchange law

**The interchange law** states that we can swap the position of `pure` in a (<*>) composition:

>u <*> pure y = pure ($ y) <*> u

It generalizes a similar property of pure functions:

    f y = ($ y) f

Here, `($ y)` is the right section of the ($) operator.

### Relation between fmap and pure

One more relevant property concerns the relation between `fmap` and `pure`:

>pure f <*> u = fmap f u

Recall that `<$>` is the operator synonym of `fmap`, which somewhat resembles (<*>). It is often used to write the following:

>pure f <*> u = f <$> u

While these laws characterize the function application-like nature of applicative functors, they do not give us much of a sense of their practical use. We explore this next while we review a range of different instances.

## Applicative functors and effects

While we used `ZipList` and the family of `zip` functions as our initial intuition for the notion of applicative functors, there is a different notion that is helpful to understand most Applicative instances - the notion of *computational effects*.

The idea is that, whereas `a` denotes a readily available value, `f a` denotes a computation (or context) that will yield a value of `a`.

Depending on the type of computation, `f`, other relevant effects may happen during the computation. For example, the computation may fail for some reason, and thus not produce a value at all. We explore this and several other possible effects through different Applicative instances.

### Fallability with Maybe

Failing computations may or may not yield a result. We already know how to model these using the `Maybe` type, where `Just x` denotes a successful outcome with a result `x`, and `Nothing` denotes a failure. The `Maybe` type is an instance of `Applicative`:

```hs
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure a = Just a

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  _ <*> Nothing = Nothing
  Nothing <*> _ = Nothing
  Just f <*> Just x = Just (f x)
```

With `pure`, we lift a readily available value, x, into a possibly failing computation. `<*>` fails if either the function or its parameter fails, and only succeeds when both function and parameter can be produced.

### Fallability with Either


## Implicitly parameterized computations with functions

In the previous examples (Maybe, Either), the list of functions and values was given, but we can also consider the approach where we can supply different functions and values for different runs of the program.

Let's define an environment that holds the two:

```hs
data Env = Env { fun :: Float -> Float, param :: Float }
-- fun   :: Env -> (Float -> Float)
-- param :: Env -> Float
```

This gives us two projection functions, one to select a function and the other to select a parameter for that functor, both selected from an environment.

```hs
data Environ a b = Environ { runEnviron (a -> b, a) }
```


Both projections may be considered as computations that rely on an environment to produce a result. These form an `Applicative` instance for any type of environment:

```hs
instance Applicative ((->) env) where
  pure x    = \ env -> x
  pf <*> px = \ env -> (pf env) (px env)
```

This allows us to write the following:

```hs
prog :: Env -> Float
prog = fun <*> param
```

On different runs, we can then supply different values to vary the behavior:

```hs
_ = prog (Env { fun = (+1), param = 0 }) -- 1.0
_ = prog (Env { fun = (*2), param = 3 }) -- 6.0
```

## Analyzing computations with Const functor

We can analyze a computation without actually executing it, and just calculate some metrics about it. This involves using the Const functor:

```hs
-- Data.Functor.Const
newtype Const m a = Const { getConst :: m }
```

The Const functor is an edge case of a functor: `Const m a` does not actually contain or otherwise involve any value of the `a` type. We can see this in the following Functor instance:

```hs
instance Functor (Const m) where
  fmap f (Const m) = Const m
```

Here, the mapped function, `f`, goes unused because there is nothing to apply it to. The purpose of this constant functor is the actual payload of the `m` type. In the `Applicative` instance, we expect this type, `m`, to be a monoid:

```hs
instance Monoid m => Applicative (Const m) where
  pure x = Const mempty
  Const m1 <*> Const m2 = Const (m1 <> m2)
```

Because `Const m a` does not contain any `a`, `pure x` does not store `x`. Likewise, (<*>) does not apply any function to any value, because neither is available. Instead, `pure` allocates the `mempty` value and (<*>) `mconcat`s those values.

### Example

As a small application, consider the well-known Fibonacci function:

```hs
fib :: Integer -> Integer
fib n | n < 2     = 1
      | otherwise = fib (n - 1) + fib (n - 2)
```

Suppose we wish to analyze how many recursive calls happen without actually computing the result.

We can count with the `Sum Integer` monoid; the `Const (Sum Integer)` applicative functor only takes care of tracking that count and not computing the Fibonacci numbers. Using that, we can introduce a primitive operation `tick` that records a single call:

```hs
tick :: Const (Sum Integer) ()
tick = Const (Sum 1)
```

As the result value is irrelevant here, we use `()` for result. The actual purpose of `tick` is its side effect, i.e. counting by 1. We introduce the `tick` in the definition of `fib`:

```hs
fib :: Integer -> Integer
fib n | n < 2     =              1
      | otherwise =                       fib (n - 1)  +  fib (n - 2)

fib :: Integer -> Const (Sum Integer) Integer
fib n | n < 2     = tick *> pure 1
      | otherwise = tick *> (pure (+) <*> fib (n - 1) <*> fib (n -2))
```

We use the `tick *> p` combination because we only want the side effect of tick and then leave it up to `p` to produce a result. The `*>` is a predefined operator derived from (<*>) that discards the result of its left operand. It has a dual operator, `<*`, that discards the result of its second operand.

```hs
(*>) :: Applicative f => f a -> f b -> f b
px *> py = pure (\x y -> y) <*> px <*> py
```

This tells us that computing the 5th Fibonacci number takes 15 calls to `fib`:

```hs
_ = fib 5
Const {getConst = Sum {getSum = 15}}
```

## Logging computations with tuples

If we want to produce a metric, or a log of some sort, alongside the actual computation, we can use a variant on the `Const` functor, namely a pair:

```hs
instance Monoid m => Applicative ((,) m) where
  pure x = (mempty, x)
  (m1, f) <*> (m2, x) = (m1 <> m2, f x)
```

Here, a computation consists of a pair whose first component is the log, `m`, which is a value of some monoid (because it must be concatenable), and the second component is the result of the computation.

The first component of the tuple behaves like the constant functor and the second component behaves like a pure computation.

In the case of the `String` monoid (i.e. the list monoid), the log is conventional and accumulates text messages. In the case of the `Sum Integer` monoid, as we saw previously, it is use for counting. This requires a slightly modified definition of the `tick` operation:

```hs
tick :: (Sum Integer, ())
tick = (Sum 1, ())
```

It now produces a `()` value alongside the count. The only thing we need to change in our applicative definition of `fib` is its type:

```hs
fib :: Integer -> (Sum Integer, Integer)
fib n | n < 2     = tick *> pure 1
      | otherwise = tick *> (pure (+) <*> fib (n - 1) <*> fib (n - 2))
```

The modified type conveys that the result is computed alongside the count:

```hs
_ = fib 5
(Sum {getSum = 15},8)
```

Now, we can see that the 5th Fibonacci number is 8 and, as before, that it took 15 calls for fib to compute this.

With a custom monoid, we can get more detailed statistics. This custom monoid is based on the `Map` data structure:

```hs
newtype MonoidMap k v = MM { runMM :: Map k v } deriving Show
```

We define the combination of two maps as their union:
```hs
instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  MM m1 <> MM m2 = MM (unionWith (<>) m1 m2)
```

We require that the values in the map are elements of a semigroup. This way, when the two maps have a common key, the two values for that common key can be combined. Moreover, the empty map is the neutral element of the union.

```hs
instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
  mempty = MM empty
```

We can modify the definition of tick to record with which parameter, n, fib is called with this:

```hs
tick :: Integer -> (MonoidMap Integer (Sum Integer), ())
tick n = (MM (singleton n (Sum 1)),())
```

Now, fib can declare that its parameter is `n` with `tick n`:

```hs
fib :: Integer -> (MonoidMap Integer (Sum Integer), Integer)
fib n | n < 2     = tick n *> pure 1
      | otherwise = tick n *> (pure (+) <*> fib (n - 1) <*> fib (n - 2))
```

This way, we can see how often fib is called recursively for different parameter values:

```hs
_ = fib 5
(MM {runMM = fromList [(0,Sum {getSum = 3}),(1,Sum {getSum = 5}),(2,Sum {getSum = 3}),(3,Sum {getSum = 2}),(4,Sum {getSum = 1}),(5,Sum {getSum = 1})]},8)
```

As we can see, there are a lot of repeated calls.

## Stateful computations with State

## Nondeterministic computations with lists

## No effect

## Compositions

Now that we have seen an assortment of different types of effectful computation, we turn to the question of being able to use multiple effects in the same program.

This is possible by coming up with a custom representation and writing a new `Applicative` instance for it. However, much more conveniently, we can combine existing instances using either a product or a composition.

### The product of applicative functors

The product of two applicative functors, `f` and `g`, runs two computations, using `f` and `g` in parallel:

```hs
-- Data.Functor.Product
data Product f g a = Pair (f a) (g a)
```

This product of two computations is a functor when its two first parameters are functors:

```hs
instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Pair x y) = Pair (fmap f x) (fmap f y)
```

In the same way, the Applicative instance defines its methods component-wise:

```hs
instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = Pair (pure x) (pure x)
  Pair pf qf <*> Pair px qx = Pair (pf <*> px) (qf <*> qx)
```

The definition of (<*>) shows that the two parallel computations happen in lockstep. However, data isn't exchanged between them; they are independent and each produces its own result.

We have already seen an example of this product construction, namely the `(m,)` tuple applicative functor. It is essentially a specialized definition of `Product (Const m) Identity`. While such a specialized definition does not reuse the existing instance code, it avoids the nested constructors. This makes it a bit more convenient to use and slightly more efficient.

### The composition of applicative functors

While a bit harder to wrap your head around, the composition of two applicative functors is often more useful than their product. It allows effects to interact and contribute toward a common result.

By looking at the composition of two functors, we can understand their nesting:

```hs
-- Data.Functor.Compose
newtype Compose f g a = Compose { getCompose :: f (g a) }
```

Its Functor instance maps through the two layers:

```hs
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose t) = Compose (fmap (fmap f) t)
```

The Applicative instance handles the two layers similarly:

```hs
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))

  Compose pqf <*> Compose pqx
    = Compose ((\qf qx -> qf <*> qx) <$> pqf <*> pqx)
```

The pure method stacks two layers of pure computation, and the (<*>) operator uses a zipWith strategy on the outer layers that zips the corresponding inner layers. Using this composition, we can define a variant of the Fibonacci function that involves both memoization and logging:

```hs
type Memoization = State (Map Integer Integer)
type Logging = (,) (MonoidMap Integer (Sum Integer))
type Effect = Compose Memoization Logging
```

The new definition that features the two effects is now essentially a merge of the two earlier definitions with their single effect:

```hs
fib :: Integer -> Effect Integer
fib n = Compose (State (\ s ->
  case Map.lookup n s of
    Just f  -> (pure f, s)
    Nothing ->
      let ((m,f), s') = runState (getCompose (fib' n)) s
      in  ((m,f), insert n f s')))
  where
  fib' n | n < 2     = lift (tick n) *> pure 1
         | otherwise = lift (tick n) *> ((+) <$> fib (n-1) <*> fib (n-2))
  lift q = Compose (pure q)
```

The main addition to the earlier code is that we have to acknowledge the presence of the other code. This means applying the lift function to tick, to wrap it in the composition. Similarly, the memoization logic now acknowledges the presence of the log m.

Let's run this new Fibonacci function with a memoization table that is initially empty:

```hs
*Main> runState (getCompose (fib 10)) emtpy

((MM {
  ruMM = fromList
    [ ( 0, Sum {getSum = 1})
    , ( 1, Sum {getSum = 1})
    , ( 2, Sum {getSum = 1})
    , ( 3, Sum {getSum = 1})
    , ( 4, Sum {getSum = 1})
    , ( 5, Sum {getSum = 1})
    , ( 6, Sum {getSum = 1})
    , ( 7, Sum {getSum = 1})
    , ( 8, Sum {getSum = 1})
    , ( 9, Sum {getSum = 1})
    , (10, Sum {getSum = 1})
    ]
  }, 89),
  fromList
    [ (0,1)
    , (1,1)
    , (2,2)
    , (3,3)
    , (4,5)
    , (5,8)
    , (6,13)
    , (7,21)
    , (8,34)
    , (9,55)
    , (10,89)
  ]
)
```

The result is a nested tuple of the form `((callstats, result), memotable)`:
- The call statistics are a table that associates a call count of 1 with each number in the range of 0 to 10
- The result is 89
- The memoization table associates each Fibonacci number with each number in the range of 0 to 10
- Hence, we can confirm that memoization does its job and computes each Fibonacci number only once.
