# Reader

The function type, i.e. the most general function type, (a -> b), is a type named `(->)`, which is also its type ctor, and two type params `a` and `b`, where `a` repr the input type and `b` the output type.

This type could have benn defined as:

```hs
-- this form enables partially applied input type (r ->) ≅ ((->) r)
data (->) r a = (->) r a
-- this form enables partially applied output type (-> r)
data (->) a r = (->) r a
```

Since we can't define it like that (cannot hide function ctor definition), we can define it by wrapping it in the newtype. This newtype receives its type params as `r` then `a`, which is a form that enables us to have a partially applied type ctor as `(r ->)` ≅ `((->) r)`.

The `r` type (input) is fixed, while the the `a` type (output) varies.

```hs
newtype Reader r a = Reader { runReader :: (->) r a }
```

* The function type `r -> a` or `((->) r a)` can implement various classes. The kind of function type ctor is `(->) :: * -> * -> *`. However, we can fix the type param `r`, that repr the function's input type, by partially applying it to the type ctor, so its kind becomes `((->) r) :: * -> *`. This is the exact kind we need to implement various classes of the form `class C (f :: * -> *)` such as Functor, Applicative and Monad.

* The `Reader r a` newtype wrapper can also be used similarly, i.e. by partially applying its type ctor to the TP `r` (repr input type), as `Reader r`, which can be the receiver type for class implementations.

* These two are isomorphic: `Reader r a` ≅ `(->) r a`   
  therefore, apart from unwrapping and rewrapping the data ctor, 
  they'll have practically the same instance implementations.



## Functor

```hs
instance forall r. Functor
  ((->) @{'GHC.Types.LiftedRep} @{'GHC.Types.LiftedRep} r) where

instance Functor ((->) r) where
  fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap g f x = g $ f x
  fmap g f = g . f
  fmap = (.)
  -- the B combinator

instance forall r. Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap g (Reader f) = Reader $ \x -> g $ f x
  fmap g (Reader f) = Reader $ g . f
```

Mapping a function `g` over a function `f` amounts to the composition `g . f` (g after f). This is the B (bluebird) combinator `λgfx.g(fx)`.


## Applicative

Functions are also applicative functors and they allow us to operate on the eventual results of functions as if we already had their results.

```hs
instance Applicative ((->) r) where
  pure :: a -> r -> a
  pure a r = a
  pure = const

  (<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
  g <*> f x = g x $ f x
  -- the S combinator

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader g <*> Reader f = Reader $ \x -> g x $ f x
```

The `pure` amounts to the `const` function i.e. the *K combinator*.    
The `<*>` is the *S combinator*


## Monad

Not only is the function type `(->) r` a functor and an applicative functor, but it's also a monad. Just like other monadic values, a function can also be considered a value with a context. The context for functions is that that value is not present yet and that we have to apply that function to something in order to get its result value.

```hs
instance Monad ((->) r) where
  return :: a -> r -> a
  return = const

  (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
  f >>= g = \x -> g (f x) x
  -- similar to S combinator with g's params flipped:
  -- S := λg f x.    g  x (f x)
  -- S':= λg f x.    g    (f x) x
  -- S':= λg f x. (C g) x (f x)


instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader $ \_ -> a

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= arb = Reader $ \r -> runReader (arb $ ra r) r
```

The `bind` operator is similar to the S combinator but instead of `λgfx.gx(fx)` the arguments of g are flipped, `λgfx.g(fx)x`.


## Parallels

The methods of all 3 classes are very similar in a way:

```
K  :=  λa b . a                    pure, return

B  := λ g f x . g   (f x)          (<$>), fmap
S  := λ g f x . g x (f x)          (<*>)
S' := λ g f x . g   (f x) x        (>>=)

S' := λ g f x . S (C g) f x        (>>=)


fmap  g f x = g   (f x)      = B    g  f
(<*>) g f x = g x (f x)      = S    g  f x
(>>=) g f x = g   (f x) x    = S (C g) f x

(>>=) g f x = S (flip g) x (f x)
```

Some examples:

```hs
f1 = ( (2*) <$> (+5) ) 2   -- 14      λ g f x . g   (f x)
a1 = ( ( *) <*> (+5) ) 2   -- 14      λ g f x . g x (f x)
m2 = ( ( *) =<< (+5) ) 2   -- 14      λ g f x . g   (f x) x
m1 = ( (+5) >>= (* ) ) 2   -- 14      λ g f x . g   (f x) x


-- f1 = ((3*) . (+5)) 2 = (3*) (2+5)
-- a1 = S (*)   (+5)  2 = (2*) (2+5)
-- m1 = S (flip (+)) (*3) 2 = S (+) (*3) 2 = ((+) 2) ((*3) 2) = (2+) (2*3)


-- Lift a binary op over 2 unary ops
f = (+) <$> (*2) <*> (+10)
f 3 -- 19

-- Lifter of a binary op over 2 applicatives
lifter :: forall {f :: * -> *} {a} {b} {c}.
          Applicative f => f a -> f b -> (a -> b -> c) -> f c
lifter ux vx bx = bx <$> ux <*> vx

lift1 = lifter (+5) (*3)
liftPlus = lift1 (+)
w1 = liftPlus 3
w2 = liftPlus 4
liftTimes = lift1 (*)
w3 = liftPlus 3
w4 = liftPlus 4
```
