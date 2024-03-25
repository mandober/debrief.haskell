# Function type

`Deconstructing lambdas - An awkward guide to programming without functions`
by Chris Penner, 2021
https://www.youtube.com/watch?v=xZmPuz9m2t0

Topics
- What are functions really
- Functions considered harmful
- Rebuilding functions from the first principles
- Functions as data
- Profunctors

## Intro

Functions are foundational in FPLs as they are
- very adaptable
- provide encapsulation of code
- means of abstraction
- stackable
- composable

In fact, they are so stackable and composable that an entire program may be considered one big function. Functions are modelled on the lambda calculus, that is, lambda calculus, which is Turing complete, may be considered a formal theory of functions.

However, functions also have some disadvantages. Firstly, it is always assumed that we only want to exectute them, and in fact, that is the only thing we really can do with functions. We cannot
- analyzing them
- debug them
- show them
- compare them for equality
- serialize them
- transpile them

Functions are one of the completely opaque types in Haskell. Functions are too powerful! And one large powerful type provides less information than several small ones. Monads are similar (well, they are Kleisli arrows after all). The `IO` is a primitive monad that is completely opaque - we have no idea what an `IO ()` action might do. The only thing we can do with it is execute it. But, we can break it down in order to gain more insign into what it might do as monads can roughly express their intentions through constraints.

```hs
-- we have no idea what this monadic action does
doTheThing :: IO ()

-- but we have a few ideas what this monad does
doTheThing ::
  ( MonadLogger m
  , MonadDB m
  , MonadState User m
  ) => m ()
```

The more generic (powerful) the tool the less we can know about how it's used (what it does). And it's similar with functions. Functions have too many behaviours bundled together. We have no way to introspect them. We cannot tell whether a function loops forever or recurses, whether it is total or partial, what its space and time complexity is. All we can do with a function is to execute it.

We need to identify and abstract over each independent behaviour of functions, similarly to how a monadic action can be scrutinized. Then we could build constraints which would allow us not only to see what a function does, but maybe also to interpret it differently.

## Breaking down functions

Breaking functions down into their component parts and component constraints.

The `Arrow` class is worth mentioning as an attempt to have different behaviors that different arrows may implement.

```hs
class Category a => Arrow a where
    {-# MINIMAL arr, (first | (***)) #-}

    -- | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

    -- | Send the first component of the input through the arg
    -- arrow, and copy the rest unchanged to the output.
    first :: a b c -> a (b,d) (c,d)
    first = (*** id)

    -- | A mirror image of `first`.
    second :: a b c -> a (d,b) (d,c)
    second = (id ***)

    -- | Split the input between the two arg arrows and combine output.
    -- Note that this is in general not a functor.
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
    f *** g = first f >>> arr swap >>> first g >>> arr swap
      where swap ~(x,y) = (y,x)

    -- | Fanout: send the input to both argument arrows and combine output.
    (&&&) :: a b c -> a b c' -> a b (c,c')
    f &&& g = arr (\b -> (b,b)) >>> f *** g
```

But it is not useful in our sense because its first method `arr` only "embeds" (passes-throught) a function into an arrow - if we're not capable of analyzing a bare function, we won't have more luck analyzing functions wrapped in arrows.

This is similar to a monadic stack with the `IO` monad as the base. No matter what additional capabilities you add on top along with their constraints, the `IO` monad at the base is still free to do unspecified things, voiding any guarantee about the behavior of the whole stack.

The approach with classes is nevertheless useful. By defining classes that describe behaviours, we can choose an interpretation that implements them.

A significant property of functions is that they compose, but functions are not the only type with that property - arrows compose as well.

```hs
class Category k where
  id    :: k a a
  (.)   :: k b c -> k a b -> k a c

  -- backward composition
  (<<<) :: k b c -> k a b -> k a c
  (<<<) = (.)

  (⋘) :: k b c -> k a b -> k a c
  (⋘) = (.)

  (⥀) :: k b c -> k a b -> k a c
  (⥀) = (.)

  -- forward composition
  (>>>) :: k a b -> k b c -> k a c
  (>>>) = flip (.)

  (⋙) :: k a b -> k b c -> k a c
  (⋙) = flip (.)

  (⥁) :: k a b -> k b c -> k a c
  (⥁) = flip (.)


⋘ ⋙  ⥁ ⥀  🕃 🕄  ⚆ ⚇  ⚈ ⚉  ⧂



-- left to right composition
f ⋙ id ⋙ g
-- left to right composition
f ⥁ g
-- right to left composition
g ⥀ f
```

Categorical arrows are a generaliation of functions with two intrinsic behaviors: identity (unit) as a no-op behavior and composition.

With only this one class `Category`, we can already start to abstract over functions and do some interesting things. We can define a combinator that only uses these constraints.

```hs
thrice :: Category k => k a a -> k a a
thrice k = k ⋙ k ⋙ k
```

We use `k` here as a category, and rather than using a function from `a` to `a`,we use an arrow from `a` to `a`. We can define the combinator `thrice` that takes an endomorphism, from `a` to `a`, and runs it 3 times.

The combinator `thrice` does work with functions, but it can also work with something other than functions.

```hs
add3 :: Integer -> Integer
add3 = thrice (+ 1)

x1 :: Integer
x1 = add3 10 -- 13
```

## A category of JavaScript function

The JS category is a category of JavaScript values as objects and JS functions as arrows, but encoded as Haskell types. The Identity arrow is the identity function in JS and composition of arrows is composition of JS functions.

```hs
newtype JSFunc a b = JSFunc { renderJS :: Text }

instance Category JSFunc where
  unit :: JSFunc x x
  unit = JSFunc "(x => x)"

  -- | faux pseudo quasi fake
  (⋙) :: JSFunc x y -> JSFunc y z -> JSFunc x z
  JSFunc f ⋙ JSFunc g = JSFunc $ "(x => g(f(x)))"
  {-  [i|(arg) =>
        {
          const fst = #{renderJS f}
          const snd = #{renderJS g}
          return snd(fst(arg))
        }
      |]
  -}

times10 :: JSFunc Int Int
times10 = JSFunc "(x => x * 10)"
-- JSFunc {renderJS = "(x => x * 10)"}

times1000 :: JSFunc Int Int
times1000 = thrice times10
-- JSFunc {renderJS = "(x => g(f(x)))"}

x3 :: String
x3 = renderJS times1000
-- "(x => g(f(x)))"
{-
  should be:

  (input) =>
    {
      const fst = (x => x * 10)
      const snd = (input) =>
        {
          const fst = (x => x * 10)
          const snd = (x => x * 10)
          return snd(fst(input))
        }
      return snd(fst(input))
    }
-}
```

The two phantom types are to satisfy `Category` type class (dubiously to keep things safe), but JS functions are represented as text.

## Discarding arguments

In Haskell, functions can copy or discard their arguments at will, but in other quasi models of categories in other PLs (linear functions, C, Rust, electrical circuitry) this is not the case.

```hs
class Category k => Cartesian k where
  copy    :: k a (a, a)
  consume :: k a ()
  fst     :: k (l, r) l
  snd     :: k (l, r) r

-- pseudo
instance Cartesian JSFunc where
  copy = JSFunc "(x => ([x, x]))"
  consume = JSFunc "(x => null)"
  fst = JSFunc "(([x, _]) => x)"
  snd = JSFunc "(([_, y]) => y)"
```

## Plumbing

Functions are allowed to do many other (internal) plumbing: they can route their arguments to other internal functions. They can route their arguments into either multiple internal functions or even to internal functions that exist at different stages of execution. An example of this is the palindrome

```hs
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str
```

How would we do this without using function application?

```hs
isPalindrome :: String -> Bool
isPalindrome = (\str -> (str, str))          -- copy or split
            ⋙ (\(s, s') -> (reverse s, s')) -- first
            ⋙ (\(revS, s) -> revS == s)
```

## Strong category

So how can we encode that i have this idea of a strong category here
it's also kind of like a manoidal product category 
but strong is the name that for instance Kmett uses in his profunctors library
which has a lot of these cool classes 
that's based on the profunctor hierarchy
rather than the category hierarchy 
but it turns out they're almost interchangeable 
you just have to change the superclass 

We have two combinators `first` and `second` that expect an arrow that goes from `a` to `b` and they both take an additional argument that goes through unchanged.

```hs
class Category k => Strong k where
  first  :: k a b -> k (a, x) (b, x)
  second :: k a b -> k (x, a) (x, b)
```
