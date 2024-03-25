# Manual theading

In general, a regular function `f` is converted into a stateful function `g` that takes and returns a pair. The *input pair* is made of the function's regular input value and the state value. The *output pair* is made of the function's regular output value and the possibly modified state value.

```hs
f :: a -> b
g :: (a, s) -> (b, s)

g' :: (s, a) -> (b, s)
g'' :: a -> s -> (b, s)
```

Swapping a pair's components and using currying, we actually have several compatible signatures that type the stateful function.

We thread the state through functions using a pair: *the input pair* is made from the function's regular input value and the state, and *the output pair* consists of the function's regular output value and the (possibly updated) state.

```hs
f  :: a -> b
f' :: (a, s) -> (b, s)
```

Importantly, the type of the state is kept the same, in both input and output (above, it is denoted by a polymorphic type `s`), but the value of the state may change.

A function takes a state (and its regularly scheduled input) and returns the updated state (and its regularly scheduled output). The next function in the *pipeline* - **whether it needs access to the state or not** - must be modified to accept and return a pair. Obviously,

>Threading the state screws up the composition of functions.

Functions that were easily composed before cannot be composed after. We'd very much like to have our cake and eat it too - *we want to thread the state through functions, but also be able to compose such functions*. Classic, solving one problem gave rise to another.



## Before the state

Before we start implementing the state as a global counter that tracks the number of function calls, we have two unremarkable functions, `revOld` and `sortOld`, composable as `sortRevOld = revOld . sortOld`.

```hs
-- | reverse a list
rev :: [a] -> [a]
rev xs = reverse xs

-- | sort a list
srt :: Ord a => [a] -> [a]
srt xs = sort xs

-- | sort then reverse a list
sortrev :: Ord a => [a] -> [a]
sortrev = rev . srt

x1,x2,x3 :: [Integer]
x1 = srt     [1,4,5,3,2] -- [1,2,3,4,5]
x2 = rev     [1,4,5,3,2] -- [2,3,5,4,1]
x3 = sortrev [1,4,5,3,2] -- [5,4,3,2,1]
```

## After the state

Now we model the state by adding an extra parameter to all functions that need to manipulate it. Such functions will also need to return an extra value. As mentioned, we'll encode this requirements using a pair.

In general, a regular function `f` is converted into a stateful function `g` that takes and returns a pair. The *input pair* is made of the function's regular input value and the state value. The *output pair* is made of the function's regular output value and the possibly modified state value.

```hs
f :: a -> b
g :: (a, s) -> (b, s)

g' :: (s, a) -> (b, s)
g'' :: a -> s -> (b, s)
```

Swapping a pair's components and using currying, we actually have several compatible signatures that type the stateful function.

## Back to our example

We alter the functions so that each takes and returns a pair (actually we use currying, so each function takes a pair of args instead of a literal pair).

```hs
-- | revS (in terms of rev) and increment the counter
revS :: Int -> [a] -> (Int, [a])
revS c xs = (c + 1, rev xs)

-- | srtS (in terms of srt) and increment the counter
srtS :: Ord a => Int -> [a] -> (Int, [a])
srtS c xs = (c + 1, srt xs)
```

However, this change kills the composition of these two functions. We'll also work to restore the composition, but right now we want to isolate the "core state type", so to say.

If we have an n-ary function we can convert it into a stateful function that takes a pair, the first component of which is the n-tuple of function's origianl params, and the second component is the state param. Due to the equivalence of swapping the components of a tuple, and the equivalence due to cueeying, the first `n` parameters of the stateful function can be its original parameters, and the last param we'll use for state.


Consider again the function `reverseWithCount` which we have already made stateful (and called `revS` above). And consider another function `appendReversedWithCount` defined in terms of the `reverseWithCount` function. This latter function takes two lists, calls `reverseWithCount` to reverse each list (and increment the counter), then it returns the result of appending the two reversed list, along with the incremented counter.

```hs
-- Reverse a list, and increase the counter
reverseWithCount :: [a] -> Int -> ([a], Int)
reverseWithCount xs c = (reverse xs, c + 1)

-- Reverse and append lists, and increase the counter
appendReversedWithCount :: [a] -> [a] -> Int -> ([a], Int)
appendReversedWithCount xs ys c0 =
  let (xs', c1) = reverseWithCount xs c0
      (ys', c2) = reverseWithCount ys c1
  in  (xs' ++ ys', c2 + 1)

x4 :: ([Integer], Int)
x4 = appendReversedWithCount [1,4,5,3,2] [9,6,8,5,7] 0
-- ([2,3,5,4,1,7,5,8,6,9], 3)
```

The definition of the function `appendReversedWithCount` above clrealy shows how the state gets threaded through functions. To underline this point, consider the following function with takes 3 lists:

```hs
append3ReversedWithCount :: [a] -> [a] -> [a] -> Int -> ([a], Int)
append3ReversedWithCount xs ys zs c0 =
  let (xs', c1) = reverseWithCount xs c0
      (ys', c2) = reverseWithCount ys c1
      (zs', c3) = reverseWithCount zs c2
  in (xs' ++ ys' ++ zs', c3 + 1)
```

We see that we can push the state param to be the last input param. In the pair the functions return, state may be the first or second component, it really doesn't matter much, but it pays to keep things consistent across functions.

The important thing is tha the functions lower in the call stack are able to make updates to the count, and those updates are seen by the functions higher in the stack.

Another thing that is different with stateful functions is that we need to call the first function (in a pipeline) with an *initial state value*. In the case of the counter, that would be `0 :: Int` for the current count.

If a function needs to use the current count as a branching factor, it can since it is just a regular function parameter like others. Also, this solution is scalable - we can make the state hold more data by using a data structure to represent it.

## The consequences

However, this is clearly not very comfortable to use. We had to mangle the definitions of all the functions, adding a lot of boilerplate in order to thread the state through. Manually threading the state increases the possibility of errors since it is very easy to reference a wrong state variable - they all have the same type so the compiler cannot assist us.

A bigger problem is that the added state means that we are now responsible for calling stateful functions with correct args and initial state value; it is our responsibility to thread the updated state correctly, and manage the return value, instead of having all that bookkeeping done automatically. Not to mentioned that we cannot anymore compose these stateful functions like we used to do.

Monads help us by managing some kind of extra "context". In this case, surely the current state is that extra context, so we should be able to use monads to improve our current situation.

>However, in order to use monadic services, we need to come up with a type that best represents the state, so we can make that type an instance of various useful type classes.

From the signatures of the prior functions, as already said, we are trying to extract the core type - a type that best represents the augmentation of a state to a function.

```hs
f  :: a -> b
g' :: (a, s) -> (b, s)
g  :: a -> s -> (s, b)
```

The last signature has the origianal (one or more params) first, here only one, `a`, and the state param `s` last. And it returns a pair made of state and its original output value, which we denote as `(s, b)`.

We are trying to extract the state type as the last type in a function signature. Since a function may have any number of input params originally, we will not include the input params in the state core type - they will be added before our state type as needed.

For example, the function `f` below originally has 3 input params. The signature of its stateful version, `g :: a -> b -> c -> (s -> (s, c))`, can be interpreted as taking the 3 original params (`a`, `b` and `c`) and returning a function of type `s -> (s, c)`, which takes the state arg `s` and returns the pair `(s, c)`.

```hs
-- generic function signatures

-- original function
f :: a -> b
-- converted into a stateful function
g :: (a, s) -> (b, s)
g :: a -> s -> (b, s)
g :: a -> (s -> (b, s))

f1 :: a -> a
g1 :: (a, s) -> (a, s)
g1 :: a -> (s -> (a, s))

f2 :: a -> a -> a
g2 :: (a, a, s) -> (a, s)
g2 :: a -> a -> s -> (a, s)
g2 :: a -> a -> (s -> (a, s))

f3 :: a -> b -> c -> c
g3 :: (a, b, c, s) -> (c, s)
g3 :: a -> b -> c -> s -> (c, s)
g3 :: a -> b -> c -> (s -> (c, s))
```

## Stateful core type

Focusing on the end of these signatures we see the prime candidate for a type to represent a state: `s -> (a, s)`.

```hs
type Stateful a s = s -> (a, s)
```

We can express the state type as a type alias (to admire it for a while), but type aliases cannot be targets (receivers) of type classes. Nevertheless, we can use it in type signatures to check that we got everything right. For example, we can adjust the last part of the signature of the function `reverseWithCount` to use our type alias:

```hs
type Stateful a s = s -> (a, s)

reverseWithCount  :: [a] -> Int -> ([a], Int)
reverseWithCount' :: [a] -> Stateful [a] Int
```

If the **existing definition of `reverseWithCount` works unchanged with the new signature, then everything is all right**! And it does:

```hs
type Stateful a s = s -> (a, s)

reverseWithCount :: [a] -> Int -> ([a], Int)
reverseWithCount xs c = (reverse xs, c + 1)

reverseWithCount' :: [a] -> Stateful [a] Int
reverseWithCount' xs c = (reverse xs, c + 1)
```

## The state newtype

In the standard library, in the package `transformers`, the state is actually defined as the following newtype:

```hs
newtype State s a = State (s -> (a, s))
```

Actually, the definition also hosts the accessor function `runState`:

```hs
newtype State s a = State { runState :: s -> (a, s) }

-- the type of the `State` data ctor is
State :: forall s a. (s -> (a, s)) -> State s a
-- and the type of the `runState` accessor is
runState :: forall s a. State s a -> s -> (a, s)
```

The data constructor `State` (itself being a function like all data ctors) takes a farg - which takes a state `s` and returns a pair `(a, s)` - and returns a `State s a`. In other words, it slaps the tag `State` onto a function of the type `s -> (a, s)`, thereby promoting it to a State ADT.

The accessor function `runState` does the opposite of the data ctor. It takes a state data type `State s a` and demotes it into a plain function of type `s -> (a, s)`, by stripping the `State` tag from it. The same can be achieved with pattern matching so we now have a choice between the two.


## StateT monad transformer

Note: truth be told, the `State` type is actually defined as a type alias in terms of the state monad transformer, `StateT`, using the identity functor in place of a monad. Monad transformers are data types that allow us to roll two monads into one that incorporates the behaviors of both.

### Quiz: StateT definition

Taking into account the fact that a monad transformer combines the behaviors of two monads into one producing a monad that incorporates the behaviors of both: 
>What is the correct placement for the "other" monad `m` on the right-hand side of the equation for `StateT`? Why?

```hs
newtype StateT m s a = StateT (m (s ->   (a,   s)) ) -- (A)
newtype StateT m s a = StateT (m  s ->   (a,   s)  ) -- (B)
newtype StateT m s a = StateT (   s -> (m a,   s)  ) -- (C)
newtype StateT m s a = StateT (   s ->   (a, m s)  ) -- (D)
newtype StateT m s a = StateT (m  s ->   (a, m s)  ) -- (E)
newtype StateT m s a = StateT (   s -> m (a,   s)  ) -- (F)
```


Spoiler ahead!




### Monad transformers

Monad transformers are data types that allow us to roll two monads into one that incorporates the behaviors of both.

```hs
newtype StateT m s a = StateT (s -> m (a, s))

-- really, as type alias
type State s a = StateT Identity s a
-- really, with the point `a` cut short
type State s = StateT Identity s
```

Monad transformers are data types that allow us to roll two monads into one that incorporates the behaviors of both.

The monad `State` and the monad transformer `StateT`, with the former given as a standalone definition in order to compare them:

```hs
type State  s   a = State  { runState  :: a ->   (a, s) }
type StateT s m a = StateT { runStateT :: a -> m (a, s) }
```

This shows that the monad `m` will plug itself in the most awkward of places.

For example, check the `State + Maybe` monad transformer:

```hs
-- State + Maybe transformer: should it be like this
newtype StateMaybe s a = StateMaybe (s -> Maybe (a, s))
-- State + Maybe transformer: or like this?
newtype StateMaybe s a = StateMaybe (s -> (Maybe a, s))
```

## The reason why

Despite all that, here, we'll work with a `State` data type that we define as a standalone newtype:

```hs
newtype State s a = State (s -> (s, a))
-- NOTE: the param `a` is the second component of the pair.
-- We'll try to stick with this form (althout they are all equivalent)
-- Curiously, the StateT has the order of components swapped!
newtype StateT m s a = StateT (s -> m (a, s))
```

NOTE: the param `a` is the second component of the pair in the `State` data type, i.e. as `(s, a)`. We'll try to stick with this form although *all these forms are equivalent up to isomorphism*. Curiously, the `StateT` has the components swapped, i.e. it has `(a, s)` instead (!).



Before we continue, we need to find the answer to the reason why:
>Why would doing all these transformations to end up with functions that use the `State` type even help us toward the simplification of syntax and the restoration of composition?

Well, monads give us two functions: `return`, used to lift a regular value into the monadic context, and `>>=` (bind) that acts as the composition operator.

## Digression about composition

The true monadic composition is achieved with the Kleisli's "fish" operator, `>=>`, and 'bind' is actually a partially applied "fish", only easier to use.

Actually, the fish operator, `>=>`, is left-to-right composition, which the operator `>>=` corresponds to. The reverse fish `<=<` is right-to-left composition, which the operator `=<<` corresponds to.

- `.`   regular function composition, `(g . f) x`
- `&`  function composition flipped,  `(f & g) x`
- `>=>` monadic  Kleisli L2R composition, `g >=> f`
- `<=<` monadic  Kleisli R2L composition, `f <=< g`
- `>>=` monadic  bind is partially applied `>=>`
- `=<<` monadic  flipped bind is reversed `>>=`
- `=>=` comonadic Kleisli L2R composition
- `=<=` comonadic Kleisli R2L composition
- `=>>` comonadic correspondence to the 'bind'
- `<<=` comonadic correspondence to the flipped 'bind'


```hs
($)      :: (a -> b) -> a -> b
(&)      :: a -> (a -> b) -> b -- flip ($)

(.) :: (b -> c) -> (a -> b) -> a -> c
(⦂) :: (a -> b) -> (b -> c) -> a -> c -- flip (.)

return :: Monad m => a -> m a

-- g >=> f               g             f
(>=>)  :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c) -- g >=> f
-- f >=> g               f             g
(<=<)  :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c) -- f >=> g

(>>=)  :: Monad m => m a -> (a -> m b) -> m b
(=<<)  :: Monad m => (a -> m b) -> m a -> m b

(>=>) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
g >=> f = \a -> 

-- (>=>) Kleisli composition L2R
-- (<=<) Kleisli composition R2L

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
amb >=> bmc = \a ->
  let mb = amb a
  in  mb >>= \b -> bmc b

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)


```

## The reason why continued

So, monads give us two functions: `return`, used to lift a regular value into the monadic context, and `>>=` (bind) that acts as the composition operator.

```hs
return :: Monad m => a -> m a
(>>=)  :: Monad m => m a -> (a -> m b) -> m b
```

>How do the types of these two functions relate to our actual `State` datatype?

The data type `State s a` is functorial in the second argument, so the type ctor `State s` will be made the reciver of the FAM type classes.

The type ctor `State s` coresponds to the `s -> (,) s` or `s -> (s,)` part of the State data type definition. The same type `State s` is then also the `m` type (monad) in the bind's signature.

```hs
newtype State s a = State (s -> (s, a))

m ≈ State s ≈ s -> (,) s

(>>=) :: m a -> (a -> m b) -> m b

-- m ≈ State s
(>>=) :: State s a -> (a -> State s b) -> State s b

-- m ≈ s -> (,) s
(>>=) :: s -> (,) s a -> (a -> s -> (,) s b) -> s -> (,) s b
(>>=) :: (s -> (s, a)) -> (a -> (s -> (s, b))) -> (s -> (s, b))
(>>=) :: (s -> (s, a)) -> (a -> s -> (s, b)) -> s -> (s, b)
```

Considering the type of bind and the type of `State`, it appears as if the state param `s`, in `s -> (s, a)`, never gets passed to the use site, i.e. the second arg to bind, the farg `(a -> m b)` does not mention state `s` - we have to assume the `a` here represents the function's original return value; this farg takes that value as an arg and returns a value `b` (we see `b` here because the value of type `a` may bhave been converted into another type) wrapped in a monadic context, `m b`.

```hs
newtype State s a = State (s -> (s, a))
--                                ⟋
--                              ⟋
--                            ↙
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

>If we use `>>=` to construct our functions, the current state will be completely abstracted away underneath the `m` type parameter.

From one perspective, that sounds contraprodactive to our efforts - we are trying to provide a way of updating the state (and this, seemingly, does not even mention the state).

But from another perspective, this is actually exactly what we want! We want to get rid of the boilerplate arising from threading the state; the only thing we can access is functions' original return values (before all this stateful nonsense). Whatever the bind method amounts to, it should handle the threading of the state for us, freeing us from having to worry about it.

As an example, let's assume that we already have correctly implemented the FAM instances for the State datatype.
>What might the definitions for our functions from before look like, using the monadic functions?

```hs
newtype State s a = State { runState :: s -> (a, s) }

-- before
reverseWithCount :: [a] -> Int -> ([a], Int)
reverseWithCount xs c = (reverse xs, c + 1)
-- after
reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = State (\s -> (s + 1, reverse list))

-- before
appendReversedWithCount :: [a] -> [a] -> Int -> ([a], Int)
appendReversedWithCount xs ys c0 =
  let (xs', c1) = reverseWithCount xs c0
      (ys', c2) = reverseWithCount ys c1
  in  (xs' ++ ys', c2 + 1)
-- after
appendReversedWithCount :: [a] -> [a] -> State Int [a]
appendReversedWithCount list1 list2 =
  reverseWithCount list1 >>= (\revList1 ->
    reverseWithCount list2 >>= (\revList2 ->
      State (\s -> (s + 1, revList1 ++ revList2))))

-- before
append3ReversedWithCount :: [a] -> [a] -> [a] -> Int -> ([a], Int)
append3ReversedWithCount xs ys zs c0 =
  let (xs', c1) = reverseWithCount xs c0
      (ys', c2) = reverseWithCount ys c1
      (zs', c3) = reverseWithCount zs c2
  in (xs' ++ ys' ++ zs', c3 + 1)
-- after
append3ReversedWithCount :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount list1 list2 list3 =
  reverseWithCount list1 >>= (\revList1 ->
    reverseWithCount list2 >>= (\revList2 ->
      reverseWithCount list3 >>= (\revList3 ->
        State (\s -> (s + 1, revList1 ++ revList2 ++ revList3)))))
```

Actually, better if we use this definitions:

```hs
newtype State s a = State { runState :: s -> (s, a) }

reverseWithCountM :: [a] -> State Int [a]
reverseWithCountM list = State (\s -> (s + 1, reverse list))

appendReversedWithCountM :: [a] -> [a] -> State Int [a]
appendReversedWithCountM xs ys =
  reverseWithCountM xs >>= (\xs' ->
    reverseWithCountM ys >>= (\ys' ->
      State \s -> (s + 1, xs' ++ ys')))
  where
    State f >>= k = State \s ->
      let (s', a) = f s
      in  runState (k a) s'
```

It is not the prettiest code, and later we'll see how this gets improved, but even at this stage, notice how almost all of the boilerplate related to passing the state around has gone.

We are left with just the logic of reversing and appending lists; the only place where we have to explicitly manage the state is when we update it.

Instead of us managing the updates from within the functions we call, it looks as though `>>=` is doing that for us now. So whatever implementation we write for the bind (e.g. bind in terms of the list monad, in terms of the Maybe monad, etc.), that is where we'll move the state handling logic (which was before done manually).

## FAM instances of the State data type

The point of `>>=`, and similarly, of `<*>`, is to do the exact same threading of the current state that we have previously done manually.

## Implementing the abstractions

```hs
newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (fmap f . g)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State \s -> (s, a)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State g = State \s ->
    let (s' , h  ) = f s
    in  fmap h (g s')

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  State f >>= k = State \s ->
    let (s', a) = f s
    in  runState (k a) s'
```

There are shorter ways to write these instances using arrow combinators or composing fmaps, but these are probably the most easily-understood definitions.

The most important thing to notice is the definitions of (<*>) and (>>=): notice how we call the functions contained within both State values given to us, and do the same parameter threading that we were doing manually before.


## Using the abstractions

Because we are now using monadic binds to define the functions, managing the state is abstracted away from us.

>How do we introspect or modify the state now?

We could directly use the `State` data constructor to do that, but having to break open the internals of the abstraction, just to do something as simple as updating the state, is wrong. Instead, we can define a few "primitives" that will provide the basic functionality specific to a certain monad.

These primitive operations are called `get`, `put` and `modify`.

```hs
-- Get the current state
get :: State s s
get = State \s -> (s, s)

-- Replace the current state with the given state
put :: s -> State s ()
put s = State \_ -> (s, ())

-- Use the given function to update the state
modify :: (s -> s) -> State s ()
modify f = State \s -> (f s, ())       -- (1)
-- or
modify f = get >>= put . f             -- (2)
-- or
modify f = do                          -- (3)
  s <- get        -- this would normally extract the `a` value
                  -- but get returns the (s, s) pair, so a = s
  put (f s)
```

Recall the relation between the type parameters in the `State` type and the values inside the `State` data ctor. For `get`, we want the functions we write to be able to read the current state - however, the only part of the data type the functions can "interact with" is the `a` value in the returned pair. Igonoring the `State` ctor, `get :: State s s` is just `get :: s -> (s, s)`, i.e. `get` takes the state `s` and returns it twice, as a pair `(s, s)`. The `get` returns the state twice, once as the state variable `s` and once as the "orignal" return value `a`, which results in it returning the `(s, s)` pair. The `get` needs to duplicate the state value from the `State`'s first type param (which the user-facing functions cannot access) to its second type param (which they can access), giving `put` the data it needs to do its job.

The `put` gets the state but throws it away, returning the given state and nothing important (as the "original" return value), `(s, ())`.

`modify` gets the current state, as `s`, and updates it using the given function `f`, which takes the state `s` and changes its value (but not the type). Lastly, `modify` returns a pair of the updated state `f s` and nothing important (as the "original" return value), `(f s, ())`.

## In terms of primitives

Now that we have everything we need, rewrite the list manipulation functions from before using `get`, `put`, `modify`, and do-notation. Try running them with an initial count of 0.

Here are the possible versions of `reverseWithCount`

```hs
reverseWithCountM1 :: [a] -> State Int [a]
reverseWithCountM1 xs =
  do
    s <- get
    put (s + 1)
    return (reverse xs)

reverseWithCountM2 :: [a] -> State Int [a]
reverseWithCountM2 xs =
  do
    modify succ
    return (reverse xs)

reverseWithCountM3 :: [a] -> State Int [a]
reverseWithCountM3 xs = get >>= \s -> put (s + 1) >> return (reverse xs)

reverseWithCountM4 :: [a] -> State Int [a]
reverseWithCountM4 xs = modify succ >> return (reverse xs)
```

Here are the possible versions of `appendReversedWithCount`

```hs
appendReversedWithCountM1 :: [a] -> [a] -> State Int [a]
appendReversedWithCountM1 xs ys =
  do
    xs' <- reverseWithCountM4 xs
    ys' <- reverseWithCountM4 ys
    modify succ
    return (xs' ++ ys')
```

Running the function with an initial count of 0.

```hs
x7 :: State Int [Integer]
x7 = appendReversedWithCountM1 [1,4,5,3,2] [9,6,8,5,7]

x8 :: (Int, [Integer])
x8 = runState x5 0 -- (3, [2,3,5,4,1,7,5,8,6,9])

x9 :: [Integer]
x9 = snd $ runState (appendReversedWithCountM1 [1,4,5,3,2] [9,6,8,5,7]) 0
```

The stateful functions are called with their original arguments, but now these functions return a `State s a` value, meaning we need to call the `runState` on the returned `State` value and the initial state value.
