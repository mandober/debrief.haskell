# Free monad

* Introduction to Free Monads - Nikolay Yakimov, 2022
https://serokell.io/blog/introduction-to-free-monads

Free monads are basically a way to easily get a generic pure `Monad` instance for any `Functor`. This can be useful in many cases when we are dealing with tree-like structures:
- building an AST for an eDSL using do-notation
- having different semantics for the same monad in different contexts, e.g. define an interpreter and a pretty-printer for an eDSL, or have a mock interpreter in addition to a real one
- building a decision-tree type structure harnessing the do-notation for non-determinism (like with lists, but for trees)

All of this is perfectly achievable with regular monads and some newtype wrappers, but free monads let us get rid of a bit of boilerplate.

## Free algebraic structures

In abstract algebra, something is a "free X" when it is, in some sense, a minimal structure that satisfies conditions for being X.

The exact definition of "minimal" in abstract algebra is a little vague, but the main idea is that "free X" satisfies all the laws for X and does not have any other laws describing it. In that context, *free* means "unrestricted" - no other laws are restricting the structure apart from those absolutely necessary. That said, in most cases, we actually might also get the structure "for free".

More formally, a **free structure** over a set `S` is a set `M`, together with operations on elements of `M`, such that:
- there is an embedding (injection) `i : S → M`
- `M` is a minimal closure, i.e. it only contains all the results of applying `i` to elements of `S`, and any results of applying the operations, defined as part of the structure, to the elements of `M`.
- the only laws that hold for the generated structure are the bare laws required for that structure (to be that structure); e.g. for a free monoid, the only axioms required to hold are the monoid axioms: closure, associativity and identity, and nothing else.

An embedding is an injective morphism. That is, an embedding `S → M` is a mapping from elements of `S` to elements of `M`, such that each element of `S` is mapped onto a unique element of `M` (injectivity), and, in some sense, this mapping is structure-preserving (a morphism). The point is that every element of the original set `S` is uniquely represented in the free structure `M`.

## Monoids

In Haskell, any type `α` in the `Monoid` type class must define 2 functions:
- to construct a neutral element, `mempty :: α`
- to combine two elements, `mappend, (<>) :: α -> α -> α`

Additionally, to be a proper monoid, the type must satisfy the monoid laws:
- associativity:   (x <> y) <> z = x <> (y <> z)
- left identity:   mempty <> x = x
- right identity:  x <> mempty = x

A *free monoid* must have these two operations (together with an embedding from some underlying set) and satisfy these 3 laws, and only these 3 laws.

For example, commutativity, `x <> y = y <> x`, should only hold if it follows from the monoid laws - and it does not. So monoids are presummed to be neither commutative nor non-commutative.

## List monoid

As a concrete example, a list (or, in general, a sequence) of elements of the set `S` is a free monoid over `S`.

>A list of elements of type `a` is a *free monoid over `a`*.

Indeed, if `M` is a set of lists of elements of `S`, then the embedding can be defined as:

```hs
type M = [S]
-- type M = Set [S]   -- innit?

i :: S -> M           -- i :: S -> [S]
i x = [x]

-- i.e.
embed :: forall a. a -> [a]
embed a = [a]
```

Then `mempty = []` and `(<>) = (++)`.

`M` contains the empty list, `[]`, as required by `mempty`, all singleton lists, all two-element lists, and so on; in fact, all possible concatenations of elements of `S`.

>Finally, all the monoid laws hold, and no other laws are implied.

On the other hand, addition over integers doesn't constitute a free monoid,
>(ℤ, +, 0) is not a free monoid
since it would imply the law of commutativity, which is not a monoid law, and, in the case of integers, does not follow from the monoid laws.

Similar to monoids, any set-theoretic construction can, in principle, have a free counterpart, like free groups or free rings.

One objection to this somewhat informal introduction might be that the concept of *"minimal structure"* is not very well-defined. Category theory offers a way to define a free structure as a *left adjunct of some forgetful functor*, see
https://bartoszmilewski.com/2016/04/18/adjunctions/


### ASIDE: UNRESOLVED QUESTIONS

So, a free monoid must not be commutative? Nor idempotent? Nor anything else? That is, no other axiom *must* hold except associativity and unitality, right? Hmmm, sounds very suspicious...

>Why get stuck only on commutativity? Shouldn't we check that all possible axioms (except the ones that should hold) do indeed fail (before declaring something to be a free structure)?

If list must not be commutative to be a free monoid, then they must not be cancellative either? Except, the axiom of (left) cancellativity seems to hold:

`xs ++ ys = xs ++ zs` ==> `ys = zs`

Does this mean that
- I've fucked up expressing cancellativity, and it does in fact also fail?
- List is cancellative (and god knows what else), but whatever?
- Somehow it is only important that commutativity fails?

```hs
xs <> ys = xs ++ ys

[] ++ xs = xs = xs ++ []              -- unit ✔
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs   -- assoc ✔
xs ++ ys ≠ ys ++ xs                   -- non-commutative ✔
xs ++ xs ≠ xs                         -- non-idempotent ✔
xs ++ (rev xs) ≠ []                   -- non-invertable ✔ assuming xs⁻¹ = rev
-- but
xs ++ ys = xs ++ zs ==> ys = zs       -- non-cancellativity ✘ FAILS!
f (xs ++ ys) = f xs ++ f ys           -- non-distributivity ✘ FAILS!
```

Reolution: 
it seems that axioms that follow from the required ones are ok. And cancellativity follows from ... (TODO)


### Addition over naturals

Addition over naturals does constitutes a free monoid despite that addition is commutative over naturals. For naturals, commutativity immediately follows from associativity:

```
n + k = (1 + … + 1) + (1 + … + 1) = (1 + … + 1) + (1 + … + 1) = k + n
          n times       k times       k times       n times
```

This doesn't hold for integers which can have different signs. To see why, assume, e.g. `n < 0 < k`. In this case, it's impossible to get `k+n` from `n+k` just by reshuffling parentheses.

Another way to look at it is:
>a free monoid doesn't do anything interesting.
Hence, we can "recover" any other monoid from a free one.

More formally, if `M` is a free monoid over `S`, then for any monoid `N`, given a map `f : S → N`, we can extend this mapping to a monoid morphism `fʹ: M → N` in a unique way.

This also implies that all free monoids over the same set are isomorphic.

Thus, if addition over naturals is a free monoid over some set `S`, then we can convert this to a list monoid over the same set (which we know is a free monoid). The trick is in the choice of the set: if we choose `S = {1}`, we can map addition to concatenation and natural numbers to lists of corresponding lengths. Hence,
>addition over naturals is a free monoid over a singleton set.

## Monads

The monad laws:
- ASS: (m >>= g) >>= h == m >>= (\ x -> g x >>= h)
- LID: return a >>= f  == f a
- RID: m >>= return    == m

They are similar to the monoid laws. The names of the laws are the same, at least. Our choice of the definition makes it a little harder to see the connection, though. If we express the Kleisli composition in terms of `>>=`, it is easier to make a comparison:

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
```

- ASS: f >=> (g >=> h) == (f >=> g) >=> h
- LID: return >=> f    == f
- RID: f >=> return    == f

That is why a monad is a monoid: *a monad is a monoid in the category of endofunctors*. It satisfies the same laws, and the "values" are of the type `Functor f => a -> f b`. Note, though, that this is not the category of endofunctors. However, there exists an isomorphism with the category of endofunctors.

We could alternatively define monads via `join`, which would make it a little more evident that monads are monoids over endofunctors.

https://www.reddit.com/r/math/comments/ap25mr/a_monad_is_a_monoid_in_the_category_of/

https://wiki.haskell.org/Hask

We can guess a few things based on the intuition we gained by looking at free monoids. First, we might note that `return` and `>=>` correspond to `mempty` and `<>`, respectively. Second, we can expect that for any `Functor f :: Type -> Type`, there is a corresponding free monad, `Free f :: Type -> Type`, the same as for free monoids.

## Free monads

Let us first try to construct `Free f` by analogy. We've seen that lists are free monoids. The typical definition of a list type in Haskell looks something like this:

```hs
data List a = Nil | Cons a (List a)
```

Now, `List a` is a type, but `Free f` is a unary type ctor that needs one more type argument. Doing that directly yields:

```hs
data Free f a = Nil a | Cons f (Free f) a
```

We can't define such a data type - `f` is not a plain type, so it can't be an argument of a data constructor. But we're actually very close.

Let us now look at the definition of `Free f`:

```hs
data Free f a = Pure a | Free (f (Free f a))
```

As you can see, it is indeed very similar to a list. Note, however, that it is a bit more general: the functor is arbitrary, potentially making it a tree with branches of type `f` and leaves of type `a`. Leaves are encoded via the `Pure` constructor, and branches via the `Free` constructor. We can also note that leaves correspond to pure values, and branches correspond to *monadic "actions"*, respectively.

This point is important, so we repeat: free monad is similar to a list, but, unlike a list, "continuation" (i.e. `Free`) can be a branching structure, depending on the choice of the base functor `f`.

It may be helpful to write out the types of these constructors explicitly:

```hs
Pure :: a -> Free f a
Free :: f (Free f a) -> Free f a
```

If you squint a bit, you will notice that `Pure` has the same signature as `return` (assuming that `Free f` is a monad). If you squint some more, you will notice that the signature of `Free` is very similar to the signature of `join`:

```hs
join :: Monad m => m (m a) -> m a
```

Instead of defining monads in terms of return and (>>=), we can alternatively define them in terms of `fmap`, `return`, and `join`. Hopefully, this provides some insight into why such a structure works as an encoding for a free monad.

Indeed, if we require that `f` in `Free f` must be a functor, we get `fmap` from the start, and `Pure` and `Free` fill the roles of `return` and `join`.

If `f` is a functor, then `Free f` is also a functor:

```hs
instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap f <$> g)
```

Notice that we need `f` to be a functor to recursively descend `Free` branch.

If `f` is a Functor, then `Free f` is also a monad:

```hs
instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)
```

Note that the implementation of `>>=` is very similar to that of `fmap`.

If we instead defined the monad instance in terms of `join`, it would look something like this:

```hs
join (Pure x) = x
join (Free x) = Free $ join <$> x
```

This implementation, while straightforward, is not particularly efficient. A more efficient implementation is known as *Church-encoded free monads*.

Free monads, along with free applicative and alternative functors, and cofree comonads, are provided by the `free` package on Hackage: 
https://hackage.haskell.org/package/free

>Using free monads, we can define a computation as a data structure. Here, "computation" is a very broad term. The data structure in question doesn't define how the computation is performed, and we can write a separate interpreter (or many interpreters) that performs the actual computation.

## Using free monads

With the theory out of the way, let us see some examples.

### State as a free monad

Let us start off with the State monad. We'll pretend we "forgot" that it's a monad and will try to implement it from scratch in terms of a free monad. First, we define a typical State newtype and let GHC auto-derive the functor instance:

```hs
newtype StateF s a = StateF { runStateF :: s -> (a, s) }
  deriving stock Functor
```

Note that we are calling it `StateF` for *state functor*.

We can also define the primitives for working with state:

```hs
getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)
```

Now we can define a "free state" monad:

```hs
type State s = Free (StateF s)
```

We'll also need to lift our state operations into the monad. We could do it manually, of course. We have `StateF s a`, and we need to get `StateF s (Free (StateF s) a)` and wrap it in `Free`. To lift any `a` into `Free f a`, we just need to apply `Pure`:

```hs
get :: State s s
get = Free $ Pure <$> getF
```

But since it is a common pattern, the 'free' library provides a function to do this for us:

```hs
liftF :: (Functor f, MonadFree f m) => f a -> m a
```

`MonadFree` is just an MTL-style type class for different encodings of the free monad. Also, while on the topic of different encodings, for more flexibility, instead of directly using `Free` and `Pure`, one would generally use `wrap` (defined in `MonadFree`) and `pure` respectively. In our case, we can pretend that:

```hs
liftF :: Functor f => f a -> Free f a
```

Hence, we can define:

```hs
get :: State s s
get = liftF getF

put :: s -> State s ()
put = liftF . putF
And, like magic, we have a State monad:

someComputation :: State Int ()
someComputation = do
  i <- get
  put $ i + 1
  pure ()
```

There is a catch, however. This code will happily compile, but it doesn't do anything interesting. Indeed, we didn't define the meaning of the computation, only its form, and we can't directly use `runStateF` to "run" our free state monad.

This means we need to write an interpreter. Remember that `Free` has 2 ctors, Pure and Free, so we need to pattern-match on both:

```hs
runState :: State s a -> s -> (a, s)
runState (Pure x) s = (x, s)
runState (Free f) s =
  let (m, s') = runStateF f s
  in runState m s'
```

If you're familiar with the usual state monad, you may have noticed that we've essentially moved the implementation of `>>=` to `runState`. The free monad doesn't specify the meaning of monadic actions, so we have to decide what those actions mean when we're running it.

To illustrate this point, we're going to write another interpreter, which is essentially a pretty-printer for the flow of a State computation:

```hs
printState :: (Show s, Show a) => State s a -> s -> String
printState (Pure x) s = "pure (" <> show x <> "," <> show s <> ")"
printState (Free m) s =
  let (x, s') = runStateF m s
  in "state change " <> show s <> " -> " <> show s' <> "\n"
    <> printState x s'
```

If we run `someComputation` above through this interpreter by invoking `printState someComputation 1`, we'll get the following output:

    state change 1 -> 1
    state change 1 -> 2
    pure ((),2)


The first line of output corresponds to `get`. While it doesn't modify the state, it's still an action, so the state change line is still printed. The second line corresponds to `put $ i + 1`, which naturally changes the state to 2. The third line corresponds to `pure ()`, but it also outputs the end state.


Let's summarize what we've learned so far:
>We can take any usual base functor for some monad and get a monad instance "for free". However, we also need to define the semantics of this monad by writing an interpreter (or interpreters).


### List as a free monad

Not all free monads behave exactly the same as their regular counterparts. For instance, you might know that the list monad encodes non-determinism.

If the statement about list monad encoding non-determinism is surprising, consider that one could use a list to represent all possible outcomes of some event and then handle those one by one. For further reading on this topic, refer to the School of Haskell article on the list monad or the Haskell wikibook section on understanding monads:

https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/13-the-list-monad#non-deterministic-computations

https://en.wikibooks.org/wiki/Haskell/Understanding_monads/List


However, if we derive a free monad for lists, the behavior is slightly different. To give a specific example:

```hs
listComputation :: Free [] Int
listComputation = do
  x <- liftF [1, 2, 3]
  y <- liftF [10, 20]
  z <- liftF [100, 200]
  pure $ x+y+z

printFreeList :: Show a => Free [] a -> String
printFreeList (Pure x) = show x
printFreeList (Free f) = "["
  <> intercalate "," (printFreeList <$> f)
  <> "]"
```

If we run printFreeList listComputation, we will get:

    [[[111,211],[121,221]],[[112,212],[122,222]],[[113,213],[123,223]]]

Notice how it gets us what is essentially nested lists, unlike the regular list monad. Notice also that it's essentially a *rose tree*. Of course, we can get the standard list monad behavior by concatenating the nested lists, but we might not necessarily want to do that.

In general, we can restore any proper monad behavior from a free monad since it doesn't define any semantics beyond those necessary for any monad. free provides a function for that:

```hs
foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
```

Given a function (a natural transformation) converting `f x` into some monad `m x`, for any `x`, we can convert `Free f a` into `m a`.

We can get the original list behavior by applying `foldFree id`:

```hs
foldFree id listComputation
-- [111,211,121,221,112,212,122,222,113,213,123,223]
```

#### On natural transformations

The discussion of *naturality* is not particularly relevant here, but we'll introduce the concept briefly. The natural transformation is a structure-preserving transformation between two functors. Here, the naturality condition is enforced by the parametricity, that is, the function is polymorphic in `x`. Essentially, you can think of any function with type `forall a. f a -> g a` as a natural transformation between `f` and `g`, so long as you ignore 'undefined' and other bottoms.


### Free monads for eDSLs

By now, we hopefully got some intuition for how free monads work. Let us now define a toy calculator language that reads a few integers from the standard input, adds them together, and prints the result.

>The key intuition here is that if the next action depends on the previous one, we need to encode this as a continuation, i.e. we need to have a function accepting some argument and returning the functor parameter.

For example, for addition, we would need to define a data constructor with 3 args: two operands and the continuation. The continuation would need to accept the result of addition and (eventually) return some end result.

If a continuation doesn't accept a value, we might just as well encode it as its end result. Since Haskell is lazy, `() -> a` is basically the same as `a`.

Now, if we try to define a functor where every operation uses the same type everywhere, we'll run into a bit of an issue. Consider, e.g., this datatype:

```hs
data ASTF a
  = Add a a (a -> a)
  | Input (a -> a)
  | Output a a
```

If we try to derive a Functor instance, GHC complains:

    Constructor 'Add' must not use the type variable in a function argument


It is in fact impossible to define a lawful Functor instance if the functor's type variable appears as an input argument (this has to do with variance). Hence, we introduce two type variables: one for operation arguments, and another for the overall result.

```hs
data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  deriving Functor
```

Notice that for `Add,` we're saying that it takes two arguments `t`, its result is also `t`, but ultimately, the continuation returns the type `a`.

In practice, we will define `add :: t -> t -> Free (ASTF t) t`, but to have a lawful Functor and also more flexibility, we're not assuming these types are the same. Similar reasoning applies to `Input`.

With `Output`, we could've defined it as `Output t (() -> a)` since our output action doesn't have any meaningful result. But we omit this extraneous argument and simply encode the continuation as a value. In practice, we set `a` to `()`.

We can now define our monad and a few helper functions:

```hs
type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ Output x ()
```

And write a simple program:

```hs
program :: (Read a, Show a) => FreeAST a ()
program = do
  x <- input
  y <- input
  r <- add x y
  output r
```

Now, the interpreter for this program will need to convert our `FreeAST` to `IO` because we are doing input and output. We can do this with `foldFree`; we only need to provide a conversion from the base functor to `IO`.

```hs
computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
  where
  go :: ASTF Int x -> IO x
  go arg = case arg of
    Add x y next  -> pure $ next (x + y)
    Input next    -> next . read <$> getLine
    Output x next -> do
      print x
      pure next
```

We can also write other interpreters, of course. For instance, we could write a pretty-printer here. Using the fact that we can convert a free monad to any other monad using `foldFree`, we'll do just that.

For instance, let's convert `FreeAST String` into `WriterT String (State Int)`. We'll be naming variables using a counter in the State and producing the printed output using the `Writer`. Note that the argument type `t` is set to `String` here since our terms will be variable names, not values.

```hs
printAST :: FreeAST String () -> String
printAST fast = snd $ evalState (runWriterT $ foldFree go fast) 0
  where
  freshVar :: State Int String
  freshVar = do
    n <- get
    put  $ n + 1
    pure $ "x" <> show n
  go :: ASTF String a -> WriterT String (State Int) a
  go f = case f of
    Add x y g -> do
      var <- lift freshVar
      tell $ var <> " <- add " <> x <> " " <> y <> "\n"
      pure $ g var
    Input x -> do
      var <- lift freshVar
      tell $ var <> " <- input\n"
      pure $ x var
    Output s next -> do
      tell $ "output " <> s <> "\n"
      pure next
```

Notice that, up to this point, all constructors of our ASTF had exactly one parameter that had anything to do with the functor. A question you might have now is: what happens if we have several? If you've gained a bit of intuition for free monads by now, you might suspect we get a branching computation, and that's exactly right! Let's add simple branching to our toy language, via `If`, to see how that works.

```hs
data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  | If t a a
  deriving Functor
```

We add a new constructor, `If`, with one scalar parameter and two branches. We can now define a helper:

```hs
if' :: t -> FreeAST t a -> FreeAST t a -> FreeAST t a
if' cond t f = Free $ If cond t f
```

Notice how instead of `liftF` we have to use `Free` directly here - our branches are already of type `FreeAST`. As discussed earlier, in general you would use `wrap` defined in `MonadFree` instead of `Free` directly, but we are glossing over that for simplicity. We now modify the interpreter `computeAST` by handling the `If` case:

```hs
computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
  where
  go :: ASTF Int x -> IO x
  go arg = case arg of
    Add x y next -> pure $ next (x + y)
    Input next -> next . read <$> getLine
    Output x next -> do
      print x
      pure next
    -- only changes are below
    If cond t f -> if cond /= 0 then pure t else pure f
```

Here we define the semantics of the branching - for the sake of not complicating the example, we interpret 0 as 'false' and non-zero as 'true'.

When we try modifying the pretty printer, we run into a problem. Since our branching operation introduces a bit of non-determinism into the computation, our approach with `WriterT (State Int)` doesn't work anymore. One could fix it by explicitly adding some kind of non-determinism transformer on top of this, like `ListT` or `LogicT`, but let us instead write the pretty-printer directly.

```hs
printAST :: FreeAST String a -> String
printAST fast = snd $ evalState (runWriterT $ go fast) 0
  where
  freshVar :: State Int String
  freshVar = do
    n <- get
    put  $ n + 1
    pure $ "x" <> show n
  go :: FreeAST String a -> WriterT String (State Int) ()
  go (Pure _) = pure ()
  go (Free f) = case f of
    Add x y g -> do
      var <- lift freshVar
      tell $ var <> " <- add " <> x <> " " <> y <> "\n"
      go   $ g var
    Input x -> do
      var <- lift freshVar
      tell $ var <> " <- input\n"
      go   $ x var
    Output s next -> do
      tell $ "output " <> s <> "\n"
      go next
    If cond onTrue onFalse -> do
      tell $ "if' " <> cond <> " (do\n"
      _ <- go onTrue
      tell "\n) (do\n"
      _ <- go onFalse
      tell "\n)\n"
```

Not much has changed. Instead of `pure`, we use explicit recursion, and we have to handle the `Pure` case explicitly. For the sake of simplicity, the return type of `go` is fixed at `()` since we don't care about the result here.

When we try to run this pretty-printer on some code that uses branching, we might be a little surprised by the output. For example, with the following "program":

```hs
someAST :: (Read a, Show a) => FreeAST a ()
someAST = do
  x <- input
  y <- input
  r <- if' x (add x y) (pure y)
  output r
```

Applying our pretty-printer, we get:

```hs
x0 <- input
x1 <- input
if' x0 (do
x2 <- add x0 x1
output x2
) (do
output x1
)
```

The reason for this is simple: when the free monad is built up, the continuation is passed to each place where the base functor is recursive in its parameter. This means that all the code after our `if'` command gets copied to both branches. There isn't a workaround for this because free monads build trees and not general graphs. However, a more efficient representation - e.g. the Church encoding mentioned previously - will reduce the overhead.

#### Decomposing ASTs

Naturally, we don't have to define the whole AST at once - we're free to decompose it however we see fit. Likewise, we can also decompose interpreters.

For instance, we could decompose our AST into a part for arithmetic and a part for I/O:

```hs
data ArithASTF t a
  = Add t t (t -> a)
  -- + Sub, Mul, etc
  deriving Functor

data IOASTF t a
  = Input (t -> a)
  | Output t a
  deriving Functor

data ASTF t a
  = Arith (ArithASTF t a)
  | IO (IOASTF t a)
  deriving Functor
```

We would have to modify the helper functions slightly:

```hs
input :: FreeAST t t
input = liftF $ IO $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Arith $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ IO $ Output x ()
And we could also decompose the interpreter:

computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree computeASTF

computeASTF :: (Num t, Read t, Show t) => ASTF t x -> IO x
computeASTF arg = case arg of
  Arith c -> pure $ computeArith c
  IO c -> computeIO c

computeArith :: Num t => ArithASTF t a -> a
computeArith (Add x y next) = next (x + y)

computeIO :: (Read t, Show t) => IOASTF t a -> IO a
computeIO arg = case arg of
  Input next -> next . read <$> getLine
  Output x next -> do
    print x
    pure next
```

Notice how we managed to neatly decompose the interpreter into two: one pure and one doing I/O.

This all might be pretty obvious, but it bears mentioning.

### Trees as free monads

To close out this section, let's look at another application of free monads. Namely, let's build a binary search tree from a sorted list.

Our base functor for the binary tree would look like this:

```hs
data BinTreeF l a = NodeF l a a
  deriving Functor
```

You might ask yourself: where are the leaf and/or nil branch constructors? We can actually encode those as the "return value" of type `Maybe l`, so we don't need them here.

Now we define our free monad type and build the tree.

```hs
type FreeBinTree l = Free (BinTreeF l)

buildBalanced :: [Int] -> FreeBinTree Int (Maybe Int)
buildBalanced [] = pure Nothing
buildBalanced [x] = pure $ Just x
buildBalanced xs = do
  let len = length xs
      (l,x:r) = splitAt (len `div` 2) xs
  b <- liftF $ NodeF x l r
  buildBalanced b
```

The first two cases, where lists have sizes of zero and one, respectively, are self-evident. If there are more elements in the list, we divide it in half. The approximate "middle" element serves as the current node value. We then recursively construct left and right subtrees from the corresponding halves of the list.

Notice how we're passing left and right halves of the list to `NodeF` and then bind it to name `b`. As we discussed previously, multiple functor variables per constructor encode non-determinism. So here, we basically split the computation into two branches. We then recursively build the tree for each branch.

The caveat here is there's no simple way to print this all out, and working with a tree wrapped in the `Free` is probably not particularly convenient. So we can convert this `FreeBinTree` into a regular binary tree, for example:

```hs
data BinTree l = Nil | Leaf l | Branch l (BinTree l) (BinTree l)
  deriving Show

convert :: FreeBinTree a (Maybe a) -> BinTree a
convert (Pure Nothing) = Nil
convert (Pure (Just x)) = Leaf x
convert (Free f) =
  let NodeF x l r = convert <$> f
  in Branch x l r
```

Note how `Pure` values correspond to leaves, and `Free` values correspond to branches. Running `convert . buildBalanced` on a list produces the expected result, i.e. a *binary search tree*.

```hs
> convert $ buildBalanced [0..10]
5 (2 (1 0
        Nil)
     (4 3
        Nil))
  (8 (7 6
        Nil)
     (10 9
         Nil))
```

## Conclusions

Hopefully, after this brief introduction, you understand, at least in principle, what free monads are and how they may be useful. Of course, the only way to get proficient with them is to use them in projects, and no blog post can replace actual hands-on experience, but that applies to everything in programming.

Let us then briefly review what we've learned.

- Free monads are "free" because they do not impose any additional constraints beyond those required by the definition of a monad.
- They are a particular type of a free algebraic structure.
- As such, they are very similar to free monoids.
- They build tree-like structures, which later can be interpreted.
- Any typical Haskell monad can be implemented as a free monad with a corresponding interpreter.
- Generally, a free monad can be converted to any other monad via a natural transformation.
- One particular application of free monads is in building ASTs for eDSLs.
- But you could use them almost anywhere where a tree could be used.

For further reading: it turns out, to get a monad, we don't really even need a functor if we cheat a little. This construction is known as *freer monads* (as in, more free than free). See:

* Free and Freer Monads: Putting Monads Back into Closet
https://okmij.org/ftp/Computation/free-monad.html


All examples in this article are available as code on GitHub:
https://github.com/lierdakil/free-monad-examples


## Exercises

Implement the standard monads Reader and Writer using Free. Here is a template to get you started:

```hs
import Control.Monad.Free

newtype ReaderF r a = ReaderF { runReaderF :: r -> a }
  deriving Functor

type Reader ...

ask :: Reader r r
ask = undefined

runReader :: Reader r a -> r -> a
runReader = undefined

newtype WriterF w a = WriterF { runWriterF :: (w, a) }
  deriving Functor

type Writer ...

tell :: w -> Writer w ()
tell = undefined

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen = undefined

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass = undefined

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = undefined
```

Using a free monad, define a monad for a String-keyed, String-valued key-value store. The store must support two commands, assuming the store monad is called Store:

```hs
type Key = String
type Value = String

getValue :: Key -> Store (Maybe Value)
putValue :: Key -> Value -> Store ()
```

For an interpreter, implement a natural transformation from `StoreF to IO`, using `IORef [(String, String)]` as a backing store. Feel free to use `Map` or `HashMap` if you want to.

Notice that `BinTree` defined above is not a Monad. However, if leaves and branches could have different types, it would be. Now consider the following type:

```hs
data BinTree l a
  = Leaf a | Branch l (BinTree l a) (BinTree l a)
  deriving (Show, Functor)
```

It is a monad.

Implement a conventional Monad instance, and implement `convert :: FreeBinTree l a -> BinTree l a` using `foldFree` and a natural transformation `BinTreeF l a -> BinTree l a`.

If you get absolutely stuck, the intended solutions to exercises are avaliable in the GitHub repository with the examples in the solutions branch.
