# ST trick

(from the book: *Thinking With Types* by Sandy Maguire, 2019   
Chapter 7.2 Scoping Information with Existentials)


*Existential types* can be used to prevent information from leaking outside of a desired scope. For example, it means we can ensure that allocated resources can't escape a pre-specified region. We can use the type system to prove that a HTTP session-token is quarantined within its request context, or that a file handle doesn't persist after it's been closed.

Because existential types are unable to exist outside of their quantifier, we can use it as a *scoping mechanism*. By *tagging sensitive data* with an existential type, the type system will refuse any attempts to move this data outside of its scope.

Haskell's `ST` monad is the most famous example of this approach, lending its name to the approach: *the ST trick*. ST allows us to write stateful code, including *mutable variables*, to perform computations, **as long as the statefulness never leaves the monad**. In other words, ST allows you to compute pure functions using impure means.

The ST is just plain library code, not some copiler intrinsic. We can implement it using `unsafePerformIO`, the function that is fundamentally unsafe, but observe that there is nothing inherently unsafe about mutable variables. It's not the presence of mutable variables that makes code hard to reason about. So long as all of its mutations are kept local, we know that a computation is pure.

Mutable variables on their own do not cause us to lose referential transparency.*Referential transparency* is lost when code relies on *external mutable variables*. Doing so creates an *invisible data dependency* between our code and the state of its external variables. It is these cases alone that we need worry about. As such, it's completely safe to have mutable variables so long as you can prove they never escape; and the ST trick exists to prevent such things from happening.

At its heart, `ST` is just the `Identity` monad with a phantom type parameter, `s`. It exists only as a placeholder for the existential type *tag*.

```hs
newtype ST s a = ST { unsafeRunST :: a }
-- Entering this in GHCi outputs:
--   type role ST phantom representational
--   newtype ST s a = ...
--   unsafeRunST :: ST s a -> a
```

Applicative and Monad instances can be provided for ST. To ensure that our "unsafe" IO is performed while it's actually still safe, these instances must be explicitly *strict*. This is not necessary in general in order to perform the ST trick; it's necessary here only because we'll be using `unsafePerformIO` for the example.

```hs
import Data.IORef

-- ST :: a -> ST s a
-- unsafeRunST :: ST s a -> a

instance Functor (ST s) where
    fmap :: (a -> b) -> ST s a -> ST s b
    fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
    pure :: a -> ST s a
    pure = ST
    
    (<*>) :: ST s (a -> b) -> ST s a -> ST s b
    ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
    return :: a -> ST s a
    return = ST

    (>>=) :: ST s a -> (a -> ST s b) -> ST s b
    ST a >>= f = seq a $ f a

-- IORef as a mutable var
newtype STRef s a = STRef { unSTRef :: IORef a }
-- GHCi outputs:
--   type role STRef phantom representational
--   newtype STRef s a = ...
--   unSTRef :: STRef s a -> IORef a

-- STRef :: IORef a -> STRef s a
-- unSTRef :: STRef s a -> IORef a
```

Mutable variables can be introduced inside of the ST monad. For our implementation, we can simply implement these in terms of `IORef`.

Pay attention to the fact that `STRef` also has a phantom `s` type parameter. This is not accidental as `s` acts as a label irrevocably knotting a `STRef` with the `ST` context that created it.

Function wrappers for `STRef` around `IORef` are provided, each of which unsafely performs IO. For example, we'd like to be able to create new `STRef`s.

```hs
newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef
```

Creating a `STRef` gives us one whose `s` TP is the same as `s` in `ST`. 
*This is the irrevocable bond that links the two types together*.

There are a few more useful functions to wrap.

```hs
readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref $ f a
```

And finally, we provide a function to escape from the `ST` monad. This is merely `unsafeRunST`, but with a specialized type signature.

```hs
{-# LANGUAGE RankNTypes #-}

runST :: (forall s. ST s a) -> a
runST = unsafeRunST
```

Here we see the introduction of the ST trick.

The type `(forall s. ST s a)` indicates that `runST` is capable of running only those `ST`'s which do not depend on their `s` parameter.

We will discuss why exactly this works shortly, but let's first convince ourselves that `runST` lives up to its promises. We can write a safe usage of `ST`, one which uses its state to compute a pure value.

```hs
safeExample :: ST s String
safeExample = do
    ref <- newSTRef "The ST"
    modifySTRef ref (++ " trixie!")
    readSTRef ref


ex1 = runST safeExample     -- "The ST trick."


-- Attempt to escape scope produces an ERROR
err_ex2 = runST (newSTRef True)
{-
• Couldn't match type 'a' with 'STRef s Bool'
  because type variable 's' would escape its scope
  This (rigid, skolem) type variable is bound by
  a type expected by the context:
       forall s. ST s a
  Expected type: ST s a
  Actual   type: ST s (STRef s Bool)
• In the first argument of 'runST', namely '(newSTRef True)'
  In the expression: runST (newSTRef True)
  In an equation for 'it': it = runST (newSTRef True)
• Relevant bindings include it :: a
-}
```

The type system now prevents us from `runST`-ing any code that would leak a reference to the `STRef`.

### How does it work

Let's look again at the type of `runST`

        runST :: (forall s. ST s a) -> a

The word *forall* here acts as a quantifier over `s`; this TP exists in scope only within `ST s a`. Because it's existential, without a quantifier, we have no way of referencing it, thus *it doesn't exist outside of its forall binder*.

And that precisely, is the secret of the ST trick. We exploit the fact that *existentials can't leave their quantifier* in order to scope our data. The "quarantined zone" is defined with an existential quantifier, *we just tag the quarantined data* with the resulting existential type, and the type system does the rest.

To really drive this home, let's look at a specific example. Take again the case of `runST (newSTRef True)`. If we specialize the type of `runST` here, it results in the following:

```hs
runST :: (forall s. ST s (STRef s Bool))
      -> STRef s Bool
```

Written like this, it's more clear what's going wrong. The type variable `s` is introduced (and scoped on the first line); but later, `s` is referenced (on the line below), and at that point the type no longer exists, there isn't any type `s` in scope.

GHC calls `s` **a rigid skolem type variable**.

**Rigid variables** are those that are constrained by a type signature written by a programmer; they are not allowed to be type-inferred since the programmer has already stated their type.

> A **skolem** is (for all intents and purposes) any existential type.

(Mathematically, it is an existentially quantified (∃) variable expressed in terms of a forall quantifier (∀). Since in Haskell we only have forall quantifiers, all existential variables are necessarily skolems.)

The purpose of the phantom variable `s` in `ST` and `STRef` is exactly to introduce a rigid skolem.
- if it weren't rigid (specified), it would be free to vary, and Haskell would correctly infer that it is unused.
- if it weren't a skolem, we would be unable to restrict its existence.

The ST trick can be used whenever you want to restrict the existence of some piece of data. In the wild, it is used to tag vars owned by external FFI, or to implement monadic regions which have varying effectful capabilities.
