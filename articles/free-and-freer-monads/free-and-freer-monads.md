---
downloaded:       2022-01-01
page-url:         https://okmij.org/ftp/Computation/free-monad.html
page-title:       Free and Freer Monads
article-title:    Free and Freer Monads
---
# Free and Freer Monads

Low-brow explanation of free and freer monads and of the boilerplate they free us from
Our running example is the familiar State monad, representing the effect of accessing and updating one piece of the global mutable state. The common implementation threads the global state throughout the program, letting the two primitives `get` and `put` get hold of and replace the state:

     newtype State s a = State{unState :: s -> (a,s)}
     
     get :: State s s
     get = State $ \\s -> (s,s)
     
     put :: s -> State s ()
     put s = State $ \\\_ -> ((),s)
     
     runState :: State s a -> s -> (a,s)
     runState = unState

The operations `get` and `put`, with their implied laws, and the interpreter `runState` are the essential parts; they are what makes `State` a mutable-state computation. To conveniently use these operations in Haskell programs, however, we need to write the following instances:

     instance Functor (State s) where
         fmap f (State m) = State $ \\s -> let (v,s') = m s in (f v,s')
     
     instance Applicative (State s) where
         pure x = State $ \\s -> (x,s)
         State f <\*> State x = 
             State $ \\s -> let (vf,s1) = f s
                               (vx,s2) = x s1
                           in (vf vx, s2)
     
     instance Monad (State s) where
         return x = State $ \\s -> (x,s)
         State m >>= k = State $ \\s -> let (v,s') = m s in unState (k v) s'

There is a lot of boilerplate: although `Applicative` and `Functor` can be expressed in terms of `Monad`, they still have to be written explicitly. As a matter of fact, *all* these instances are boilerplate, even the `Monad`. The most general monad, functor and applicative instances can be defined once and for all and we should never bother with these instances and their laws again.

Free monad gets rid of the boilerplate. (The word \`\`free'' refers to the category theory construction of the left adjoint of a forgetful operation. We show what it means in plain English.) Our `State s` is a functor, an applicative and a monad. Let us forget the last two: imagine we have deleted the file with the `Applicative` and `Monad` instances for `State s`. As it turns out, nothing is lost: we can still conveniently use `State s` in our programs, courtesy of the Free monad construction:

     data Free f a where
       Pure   :: a -> Free f a
       Impure :: f (Free f a) -> Free f a
     
     eta :: Functor f => f a -> Free f a
     eta = Impure . fmap Pure

Here the type constructor `f` has to be a functor: that is, if we have an `f a` value and we know how to transform any value of type `a` to a value of type `b`, we should be able to obtain the `f b` value. In other words, `f` should support the `map` operation. Even shorter: `f` has to be a member of the type class `Functor`. As its type says, `eta` turns any functor `f` into the monad `Free f`. `Free f` is indeed a monad (and a functor and an applicative):

     instance Functor f => Functor (Free f) where
       fmap f (Pure x)   = Pure $ f x
       fmap f (Impure m) = Impure $ fmap (fmap f) m
     
     instance Functor f => Applicative (Free f) where
       pure = Pure
       Pure f <\*> m   = fmap f m
       Impure f <\*> m = Impure $ fmap (<\*> m) f
     
     instance Functor f => Monad (Free f) where
       return = Pure
       Pure a   >>= k = k a
       Impure m >>= k = Impure (fmap (>>= k) m)

`Free f` has all these properties for any functor `f`. Once the above instances are introduced, we do not need to write any more monad instances, for state or any other effect. Because the free monad satisfies all -- and only all -- monad, applicative and functor laws, we do not have to bother with proving the monad laws either.

Coming back to our running example: recall that we have forgotten that `State s` is a monad. We now make it a monad again, this time *without* writing any monad and applicative instances. We merely have to say:

     type FState s = Free (State s)

and take on the essentials: the primitives `getF` and `putF` (for which we can reuse `get` and `put` written earlier)

     getF :: FState s s
     getF = eta get
     
     putF :: s -> FState s ()
     putF = eta . put

and the interpreter

     runFState :: FState s a -> s -> (a,s)
     runFState (Pure x) s   = (x,s)
     runFState (Impure m) s = let (m',s') = unState m s in runFState m' s'

We are all set to program stateful computations, for example:

     testState :: FState Int Int
     testState = do 
       putF 10
       x <- getF
       return x
     
     test\_run = runFState testState 0
     -- (10,10)

To reiterate, to use the state effect we only had to: define `State s` and its `Functor` instance, write `get`, `put`, and the interpreter `runFState`. We tackled the important parts, without getting distracted with the boilerplate instances and boilerplate laws. We later see that even the `Functor (State s)` instance is unnecessary.

Recall, we forgot the `Monad` and `Applicative` instances of `State s`, and got them (or, their ersatz copies) back, through the free monad. Free monad gave us the monad and applicative instances for free. As it happens, we may forget even that `State s` is a functor. With `fmap` no longer available, the free monad construction does not work. However, there is a way to obtain `fmap` by formal cheating. If a structure `g a` does not support the `fmap` operation, we may still pretend that it does:

     data Lan g a where
       Lan :: g x -> (x -> a) -> Lan g a
     
     instance Functor (Lan g) where
       fmap f (Lan gx h) = Lan gx (f . h)
     
     lan :: g a -> Lan g a
     lan ga = Lan ga id

(Technically, `Lan` is an instance of the left Kan extension.) `Lan g a` keeps the arguments of `fmap` without actually doing any mapping. Still, it has the appearance of a mappable structure, and gives the needed `Functor` instance and `fmap`. Therefore, `Free (Lan g)` is a monad. We have succeeded to turn `g a` without any special properties into a monad, which we call a *freer* monad. Haskell Symposium 2015 paper describes an optimal and extensible version. For us here, the following desugared `Free (Lan g)` will be sufficient:

     data FFree g a where
       FPure   :: a -> FFree g a
       FImpure :: g x -> (x -> FFree g a) -> FFree g a

In contrast to `Free f`, the type constructor `g :: * -> *` does not have to be a functor; it can be anything at all. And yet `FFree g` is surely a functor, an applicative and a monad: `FFree g` is the monad by its very construction.

     instance Functor (FFree g) where
       fmap f (FPure x)     = FPure (f x)
       fmap f (FImpure u q) = FImpure u (fmap f . q)
     
     instance Applicative (FFree g) where
       pure = FPure
       FPure f     <\*> x = fmap f x
       FImpure u q <\*> x = FImpure u ((<\*> x) . q)
     
     instance Monad (FFree g) where
       return = FPure
       FPure x      >>= k = k x
       FImpure u k' >>= k = FImpure u (k' >>> k)

where

     (>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
     f >>> g = (>>= g) . f

is the composition of side-effectful functions (Kleisli composition). (That monads laws hold is best seen categorically, from the fact that `FFree g = Free (Lan f)`. Category theory really shines here: it gives general theorems, which apply in very many specific circumstances.) Any structure `g a` can be turned into a monad, no matter what it is:

     etaF :: g a -> FFree g a
     etaF fa = FImpure fa FPure

Compared to `eta` of the free monad, `etaF` has no `Functor` constraint and uses no `fmap`, which is the sign of improved efficiency.

To make our `State s` a monad, we no longer have to write any instances at all, not even a `Functor`. We only have to say:

     type FFState s = FFree (State s)

define the primitives

     getFF :: FFState s s
     getFF = etaF get
     
     putFF :: s -> FFState s ()
     putFF = etaF . put

and write the interpreter

     runFFState :: FFState s a -> s -> (a,s)
     runFFState (FPure x) s     = (x,s)
     runFFState (FImpure m q) s = let (x,s') = unState m s in runFFState (q x) s'

That was all we had to do to write programs (e.g., `testState`) with the state effect.

The \`\`formal cheating'', the shifting of the work from the monad bind to the interpreter `runFFState` has become clearer. A typical computation, for example, `testState`, without syntax sugar looks like

     ((FImpure (put 10) Pure) >>= \\\_ -> getF) >>= \\x -> return x

of the general shape

     (((FImpure eff k1) >>= k2) >>= ...) >>= kn

where `eff` identifies the effect to perform, and `k1`,...,`kn` are the follow-up computations. The bind operation of `FFree g` turns the above into

     FImpure eff (k1 >>> k2 >>> ... >>> kn)

which is handed over to `runFFState` to deal with `eff` and then to follow up. Thus `FFree g` collects all effect follow-ups `k1`,...,`kn` into a sort of a heterogeneous list, with `(>>>)` playing the role of the list constructor. (One is reminded of the connection between the free monad and the free monoid, which is a sequence.) Thinking of `k1 >>> k2 >>> ... >>> kn` as a list is not merely amusing. The Haskell 2015 paper has significantly improved the performance of the freer monad by treating `k1 >>> k2 >>> ... >>> kn` truly as a heterogeneous data structure, an efficient queue.

Looking back to our `State s` example, we see that we may forget not only `return` and `bind` but also the `fmap` operation, and still recover the state monad as `FFree (State s)`. `FFree g` is also free (in the category theory/universal algebra sense), and can recover, by formal cheating, the forgotten properties such as `fmap` or `bind`. Compared to `Free f`, the freer monad affords to forget more, and hence has less boilerplate. As a consequence, it is more efficient. Putting it another way, we no longer have to write any basic monad and functor operations and instances in the first place. The freer monad gives them for free.
