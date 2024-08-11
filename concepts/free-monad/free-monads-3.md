# Free monads

* Free and Freer Monads: Putting Monads Back into Closet
https://okmij.org/ftp/Computation/free-monad.html

## Introduction

Writing Monad (and now, Applicative and Functor) instances and making sure the monad laws hold are a big part of defining a monad and even a bigger part in exponentially procreating monad tutorials. We argue that all these instances are boilerplate - avoidable boilerplate. Perhaps because of the Latin-sounding names and the evocation of the highest Math, the petty boilerplate, trivial laws, plain old plumbing have usurped an extraordinary amount of attention. Wouldn't it be refreshing if we could directly think on what an effect does rather than on how the plumbing works.

`Free` and `Freer` monads free us from the boilerplate and let us concentrate on the essentials of side-effects. They let us write *definitional interpreters* for effects, bringing in the tool that worked well in the program language research and practice.

The recently popularized `Freer` monad brought a predicament: Where does it fit in? Does it let us think clearer about effects? Is it also free? If so, how come it has not been anticipated in all those categorical explanations?

We tackle these questions below, except for the last one, of which we can only say that none of the several appearances of freer monads came, to my knowledge, via category theory. Category theory connections did emerge, in hindsight, and did prove insightful.

In contrast to the many free monad explanations, this article takes a lowbrow approach, relying on concrete examples rather than abstract algebra.

* The complete code described in the article: `FreeState.hs`
https://okmij.org/ftp/Computation/FreeState.hs

* Freer Monads and Extensible Effects
https://okmij.org/ftp/Haskell/extensible/

## Traditional monads

Our running example is the familiar State monad, representing the effect of accessing and updating one piece of the global mutable state. The common implementation threads the global state throughout the program, letting the two primitives `get` and `put` get hold of and replace the state:

```hs
newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
```

The operations `get` and `put`, with their implied laws, and the interpreter `runState` are the essential parts; they are what makes State a mutable-state computation.

To conveniently use these operations in Haskell programs, however, we need to write the following instances:

```hs
instance Functor (State s) where
  fmap f (State m) = State $ \s ->
    let (v, s') = m s
    in  (f v, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  State f <*> State x = State $ \s ->
    let (vf, s1) = f s
        (vx, s2) = x s1
    in  (vf vx, s2)

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  State m >>= k = State $ \s ->
    let (v, s') = m s
    in  unState (k v) s'
```

There is a lot of *boilerplate*: although Applicative and Functor can be expressed in terms of Monad, they still have to be written explicitly. As a matter of fact, all these instances are boilerplate, even the Monad.

>Amazingly, it is possible to define the most general `Functor`, `Applicative` and `Monad` instances once and for all, and we should never again be bothered with these instances and their laws.

## Free Monad

Free monad gets rid of the boilerplate. The word 'free' refers to the categorical construction of the *left adjoint of a forgetful operation*. We show what it means in plain English.

The `State s` is a functor, an applicative and a monad. Let us forget the last two: imagine we have deleted the file with the Applicative and Monad instances for `State s`. As it turns out, nothing is lost: we can still conveniently use `State s` in our programs, courtesy of the Free monad construction:

```hs
data Free f a where
  Pure   :: a -> Free f a
  Impure :: f (Free f a) -> Free f a

eta :: Functor f => f a -> Free f a
eta = Impure . fmap Pure
```
