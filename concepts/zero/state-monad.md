# State monad

Monads make many language concepts and constructs possible in a purely functional language such as Haskell. They enable us to model things that include state, exceptions, continuations, indeterminism, etc.

The state monad particularly, can be seen as an amalgam of Reader and Writer monads. Reader monad provides a read-only environment that we can consult from different functions. The Writer monad enables logging; e.g. it can empower functions to log their execution. The State monad make both powers available: it takes care of threading a read-write state throughout our program, making it available to our pure functions.

State can be thought of as a lookup table; for example, state can be realized as a lookup table; an environment is then such a state, containing the mapping from variables to their actual values. Probably the simplest way to represent the state is via a pair `(s, a)`, where the type param `s` stands for a type of state and `a` is a type of value we're working with. Actually, taking a hint from automata theory, a state should be represented as a transition from the old (or current) state to the new (or next) state, `s -> (s, a)`.

We have a state represented as the function type:    
`s -> (s, a)`

We can "reify" it as an easier-to-handle entity by defining a type synonym:    
`type State s a = s -> (s, a)`

However, since we can't do shit with type synonyms, we must wrap it in a newtype, which means introducing a data ctor. It'll have the same name as the type ctor, as is the convention:    
`newtype State s a = State (s -> (s, a))`

Unfortunatelly, its introduction means we'll be unwrapping and re-wrapping stuff from/into this ctor at every step: unwrap it by pattern matching to get at the meat inside, do something then re-wrap it before we can return it. But it's the only way to reify the state so we can make it an instance of the classes we're interested in (and we're always intersted in making it a member of Functor, Applicative and Monad, in the least).

Another thing with `newtype` is that we can only use it to wrap a single-data- ctor types. This is the case with our state here (although that data ctor was forced on us). On the bright side, the newtype construction permits havinf the accessor function (one data ctor so one accessor), which we'll take advantage of, and name the accessor function `runState` (as is the convention). It allows us to get at the state type quickly: `runState :: State s a -> s -> (a, s)`.    
runState takes a state (State s a) and returns a state fn (s -> (a, s)).
But (State s a) *is* (s -> (a, s)) so what the hell?! [hopefully resolved later]

We keep in mind that, essentailly, our type is `s -> (a,s)`, and all these extra layers in the final repr of state type are just necessary compications:   
`newtype State s a = State { runState :: s -> (s, a) }`


```hs
-- state is a fn type
s -> (s, a)

-- state as the type synonym
type State s a = s -> (s, a)

-- state as the newtype
newtype State s a = State (s -> (s, a))

-- state as the newtype with the runState accessor
newtype State s a = State { runState :: s -> (s, a) }

-- the runState accessor gets us the true state type
runState :: State s a -> s -> (a, s)
```


Another thing - we might as well define now the monad transformer that involves the state monad and another (arbitrary) monad:    
`newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }`

The test of compatibility and overall corretness is that we should be able to get our `State` type by applying the StateT transformer on the `Identity` monad.

```hs
-- State monad transformer
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- Identity monad (functor)
newtype Identity a = Identity { runIdentity :: a }

-- by applying StateT to Identity we retrieve the State
type State s a = StateT s Identity a

-- in the point-free form it looks:
type State s = StateT s Identity
```
