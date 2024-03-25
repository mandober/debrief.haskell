# Monad transformers

Each monad has a certain behavior, providing a specific effect, but when we need to support more effects we need to combine different monads into one.

Monad transformers are about combining monads into monadic stacks. They allow us to pick and combine the required capabilities of different monads and integrate them into a single type.

To support both state and partiality, we can define a new type `StateMaybe`, that combines the `State` and `Maybe` monads.

But do we nest the `State` within Maybe, or `Maybe` within the State?

```hs
-- Maybe precursor monad
data Maybe a = Nothing | Just a
-- MaybeT monad transformer
newtype MaybeT m a = m (Maybe a)

-- State precursor monad
newtype State s a = State (s -> (a, s))
-- StateT monad transformer
newtype StateT s m a = StateT (s -> m (a, s))

-- Maybe + IO (MaybeT with IO plugged in)
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

-- State + Maybe (MaybeT with State plugged in)
newtype StateMaybe s a = StateMaybe (s -> Maybe (a, s))
-- or is it defined like this?
newtype StateMaybe s a = StateMaybe (s -> (Maybe a, s))
-- Maybe + State (StateT with Maybe plugged in?)
newtype MaybeState s a = MaybeState (Maybe (s -> (a, s)))
```
