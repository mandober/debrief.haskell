# Monad transformers stacks

- https://wiki.haskell.org/Monad_Transformers
- https://wiki.haskell.org/Monad_Transformers_Explained
- https://www.youtube.com/watch?v=8t8fjkISjus
- https://begriffs.com/posts/2017-04-09-monad-tutorial-workshop.html

Monad transformers are combinations, that is, nestings of different monads in a so-called monadic stack. Here's an example of an evaluator of expression that started as a plain evaluator, but then differentmonads were added as enhancements: `Reader` for readonly shared env, `Except` to support exception throwing, `Writer` to support logging, and finally `State` to hide the threading of the state.


```hs
-- stack o'monads
type Eval a = ReaderT Env
                (ExceptT String
                  (WriterT [String]
                    (StateT Int IO))) a
-- types
ReaderT Env      m a | m := ExceptT String (WriterT [String] (StateT Int IO))
ExceptT String   m a | m :=                 WriterT [String] (StateT Int IO)
WriterT [String] m a | m :=                                   StateT Int IO
StateT Int       m a | m :=                                              IO
```
