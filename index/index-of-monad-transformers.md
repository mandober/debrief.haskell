# Haskell :: Index :: Monads transformers

- `IdentityT` base MT for pure computation
- `IOT`       base MT for effectful computation
- `ReaderT`   global readonly environment
- `WriterT`   logging, tracing, accumulating output
- `StateT`    mutable global state
- `RWST`      combination of Reader, Writer and State
- `MaybeT`    fallable computation, partiality
- `ExceptT`   exceptions, fallable computation with error messages
- `AccumT`    
- `RandomT`   random values
- `ListT`     nondeterministic computation
- `ContT`     continuations
- `SelectT`   search algorithms
- `ParsecT`   parser
- `LogicT`    backtracking
- `ResourceT` bracketing (cleanup used) resources



Mon : MonT : runMonT : mapMonT : MonadMon

precursor    | transformer | computation     | type
-------------|-------------|-----------------|-------------------
Identity     | IdentityT   | pure            |
Maybe        | MaybeT      | fallible        |
Either       | ExceptT     | fallible w msg  |
Reader       | ReaderT     | global ro env   |
Writer       | WriterT     | trace, log      |
State        | StateT      | g mutable state |
RWS          | RWST        | R, W, S         |
List (Maybe) | ListT       |                 | 
Cont         | ContT       |                 | 
Select       | SelectT     |                 | 
             | AccumT      |                 | 



MonadReader
MonadWriter
MonadState
MonadBase
MonadIO
MonadTrans

## ReaderT

- precursor     : Reader
- transformer   : ReaderT
- related class : MonadReader

```hs
type    Reader  r     = ReaderT Identity
newtype Reader  r a   = Reader  { runReader  :: a ->   r }
newtype ReaderT r m a = ReaderT { runReaderT :: a -> m r }
```

## WriterT

- precursor     : Writer
- transformer   : WriterT
- related class : MonadWriter

```hs
type    Writer  a     = WriterT Identity
newtype Writer  w a   = Writer  { runWriter  ::   (w, a) }
newtype WriterT w m a = WriterT { runWriterT :: m (w, a) }
```

## StateT

- precursor     : State
- transformer   : StateT
- related class : MonadState

```hs
type    State  s     = StateT Identity
newtype State  s a   = State  { runState  :: s ->   (s, a) }
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
```


## RWST

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.RWS.CPS.html#RWST

- precursor     : RWS
- transformer   : RWST
- related class : MonadRWS ?

```hs
newtype RWST r w s m a = RWST { unRWST :: r -> s -> w -> m (a, s, w) }
```


## ListT

https://hackage.haskell.org/package/list-t-1.0.5.7/docs/src/ListT.html#ListT

## ExceptT

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.Except.html#ExceptT
