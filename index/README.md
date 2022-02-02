# Indices

index-of-std-packages.md
index-of-std-modules.md
index-of-std-data-structures.md
index-of-std-containers.md
[Index of std classes](./index-of-std-classes.md)
index-of-std-monads.md
index-of-std-monad-precursors.md
[Index of std monad transformers](index-of-std-monad-transformers.md)
index-of-std-monadic-classes.md
index-of-computations-and-monads.md
index-of-packages.md
index-of-modules.md
index-of-data-structures.md
index-of-containers.md
index-of-classes.md
index-of-monads.md
index-of-monad-precursors.md
index-of-monad-transformers.md
index-of-monadic-classes.md


- functions
- data types
  - related functions (non-class methods)
- classes
  - methods


Σιγμα : runΣιγμα : ΣιγμαT : runΣιγμαT : MonadΣιγμα

monad     | transform | accesor/method| related class
----------|-----------|---------------|---------------
N/A       | N/A       | liftBase      | MonadBase
N/A       | N/A       | liftIO        | MonadIO
N/A       | N/A       | lift          | MonadTrans
Reader    | ReaderT   | runReaderT    | MonadReader
Writer    | WriterT   | runWriterT    | MonadWriter
State     | StateT    | runStateT     | MonadState
RWS       | RWST      | runRWST       | N/A
Cont      | ContT     | runContT      | N/A
Select    | SelectT   | runSelectT    | N/A
Identity  | IdentityT | runIdentityT  | N/A
Indetity  | IndetityT | runIndetityT  | N/A
Maybe     | MaybeT    | runMaybeT     | N/A
[]        | ListT     | runListT      | N/A




- data type     : Reader
- transformer   : ReaderT
- related class : MonadReader


```hs
type    Reader  r     = ReaderT Identity
newtype Reader  r a   = Reader  { runReader  :: a ->   r }
newtype ReaderT r m a = ReaderT { runReaderT :: a -> m r }

```
