# Haskell :: Index :: Data types

Index of standard data types
- Index of scalar primitives
  - Data.Void
  - Bool
  - Ordering
  - Word
    - Word8
    - Word16
    - Word32
    - Word64
  - Int
    - Int8
    - Int16
    - Int32
    - Int64
  - Float
  - Double
  - Data.Fixed fixed-precision arithmetic
  - Data.Complex
  - Ratio
  - Rational
  - Char
  - Integer
  - Void
  - unit, ()
  - Numeric.Natural
  - GHC.TypeNats
  - GHC.Natural
- Index of compound primitives
  - String type alias, type String = [Char]
  - list, []
  - tuples, (,), (,,), (,,,), …
  - function type, (->)
- Data.Kind.Constraint
- Data.Kind.FUN
- Data.Kind.Type
- FilePath (String)
- ReadS
- ShowS
- Maybe
- Either
- IO
- IOError
- Control.Monad.State.State
- Control.Monad.State.StateT
- Control.Comonad.Store
- Control.Comonad.StoreT
- Data.Functor.Identity
- Data.Functor.Const
- Data.Functor.Composable

 Data Structures
  - Maps and Folds
  - Sets
  - Trees
  - Graphs
  - Recursive structures
  - Recursive polymorphism
  - Cyclic structures
  - Tying the knot
  - containers
  - unordered-containers
  - vector


* Base types (base-types), Machine types (machine-types)
  - Int
  - Char
  - Float
  - Double

* Minset
  + base-types
  - `()` (unit)
  - sum type construction
  - product type construction
  - `⟘` (bottom)



Type ctor arity

* Nullary
  - Int, Integer
  - Float, Double
  - Char
  - String ~ [Char]
  - Bool
  - ()

* Unary
  - Identity a
  - Maybe a
  - [] a
  - Set a
  - Tree a = Leaf | Node a (Tree a) (Tree a)
  - Tree a = Leaf a | Node (Tree a) (Tree a)

* Binary
  - Either e a
  - (->) r a
  - (,) r a
  - Map k v
  - Reader r a = Reader (r -> a)
  - Writer w a = Writer (a, w)
  - State  s a = State (s -> (a, s))
  - Cont   r a = Cont ((a -> r) -> r)

* Ternary
  - (,,) a b c
  - ReaderT r m a = ReaderT (r -> m a)
  - WriterT w m a = WriterT      (m (a, w))
  - StateT  s m a = StateT  (s -> m (a, s))
  - ContT   r m a = ContT  ((a -> m r) -> m r)

* Quaternary
  - RWS r w s a = RWS (r -> s -> (a, s, w))

* Quinary
  - RWST r w s m a = RWST (r -> s -> m (a, s, w))
