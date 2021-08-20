# Index of base types

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
