# Haskell :: DSA :: Graphs :: Implementation :: Graph classes

```hs
-- | Graph class for digraphs
class Digraph g where
  type Vertex g
  data Edge g

  src :: Edge g -> Vertex g
  tgt :: Edge g -> Vertex g

  outEdges :: g -> Vertex g -> [Edge g]


-- | Class for algebraic graphs
class Grph g where
  type Node g
  empty   :: g
  vertex  :: Node g -> g
  overlay :: g -> g -> g
  connect :: g -> g -> g
```
