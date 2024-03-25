# Haskell :: DSA :: Graphs :: Implementation

A graph is a pair, `G = ⟨V, E⟩`, such that `E ⊆ V⨯V`
- a set of vertices, `V = {v₁, …, vₙ}`
- a set of edges,    `E = {e₁, …, eₙ}`
  - line  is an unordered pair of vertices, `𝓁 = {vᵢ, vⱼ}`
  - arrow is an   ordered pair of vertices, `𝒶 = (vᵢ, vⱼ)`

## Representations

```hs
-- (1)
-- | Graph as an adjacency list of edges.
newtype Edge a = UEdge (a, a)
newtype Graph a = Graph [Edge a]

-- (1)
type Vertex = Int
type Weight = Int
type Graph = ([Vertex], [Edge])
type Edge = (Vertex, Vertex, Weight)

-- (2)
-- Graph as an adjacency function of type: Vertex -> [(Vertex, Weight)]
-- The function domain is the set of vertices, and for each vertex u the value of the function, applied to u, is a set of pairs (v,w) such that (u,v,w) is a labelled edge. If vertices are ints, an impl of adjacency fn is by an array.
type AdjArray = Array Vertex [(Vertex, Weight)]


-- (2)
-- | Graph as an adjacency list of edges.
data family Edge g   -- data family
type family Vertex g -- type family
newtype Graph = Graph [Edge Graph]
-- (3)
-- | Weighted Graph as a mapping from each vertex to a list of its neighbours.
newtype Graph = Graph (Map (Vertex Graph) [Vertex Graph])

-- (4)
newtype Vertex a = Vertex { unVertex :: a }
newtype Edge a = Edge { unEdge :: (Vertex a, Vertex a) }
data Graph a = Graph { vertices :: Set (Vertex a), edges :: Set (Edge a) }

-- (5)
data Graph node key = Graph
  { graph    :: Graph
  , vertices :: Vertex -> (node, key, [key])
  }

-- (6)
-- | Adjacency array of edges
type Graph n w = Array n [(n, w)]

-- (7)
-- | Adjacency matrix representation
-- 2D square array of |V|⨯|V| values where both coordinates are nodes, and an entry (i,j) equals the weight of the corresponding edge between nodes i and j.
type Graph a b = Array (a, a) b

-- (8)
-- | Problem with (7) is the repr of non-existent edges, thus we use Maybe
type Graph n w = Array (n, n) (Maybe w)
```





* Adjacency list
  - vertices as records
  - each vertex stores a list of adjacent vertices
  - allows additional data on vertices
  - additional data can also be realized with edges as records, so
    each vertex stores its incident edges and
    each edge stores its incident vertices

* Adjacency matrix
  - 2D matrix
  - rows for source vertices, columns destination vertices
  - data on edges and vertices stored externally
  - only the cost for one edge can be stored between each pair of vertices

* Incidence matrix
  - 2D matrix
  - rows for vertices, columns for edges
  - entries indicate the incidence relation between 
    the vertex at a row and edge at a column




Graph implementation usually follows graph representation:
- adjacency list
- adjacency matrix
- edge list
- incidence list
- incidence matrix

## Graph operations

- Predicates
  - hasEdge, given an edge and a graph
  - hasVertex, given a vertex and a graph
  - adjacent
  - incident
- Selectors
  - select vertex given its label
  - select the edge given 2 vertices 
  - select an edge by label
- Getters
  - get all vertices given a graph
  - get all edges given a graph
  - get tail endpoint of an arrow
  - get head endpoint of an arrow
  - get the weigth of an edge, given the edge lebel
  - get the weigth of an edge, given 2 vertices
  - get all adjacent vertices (neighbours), given a vertex
  - get all adjacent edges, given a vertex
  - get endpoints, given an edge as a label



```hs
type Vertex a = a
type Edge   a = (Vertex a, Vertex a)

-- Predicates
hasEdge   :: Edge a               -> Graph a -> Bool
hasVertex :: Vertex a             -> Graph a -> Bool
adjacent  :: Vertex a -> Vertex a -> Graph a -> Bool
incident  :: Edge a   -> Vertex a -> Graph a -> Bool

-- Getters

-- | Get a list of vertices adjacent to the given vertex.
neighbors :: Vertex a -> Graph a -> [Vertex a]
```


Basic operations on a graph `G` include:
- `adjacent G x y`       - check if an edge `(x, y)` exists
- `neighbors G x`        - get all vertices adjacent to vertex `x`
- `addVertex G x`        - add vertex `x`, idempotently
- `removeVertex G x`     - remove vertex `x`, idempotently
- `addEdge G x y z`      - add edge `z` from vertex `x` to `y`, idempotently
- `removeEdge G x y`     - remove edge from vertex `x` to `y`, idempotently
- `getVertexValue G x`   - get value of vertex `x`
- `setVertexValue G x v` - set value of vertex `x` to `v`
- `getEdgeValue G x y`   - get value of edge `(x, y)`
- `setEdgeValue G x y v` - set value of edge `(x, y)` to `v`
