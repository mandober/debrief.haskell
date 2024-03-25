# Haskell :: Data structures :: Graphs

## Graphs in math

A graph is a pair, `G = (V, E)`, where `V` is a set of vertices, and `E` a set of edges, with an invariant that `E ⊆ V⨯V`.

- Vertices are just (labelled) anchors for edges.
- The most common classification of graphs is undirected vs directed.
- In a digraph, a edge is an arrow - an ordered pair of vertices, `(v1, v2)`, where `v1` is a source (outgoing) vertex and `v2` is a target (incoming) vertex.
- In an undirected graph, an edge is a line - an unordered pair (2-set) of vertices, `{v1, v2}`, where `v1` and `v2` are the edge's endpoints.
- A directed graph may subsume undirected graphs by duplicating each edge then reversing one of them.
- Normally, graphs have maximum of one (in undirected graphs) or maximum two opposite edges (in digraphs) between any two nodes.
- An identity edge has the same vertex as its both endpoints, but such edges are not commonly treated; if they are, that fact is made explicit.

## Representing graphs in math

- {adjacency, incidence} {list, matrix}

Matrix is great as it allows easy derivation of intersting properties, but it takes too much space with non-dense graphs.

One metric of density for undirected graph is `|V| <= |E| <= |V²|`.

Since `E ⊆ V⨯V` then `E = V⨯V` would be a graph with an edge between any two vertices.

The mathematical definition of graphs may be directly translated to Haskell, with vertices and edges as proper sets, or as lists:

```hs
data G a = G { vertices :: Set a, edges :: Set (a, a) }

data G a = G { vertices :: [a], edges :: [(a, a)] }
```

The main problem with this representation is that it allows partial functions that break the graph invariant `E ⊆ V×V`. For example, it is possible to define a graph that breaks the graph invariant by having an edge that refers a non-existing vertex,

```hs
g1 :: G Int
g1 = G [1] [(1,2)]

g2 :: G Int
g2 = G (fromList [1]) (fromList [(1,2)])
```

>We must always strive to make invalid states unrepresentable by defining the data types that encode invariants.

## Other graph data types in Haskell

Perhaps, our data type is just too simplistic, so let's conduct a brief survey by examining the current Haskell's graph libraries, primarily to see how they uphold the graph invariant.

The `containers` library is designed for performance and powers GHC itself. It represents graphs by *adjacency arrays* whose consistency invariant is not statically checked, which can lead to runtime usage errors such as "index out of range". Described in the paper: _"Structuring depth-first search algorithms in Haskell", 1995, King and Launchbury._

Another popular library `fgl` uses the inductive graph representation, but its API also has partial functions, e.g. inserting an edge can fail with the "edge from non-existent vertex" error. Described in the paper: _"Inductive graphs and functional graph algorithms", 2001, Martin Erwig_.

Both `containers` and `fgl` are treasure troves of graph algorithms, but it is easy to make an error when using them. Is is possible to have a safe graph construction interface that encodes the graph invariant?

## Algebraic graphs with Class

In this paper, _"Algebraic Graphs with Class", Functional Pearl, 2017, Andrey Mokhov_, we present such solution.

We present **algebraic graphs** - a new interface for constructing and transforming graphs - more precisely, *graphs with labelled vertices and unlabelled edges*.

We abstract away from graph representation details and characterise graphs by a set of axioms, much like numbers are algebraically characterised by rings (Mac Lane and Birkhoff, 1999).

Our approach is based on the *algebra of parameterised graphs*, a mathematical formalism used in digital circuit design (Mokhov and Khomenko, 2014), which we simplify and adapt to the context of functional programming.

## Algebraic graphs

Algebraic graphs have a safe and minimalistic core of 4 graph construction primitives, as captured by the following data type:

```hs
data Graph a
  = Empty
  | Vertex a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)
```

- `Empty` constructs the empty graph
- `Vertex` constructs a single-vertex graph
- `Overlay` composes two graphs by taking the union of their vertices and edges
- `Connect` is similar to `Overlay` but also creates edges between vertices of the two graphs.

The *overlay* and *connect* operations have two important properties:
1. closed (total) on the set of graphs
2. can construct any graph starting from the empty and single-vertex graphs


For example, to construct a graph with 2 vertices `{1,2}` and 1 edge `(1,2)`

```hs
-- in steps
g1 = Vertex 1
g2 = Vertex 2
g3 = Connect g1 g2

-- at once
g4 = Connect (Vertex 1) (Vertex 2)
```

Malformed graphs, such as `G [1] [(1,2)]`, are not representable.

## Axioms

Laws
1. Overlay (+) is associative
2. Overlay (+) is commutative
3. Connect (⨯ or ->) is associative
4. Empty is identity for (⨯)
5. (⨯) distributes over (+)
6. decomposition: xyz = xy + xz + yz

Theorems:
7. (+) is idempotent
8. Empty is identity for (+)

Extensions:
- Undirected (symmetrical) graphs: `x <-> y = y <-> x`
- Reflexive graphs: `Vertex x = Vertex x -> Vertex x`
- Transitive graphs: (ε ≠ y) => `x -> y -> z = (x -> y) + (y -> z)`
- Combinations:
  - preorder: reflexive + transitive
  - equivalences: undirected + reflexive + transitive
  - etc.


## Contributions

The main goal of this paper is to demonstrate that this core is a safe, flexible and elegant foundation for working with graphs (graphs without labelled edges). Our specific contributions are:

* Compared to existing libraries, algebraic graphs have a smaller core (just 4 graph construction primitives), are more compositional (hence greater code reuse), and have no partial functions (hence fewer opportunities for usage errors).

* The core has a simple mathematical structure fully characterised by a set of axioms. This makes the proposed interface easier for testing and formal verification. We show that the core is complete (any graph can be constructed),
and sound (malformed graphs cannot be constructed)

* Under the basic set of axioms, algebraic graphs correspond to
directed graphs. By extending the algebra with additional axioms, we can represent undirected, reflexive, transitive graphs, their combinations, and hypergraphs. Importantly, the core remains unchanged, which allows us to define highly reusable polymorphic functions on graphs.

* We develop a library for constructing and transforming algebraic graphs:
  http://hackage.haskell.org/package/algebraic-graphs


## Constructing Graphs

We describe the semantics of the primitives using the common representation of graphs by sets of vertices and edges, and then abstract away from this representation by focusing on the laws that these primitives satisfy.

Let `G` be the set of all directed graphs whose vertices come from a fixed universe `𝕍`. As an example, we can think of graphs whose vertices are positive integers. A graph `g ∈ G` can be represented by a pair `g = (V,E)` where `V ⊆ 𝕍` is the set of its vertices and `E ⊆ V×V` is the set of its edges. When `E ⊈ V×V` the pair `(V,E)` is inconsistent and does not correspond to a graph.

When one needs to guarantee the internal consistency of a data structure, the standard solution is to define an abstract interface that encapsulates the data structure and provides a set of safe construction primitives (smart ctors).

**The empty graph**, `g = (G, V)` thus `ε = (∅, ∅)`.

**A singleton graph** is a graph with a single vertex, e.g. `Vertex 1`, yielding graph `g₁ = (G, V)` thus `g₁ = ({1}, ∅)`.

To construct larger graphs from the two primitives, we define binary operations `Overlay` (`+`) and `Connect` (`⨯` or `→`).


The **overlay** (+): `(V₁, E₁) + (V₂, E₂) = (V₁ ⋃ V₂, E₁ ⋃ E₂)`.

The overlay of two graphs comprises the union of their vertices and edges.


The **connect** (→): `(V₁, E₁) → (V₂, E₂) = (V₁ ⋃ V₂, E₁ ⋃ E₂ ⋃ V₁ ⨯ V₂)`.

The connection of two graphs comprises the union of their vertices and edges (like overlay), plus all the edges in `V₁ ⨯ V₂`; more precisely, the last part means that when we connect two graphs, an edge is added from each vertex of the left-hand arg to each vertex of the right-hand arg (unless it already exists). The *connect is the only source of edges* when constructing graphs.

The definitions of overlay and connect coincide with those of *graph union* and *graph join*, respectively; however the arguments of union and join are typically assumed to have *disjoint sets of vertices*. We make no such assumptions, hence our definitions are total: any graphs can be composed using overlay and connect.

The overlay and connect are very similar to addition and multiplication, therefore we give connect a higher precedence to overlay.

The identity of connect is the empty graph, `ε`, which is made possible due to the cancellative property of the empty set when used as either operand in the Cartesian product operation between two sets: `A ⨯ ∅ = ∅ = ∅ ⨯ A`.

That is, since overlay doesn't produce any new edges, and since connect is the same as overlay except that it adds new edges as prescribed by the last part of its definition, i.e. `… ⋃ V₁ ⨯ V₂`, when either graph is empty, then its set of vertices is `∅`, so the last part of the definition - which only has the power of adding new edges - actually evaluates to `∅`, meaning no edges are added.





problem is that the connect operation is defined as `Overlay ⋃ V₁⨯V₂`, but it also says that ε is the identity of connect op. But the Cartesian product with the empty set (as either arg) yields ∅. So, how can `ε` (the empty graph) be identity for connect which acts like overlay (fine) plus it adds new edges by union with the Cartesian product of the vertices of two graphs.

```
g₁ = (V,E) = ({1,2}, {(1,2)}) constructed by e.g. `1 → 2`
g₂ = (V,E) = (∅, ∅) constructed by `Empty`

(V₁, E₁) → (V₂, E₂) = (V₁ ⋃ V₂ , E₁ ⋃ E₂ ⋃ V₁ ⨯ V₂)

g₁ → g₂
g₁ = (V₁, E₁) = ({1,2}, {(1,2)})
g₂ = (V₂, E₂) = (∅, ∅)

g₁ → g₂
= ({1,2}, {(1,2)}) → (∅, ∅)
      (V₁ ⋃ V₂) vertices
    {1,2} ⋃ ∅ = {1,2}

       E₁ ⋃ E₂ ⋃ V₁ ⨯ V₂ (edges)
       E₁ ⋃ E₂ ⋃ …
  {(1,2)} ⋃ ∅ = {(1,2)}
               … V₁ ⨯ V₂
               … {1,2} ⨯ ∅ = ∅ NOT: {1,2}
       E₁ ⋃ E₂ ⋃ V₁ ⨯ V₂ =
       {(1,2)} ⋃ ∅ = {(1,2)}


              g₁ → g₂     =
              g₁ → ε      =
({1,2}, {(1,2)}) → (∅, ∅) =
      (V₁, E₁) → (V₂, E₂) = (V₁ ⋃ V₂ , E₁ ⋃ E₂ ⋃ V₁ ⨯ V₂)
```


## Examples

- `1 + 2` is the graph with two isolated vertices 1 and 2
- `1 → 2` is the graph with an edge between vertices 1 -> 2
- `1 → (2 + 3)` comprises vertices {1,2,3} and edges {(1,2), (1,3)}
- `1 → 1` is the graph with vertex 1 and the self-loop
- `1 → 2 + 2 → 3` is the path graph on vertices {1,2,3}

## Laws

Axioms
1. Overlay (+) is associative
   `x + (y + z)` = `(x + y) + z` = `x + y + z`

2. Overlay (+) is commutative
   `x + y` = `y + x`

3. Connect (⨯ or ->) is associative
   `x → (y → z)` = `(x → y) → z` = `x → y → z`

4. Empty is identity for (⨯)
  - left identity:  `ε → x = x`
  - right identity: `x → ε = x`
  - total identity: `ε → x = x = x → ε` (dot product with ∅!?!)
  - (Cartesian product with ∅ is ∅ - isn't it cancellation and not identity?)

5. (⨯) distributes over (+)
6. decomposition: xyz = xy + xz + yz

Theorems:
7. (+) is idempotent
8. Empty is identity for (+)

Extensions:
- Undirected (symmetrical) graphs: `x <-> y = y <-> x`
- Reflexive graphs: `Vertex x = Vertex x -> Vertex x`
- Transitive graphs: (ε ≠ y) => `x -> y -> z = (x -> y) + (y -> z)`
- Combinations:
  - preorder: reflexive + transitive
  - equivalences: undirected + reflexive + transitive
  - etc.

- `x → (y + z)` = `x → y + x → z` (left distributivity of `→` over `+`)
- `(y + z) → x` = `y → x + z → x` (right distributivity of `→` over `+`)
- `x → y → z` = `x → y + x → z + y → z` (decomposition axiom)
- `ε → x = x` (identity of `→`)
