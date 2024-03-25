# Algebraic graphs in Haskell

In math, a graph is defined as a pair, `G = (V, E)`, where `V` is a set of vertices, and `E` a set of edges, with the *graph invariant* `E ⊆ V⨯V`.

This definition may be directly translated to Haskell, with vertices and edges as lists or `Set`:

```hs
data G a = G { vertices :: [a], edges :: [(a, a)] }
```

Either way, the problem is that these representations allows non-graphs to be defined, e.g. `G [1] [(1,2)]`. This goes against the principle motto of FP:
>Define types that encode invariants making invalid states unrepresentable!

Algebraic graphs have 4 core primitive ops for graph construction:

```hs
data Graph a
  = Empty
  | Vertex a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)
```

In brief
1. `Empty` constructs the empty graph, `ε`
2. `Vertex` constructs a single-vertex graph, labelled with typed `a`
3. `Overlay` unites two graphs
4. `Connect` combines two graphs creating new edges

The *empty* and *vertex* operations create new graph directly, while *overlay* and *connect* combine existing graphs. To construct larger graphs from the two primitives, we use two binary operations `+` and `→`.


1. `Empty`
- the empty graph 
- **The empty graph**, `g = (G, V)` thus `ε = (∅, ∅)`.
- the *empty* operation creates the empty graph 
- `Empty` ctor creates the empty graph
- the empty operation creates the empty graph
- the empty graph is denoted by `ε` and defined as `ε = (∅, ∅)`
- empty is not a terribly useful by itself however
- empty is the *identity of connect* (one of the axioms)
  `ε → x = x = ε → x`
- empty is also the *identity of overlay* (derivable theorem)
  `ε + x = x = ε + x`

2. `Vertex`
- a singleton graph
- **A singleton graph** is a graph with a single vertex, e.g. `Vertex 1`, yielding graph `g₁ = (G, V)` thus `g₁ = ({1}, ∅)`.
- *vertex* operation creates a graph with a single vertex
- `Vertex 1` creates a singleton graph labelled with `Int`


3. Overlay (+)
- overlay (+): `(V₁, E₁) + (V₂, E₂) = (V₁ ⋃ V₂, E₁ ⋃ E₂)`
- the *overlay* operation has properties similar to addition
- thus it is also denoted with the `+` symbol.
- the overlay of two graphs comprises the union of their vertices and edges.


4. Connect (⨯ or →)
- connect (→): `(V₁, E₁) → (V₂, E₂) = (V₁ ⋃ V₂, E₁ ⋃ E₂ ⋃ V₁ ⨯ V₂)`
- connect has properties similar to mult
- thus it's symbolized by `⨯`, or with a more intuitive symbol, `→`
- connect acts a lot like overlay, except that it also creates new edges: 
  from each vertex in the lhs arg to each vertex in the rhs arg
- in fact, **only connect can create new edges**
- the connection of two graphs comprises the union of their vertices and edges (like overlay), plus all the edges in `V₁ ⨯ V₂`; more precisely, the last part means that when we connect two graphs, an edge is added from each vertex of the left-hand arg to each vertex of the right-hand arg (unless it already exists). The *connect is the only source of edges* when constructing graphs.



The *connect* acts a lot like overlay, except that it also creates new edges: from each vertex in the lhs arg to each vertex in the rhs arg:

```
            g1 → g2
= (  V₁ , E₁ ) → ( V₂  , E₂ )
= ({1,2}, {…}) → ({2,3}, {…})               -- {…} elided edges
= ( { 1,2,3 }                               -- new set V
  , { (1,2), (1,3), (2,2), (2,3) }          -- new set E as (at least) V₁⨯V₂
  )
```



The `empty` is the left and right identity of connect:
- left identity of connect:  `ε → x = x`
- right identity of connect: `x → ε = x`
- total identity of connect: `ε → x = x = ε → x`

- precedence: _overlay < connect_

The identity of *connect* is the empty graph, `ε`, which is made possible due to the cancellative property of the empty set when used as either operand in the Cartesian product operation between two sets: `A ⨯ ∅ = ∅ = ∅ ⨯ A`. That is, since overlay doesn't produce any new edges, and since connect is the same as overlay except that it adds new edges as prescribed by the last part of its definition, i.e. `… ⋃ V₁ ⨯ V₂`, when either graph is empty, then its set of vertices is `∅`, so the last part of the definition - which only has the power of adding new edges - actually evaluates to `∅`, meaning no edges are added.




The definitions of overlay and connect coincide with those of *graph union* and *graph join*, respectively; however the arguments of union and join are typically assumed to have *disjoint sets of vertices*. We make no such assumptions, hence our definitions are total: any graphs can be composed using overlay and connect.



## Axioms

The *overlay* and *connect* operations have two important properties:
1. closed (total) on the set of graphs
2. can construct any graph starting from the empty and single-vertex graphs

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
