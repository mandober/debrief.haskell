# Algebraic graphs

In math, a graph is defined as a pair, `G = (V, E)`, where `V` is a set of vertices, and `E` a set of edges, with an invariant that `E ⊆ V ⨯ V`.

- `G = (V, E)`
  - a graph, `G`, is a pair made out of
  - a set of vertices, `V`, and
  - a set of edges, `E`

Default assumptions:
- digraphs are assumed
- no identity arrows (unless stated otherwise)
- edges are not weighted (unless stated otherwise)
- at most 1 arrow from `u` to `v` (otherwise it's a multigraph)
  - any two vertices `u` and `v` may be connected by exactly 0 or 1 arrow
    `∀ u v ∈ V. (u,v) ∈ E ∨ (u,v) ∉ E`
  - that is, there cannot be two arrows `(u,v)`
  - `∀ e1 e2 ∈ E. ∀ v1 v2 ∈ V. e1 = (u,v) ⋀ e2 = (u,v) --> e1 = e2`

- ∀ u u' v v' ∈ V. (u, v) ∈ E ∧ (u', v') ∈ E -> (u,v) = (u',v')  ✘
- ∀ e1 e2 ∈ E. e1 = ()



  - symmetry
    - graphs are directed graphs or digraphs (vs undirected graphs)
    - undirected graphs are symmetric, graphs are asymmetric
    - edges in digraphs are arrows, defined as an ordered pair, `(u,v)`
    - undirected edges are lines, defined as an unordered pair, `{u,v}`
  - reflexivity
    - reflexive (x ~ x) graph   `∀v ∈ V. (v,v) ∈ E`
    - irreflexive (x ≁ x) graph `∀v ∈ V. (v,v) ∉ E`
  - symmetry, x ~ y <=> y ~ x
    - symmetric
  - edges are unweighted (vs weighted)


- `v ∈ V`
  - a vertex, `v`, is a structureless object
  - a vertex or node is repr by a small circle in a graph diagram
  - vertices are usually labelled (by letters or integers)

- `(u,v)`
  - an edge, `(u,v)`, is an ordered pair of vertices
  - where `u` is the source vertex
  - and `v` is the target vertex
  - meaning it `(u,v)` is an arrow `u → v`

- `V` is a set of vertices, `V = {v₀, v₁, v₂, …, vₘ}`
- an edge is an ordered pair of vertices, `e₁ = (vᵢ, vⱼ)` where `vᵢ, vⱼ ∈ V`

- graph invariant, `E ⊆ V ⨯ V`

- set of edges, `E = {(x,y), (v,v), …, (v,v)}`

- set of edges (arrows), `E = {(vᵢ, vⱼ)}`
  - e.g. `E = {(vᵢ, vⱼ)}` 


In a digraph, a edge is called an arrow and it is represented by an ordered pair of vertices, `(v₁, v₂)`, where `v₁` is a source (outgoing) vertex and `v₂` is a target (incoming) vertex.

a graph, `G = (V, E)`
a graph, `G = ({v}, {(v,v)})`
