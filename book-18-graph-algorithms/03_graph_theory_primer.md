<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: Graph Theory Primer

This chapter introduces the graph theory the rest of the book needs. Readers familiar with the material can skim — the sections on the friendship paradox (§5) and fixed-point semantics on graphs (§7) are the ones the later chapters return to most often.

## 1. Graphs, directed graphs, DAGs

A **graph** is a pair `G = (V, E)` where `V` is a set of nodes and `E` is a set of edges. A **directed graph** has ordered edges: an edge `(u, v)` goes from `u` to `v`, not the other way around. A **directed acyclic graph** (DAG) has no directed cycles — there is no sequence of edges `v₀ → v₁ → ... → vₖ → v₀`.

The Wikipedia category graph is *mostly* a DAG. Its formal definition admits cycles (`A` is a subcategory of `B` and `B` is a subcategory of `A` is structurally allowed), and a small number occur in practice. UnifyWeaver's ingest pipeline detects and removes them, producing a clean DAG that the algorithms below assume.

For a DAG, we can speak of **ancestors** and **descendants** unambiguously. The ancestors of `v` are the set of nodes from which `v` is reachable; the descendants of `v` are the nodes reachable from `v`. In the categorisation graph, the ancestors of `Category:Physics` include `Category:Natural_sciences`, `Category:Sciences`, and eventually the root `Category:Main_topic_classifications`.

## 2. Reachability and the transitive closure

The **transitive closure** of a graph `G`, written `TC(G)`, is the set of all pairs `(u, v)` such that `v` is reachable from `u` along a directed path of any length. For a DAG, `TC(G)` is itself a DAG (and a partial order).

Computing `TC(G)` is well-defined: run a graph traversal from each node and collect everything reachable. The cost depends on the topology:

- **Best case (sparse graphs):** `O(V · E)`. A linear-time BFS or DFS from each node.
- **Worst case (dense graphs):** `O(V³)` via Floyd–Warshall or matrix multiplication.
- **Space:** the closure can have up to `V²` pairs. For `V = 2.26 million`, that is `~5 × 10¹²` pairs — well past what fits on disk for most purposes.

The cost/space asymmetry is what kills materialisation strategies on large graphs. Even when the time is feasible (a sparse multi-million-node graph takes hours, not years), the storage is not.

## 3. Breadth-first search, bidirectional search, A*

**BFS** explores a graph in layers from a source node: visit all neighbours at distance 1, then all at distance 2, and so on. It computes the *minimum-edge-count* distance from the source to every reachable node, in time `O(V + E)`.

**Bidirectional search** runs two BFS frontiers simultaneously — one from the source, one from the target — and stops when they meet. For a single source/target pair on a uniform-degree graph with branching factor `b` and shortest distance `d`, BFS visits `~b^d` nodes; bidirectional search visits `~2 · b^(d/2)`. The speedup is exponential in `d/2`.

**A\*** is BFS with a heuristic: each node has an estimated distance-to-target, and the search expands nodes in order of *known distance from source + estimated distance to target*. The heuristic must be **admissible** (never overestimate the true distance) for the search to be correct. On a DAG with known node depths (relative to a root), `parent_cost × (depth(v) − depth(target))` is admissible for ancestor queries.

UnifyWeaver's `templates/targets/fsharp_wam/kernel_bidirectional_ancestor.fs.mustache` implements bidirectional A* for the ancestor query, using the depth-difference heuristic. See chapter 5 for the worked example.

## 4. Path enumeration vs path counting

A subtler operation: rather than the *shortest* path, enumerate *all* paths from `u` to `v` up to some budget. The number of such paths is exponential in the budget on most graphs — a complete graph on `n` nodes has `n!` paths between any two nodes.

For real graphs with bounded-depth queries, the count is finite but large. The Wikipedia categorisation graph admits roughly 10⁵ paths from a typical node at depth 10 to the root, within a tight budget. With a relaxed budget, the count blows up by orders of magnitude.

Path enumeration is what the *weighted-power-mean* metric needs (chapter 4 derives why). The trade-off between budget tightness, metric expressiveness, and computational feasibility is the central tension chapter 5 navigates empirically.

## 5. Scale-free graphs and the friendship paradox

A **scale-free graph** has a power-law degree distribution: the probability that a random node has degree `k` is proportional to `k^(−γ)` for some exponent `γ`. Wikipedia categories, the web, social networks, and citation networks are all scale-free with `γ` typically in the range `[2, 3]`.

The mean degree of a scale-free graph can be small while the variance is enormous. A random node has, on average, `E[d]` neighbours, but a random *neighbour* of a random node has on average `E[d²]/E[d]` neighbours — and `E[d²]/E[d]` is much larger than `E[d]` when the degree distribution is heavy-tailed.

This is the **friendship paradox**: your friends have more friends than you do, on average. The mathematical content is: when you sample by following an edge (rather than by picking a node uniformly), you bias toward high-degree nodes proportional to their degree.

For graph algorithms, the friendship paradox has an important consequence: the **effective branching factor** an algorithm encounters during traversal is not `E[d]` but `E[d²]/E[d]`. On the Wikipedia category graph, the naive average out-degree might be 3 or 4, but the effective branching factor during traversal can be 15 or more.

The corrected branching factor, called `b_eff` in this book and in the design docs, governs convergence rates of iterative methods (chapter 7) and is the parameter against which the weighted-power-mean metric is calibrated (chapter 4).

## 6. Cohen–Havlin and ultra-small-worlds

For scale-free graphs with `γ < 3`, Cohen and Havlin (2003) proved that the typical shortest-path distance grows as `O(log log N)` — *doubly* logarithmic in the graph size. These are *ultra-small-world* graphs, in contrast to the merely *small-world* graphs (`O(log N)` distance) of the Watts–Strogatz model.

The English Wikipedia category DAG measured `γ ≈ 2.5` and an effective average shortest distance from `Main_topic_classifications` to a random descendant of about 5–8 edges. The Simple English version measured similarly. Both are ultra-small-worlds.

The practical impact: graph search converges quickly on these graphs. A bidirectional BFS reaches its meeting point in roughly `(log log N) / 2` layers, which for `N = 10⁶` is around 2–3 layers. The per-query cost is bounded by something modest.

The flip side: the *number* of distinct paths between two nodes is huge precisely because the graph is densely connected at short distances. Path-counting metrics like `d_wPow` have to grapple with this — chapter 4 returns to it.

## 7. Fixed-point semantics on graphs

A **fixed point** of a function `f` is a value `x` such that `f(x) = x`. For graphs, the relevant function is one that takes a relation (a subset of `V × V`) and returns an enriched relation. The transitive closure can be expressed as the least fixed point of:

```
TC(R) = R ∪ { (u, w) | ∃ v: (u, v) ∈ R ∧ (v, w) ∈ R }
```

Starting from `R₀ = E` (the edge set) and iterating `R_{i+1} = TC(R_i)`, the sequence converges to the transitive closure. Tarski's theorem guarantees that for monotone functions on a complete lattice (and the subset lattice on `V × V` is one), a least fixed point exists and is reached by iteration.

This is the *operational* basis for bottom-up Datalog evaluation. The Datalog program `tc(X, Y) :- edge(X, Y). tc(X, Z) :- tc(X, Y), edge(Y, Z).` defines `tc` as exactly the least fixed point of the rule above. The Datalog engine iterates until no new tuples are added.

Two practical observations:

- The number of iterations to convergence on a DAG is bounded by the longest path length — `~log log N` on ultra-small-world graphs, much less than `N`.
- Each iteration is itself a (possibly expensive) join. The cost of one iteration is `O(|R_i| · max-fanout)` in the worst case. For sparse graphs, this is manageable; for dense ones, less so.

The combination — few iterations, but each one non-trivial — is exactly what differs from naive transitive-closure materialisation. Chapter 7 unpacks when this trade is favourable.

## 8. Metric distortion

A **metric** on a graph is a function `d: V × V → ℝ_{≥0}` satisfying the triangle inequality, symmetry, and `d(x, x) = 0`. The shortest-path distance is one such metric. So is the average path length, the resistance distance, the commute distance, the weighted-power-mean — there are many.

Different metrics agree on tree-like graphs and diverge on non-tree-like ones. For a tree, the shortest path between two nodes is unique, so any "average over paths" metric collapses to the single available number. For a DAG with redundant paths, the metrics diverge — and the size of the divergence is itself a property of the graph topology.

The **tree-likeness index** (defined formally in `docs/design/TREE_LIKENESS_INDEX.md`) is a measure of how much the weighted-power-mean metric `d_wPow` diverges from the shortest-path metric on a given graph. A tree has tree-likeness index 0. A graph with substantial redundant connectivity has positive tree-likeness index; the larger it is, the further `d_wPow` strays from BFS depth.

Chapter 4 unpacks the definition. Chapter 5 reports what we measured on Wikipedia.

## 9. What this primer didn't cover

Several topics are beyond this primer because the book does not return to them:

- **Spectral graph theory** (eigenvalues of the adjacency matrix, mixing times of random walks). Relevant for the convergence rate of PageRank-style iterative methods but not for the path-enumeration metrics this book focuses on.
- **Random graph models** (Erdős–Rényi, configuration model). Useful as comparison points for scale-free graphs but not directly used in UnifyWeaver's compilation choices.
- **Graph minors and treewidth**. A different formal handle on "tree-like" structure; not currently used in UnifyWeaver.
- **Graph isomorphism, subgraph matching**. UnifyWeaver does query *over* a graph, not *between* graphs.

References to standard textbooks (Diestel, Bollobás, Newman) are appropriate if the reader wants depth in any of these directions. This book proceeds.

## Next

Chapter 4: What metrics on graphs mean.
