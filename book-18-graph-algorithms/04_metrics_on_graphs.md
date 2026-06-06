<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: What Metrics on Graphs Mean

A metric assigns a number to a pair of nodes. The choice of metric determines what *answer* a graph query is asking for. This chapter sets out the metrics this book cares about, what each one captures, and how the choice connects to compilation strategy.

## Shortest-path distance

The textbook metric. `d_BFS(u, v)` is the minimum number of edges on any path from `u` to `v`. Computed by breadth-first search in `O(V + E)`.

It is exact, fast to compute, and easy to reason about. It also throws information away. If `v` is reachable from `u` along one single peculiar 1-edge path and also along ten thousand 5-edge paths, the BFS distance is 1. Whether that is the *right* answer depends on what question you wanted to ask.

For "is there *any* connection between `u` and `v`?" — BFS distance is the right answer. For "how strongly are `u` and `v` connected, given the structure of intervening paths?" — it is not.

## Path-counted distance

A natural second metric: instead of the shortest path, count *all* paths and average their lengths somehow. For example, the **mean path length** between `u` and `v` (over paths up to some length budget) is a single number that summarises the connection structure.

Mean path length has problems. It weights all paths equally, which over-counts redundant near-identical paths. It is also expensive to compute exactly — path enumeration is exponential in the budget.

What we actually want is a metric that:

1. Reduces to shortest-path distance on trees (where there *is* only one path).
2. Diverges from shortest-path distance in a *predictable* way on graphs with multiple paths.
3. Weights paths by some notion of likelihood or naturalness, not equally.
4. Can be approximated with a budgeted path enumeration.

The **directionally-weighted power-mean metric** `d_wPow`, defined formally in `docs/design/TREE_LIKENESS_INDEX.md`, satisfies all four. The next sections unpack it informally.

## The weighted-power-mean construction

Given a path `p` from `u` to `v` with `h` total edges, `N` "parent-direction" edges (going from a node to its parent in the DAG), and `M` "child-direction" edges (going downward), assign the path a weight:

```
w(p) = (1 / D)^N · (1 / (b_eff · D))^M
```

where `D` and `b_eff` are calibration constants describing the graph (the average child branching factor and the friendship-paradox-corrected effective branching factor; see chapter 3 §5).

Then compute a power-mean over all paths in the budget:

```
d_wPow(u, v) = ( Σ w(p) · (h(p) + 1)^(−n) / Σ w(p) )^(−1/n)
```

with `n` an exponent (in practice `n = 2`, giving a quadratic-mean-like behaviour).

The construction looks ad-hoc on first reading. It is principled in the following sense:

- The weights `w(p)` correspond to the probability of generating path `p` under a model that locally chooses uniformly among in-direction and out-direction edges, with the friendship-paradox correction applied.
- The power-mean over `(h + 1)^(−n)` produces a single number with the same units as edge count (so it is comparable to BFS depth).
- On a tree, only one path exists per pair, so the metric reduces to that path's length plus one.

The metric, then, is "the depth-equivalent of all the paths, weighted by their model-probability." It says: *if you sampled a random walk from `u` according to this weight model, the typical hop count you'd take to reach `v` is around `d_wPow(u, v)`.*

## Calibration constants: D, b_eff, r

The metric depends on three constants computed from the graph:

- **`D`** — the average child branching factor (mean out-degree on the DAG considered with edges pointing toward descendants).
- **`b_eff`** — the friendship-paradox-corrected effective branching factor: `(E[d_child²]/E[d_child]) / (E[d_parent²]/E[d_parent])`. Larger than `D` on heavy-tailed graphs.
- **`b_eff · D`** — the *combined* convergence parameter that appears in the weight ratio of child-direction vs parent-direction steps.

A fourth constant, the **convergence ratio**, is derived:

```
r = b' / (b_eff · D)
```

where `b'` is the average number of parent-direction edges per intermediate node along typical paths. When `r < 1`, the geometric series of zig-zag path weights converges, and `d_wPow` is well-defined.

Measured values on Wikipedia:

| Graph | `D` | `b_eff` | `b_eff · D` | `r` (estimated) |
|---|---|---|---|---|
| simplewiki Articles | ~4.9 | ~14.8 | ~73 | ~0.16 |
| enwiki MTC | ~3.4 | ~22 | ~75 | ~0.04 |

The two wikis have similar `b_eff · D` but very different `r`. The English version has a *smaller* safety margin against the divergence threshold despite being 30× larger — a counter-intuitive finding that chapter 5 returns to.

## Tree-likeness index

The **tree-likeness index** (TLI) compares `d_wPow` against the shortest-path metric `d_BFS`:

```
TLI(G) = E_{u, v}[ d_wPow(u, v) − d_BFS(u, v) ]
```

A tree has `TLI = 0` because the two metrics coincide. A graph with many redundant paths has `TLI > 0`. The size of `TLI` measures the average "shortcut effect" — how much `d_wPow` extends paths beyond the BFS minimum because of weight from longer alternatives.

For Wikipedia categorisation:

- Per-node TLI is *zero* (to floating-point precision) at BFS depths 1–9 on simplewiki and 1–11 on enwiki, when the path enumeration budget equals the BFS depth (`B = depth(v)`).
- TLI becomes positive only at deeper depths or with relaxed budget.

This is the empirical content of the "shortcuts are rare" property — but only along certain directions, and only at tight budgets. Chapter 5 unpacks the budget-dependence carefully.

## Why this matters for compilation strategy

The metric a query asks for governs the algorithm needed to compute it:

- **`d_BFS`** is a single bidirectional or unidirectional BFS. Per-query cost is low; no path enumeration needed. Compiles cleanly to a graph-search kernel.
- **`d_wPow`** requires path enumeration up to a budget. Per-query cost is higher and grows with budget. Compiles to a graph-search kernel with explicit path tracking — or, if the structure permits, to an iterative refinement that *approximates* the path-enumeration result.
- **TLI-aware queries** (those that want a metric calibrated to the graph's tree-likeness) require the calibration constants, which require a one-time scan of the graph. Compiles as a two-phase pipeline: precompute constants, then per-query path enumeration.

The choice of metric is the user's responsibility. The choice of how to compute it is the compiler's. Chapter 9 returns to how the choice is communicated.

## When metrics agree (and when they don't)

The book's central empirical observation: on the *child-direction-to-root* query at *budget = depth(v)*, `d_wPow` and `d_BFS + 1` agree exactly across tens of thousands of measured nodes on two different Wikipedia subgraphs spanning a 30× size difference. The agreement is statistical, not theoretical — but at this sample size, "statistical agreement to floating-point precision across two graphs" is strong evidence.

On the *parent-direction* query — measuring `d_wPow(parent_of(v), v)` for nodes with multiple ancestral routes — the metrics disagree substantially. Roughly 76% of measured nodes show *some* parent-direction shortcut, meaning the weighted-power-mean is smaller than what a naive single-path measurement would predict.

On the *arbitrary-pair* query — measuring `d_wPow(u, v)` for `u` and `v` neither equal nor in ancestor relation — at least 39% of pairs show shortcuts, with the true figure likely higher (the measurement hit enumeration timeouts in the prototype).

The takeaway: "the metrics agree" is direction-dependent. The compilation question — when can we use the cheaper `d_BFS` as a proxy for the more expressive `d_wPow`? — has the answer "for some queries but not others", and the compiler needs to know which.

## Why "metric on a graph" is not a settled concept

This book treats `d_wPow` as the metric of interest because it is the one UnifyWeaver's tree-likeness work focuses on. It is not the only sensible metric:

- **Resistance distance** treats the graph as an electrical circuit and measures the resistance between nodes. Captures the redundant-path effect via parallel resistors. Computationally expensive (matrix inversion).
- **Commute distance** is the expected time for a random walk to go from `u` to `v` and back. Related to resistance distance but with different normalisation.
- **Hitting time** is the expected number of steps for a random walk to first reach `v` starting from `u`. Asymmetric.
- **Personalized PageRank** can be used as a similarity metric. Captures global structure; iterative computation.

Each of these has its own compilation implications. UnifyWeaver does not currently support most of them; `d_wPow` is the one with the tightest theoretical analysis (the calibration constants, the convergence ratio bound) and the cleanest connection to compilation strategy. Other metrics may join the supported set as the project evolves.

## Next

Chapter 5: Graph search as a primitive.
