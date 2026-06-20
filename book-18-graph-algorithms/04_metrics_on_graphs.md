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

### A note on the choice of estimator

The "right" quantity to govern path-count growth — in the sense of spectral graph theory — is the dominant eigenvalue (the Perron–Frobenius eigenvalue) of the graph's adjacency matrix. Computing it directly requires spectral decomposition of a multi-million-node matrix, which is expensive and offers no obvious computational benefit over the degree-distribution approach for the purposes the book cares about.

`b_eff` as computed above is an estimator that is *asymptotically equivalent* to the spectral radius under configuration-model assumptions: for graphs whose degree distribution is captured by `E[d]` and `E[d²]`, and where edges are otherwise random, the spectral radius converges to `E[d²]/E[d]` as the graph grows. The combined `b_eff · D` factor plays the analogous role for the directional case.

The book treats `D` and `b_eff` as *loose* spectral estimators throughout — equivalent-ish to the spectral quantities the theoretically-cleanest formulation would use, without paying the cost of spectral decomposition. Where the book says "the convergence parameter `b_eff · D`", the underlying object is a spectral one; the calculation is a degree-distribution shortcut. Appendix A.1 develops the spectral connection in more detail.

## Statistical homogeneity — when the calibration applies

The calibration constants `D` and `b_eff` are computed as global averages over the whole graph. The metric formula then uses these globals to weight individual paths. This works if the graph is *statistically homogeneous* — if the local behaviour anywhere in the graph resembles the global behaviour. It can fail if the graph has regions with very different local properties.

The theory doc (`docs/design/TREE_LIKENESS_INDEX_THEORY.md` §0.6) formalises homogeneity via three conditions:

- **H1 (local degree distribution).** The conditional degree distribution at a node, given its position in the graph, is consistent with the global degree distribution.
- **H2 (path-length distribution).** The number of intermediate parent-direction hops along typical paths is approximately Poisson, with rate consistent across regions.
- **H3 (convergence ratio).** The per-region convergence ratio `r` is approximately equal to the global `r`.

When all three hold, the global calibration constants predict local path behaviour. When they fail — typically because the graph mixes thematic clusters with different topological characteristics — locally-measured `b_eff` can differ substantially from the global value, and the metric's behaviour on individual queries diverges from the calibration-based prediction.

The empirical observation on Wikipedia: full-graph calibration over the unrestricted categorisation graph is *inhomogeneous* (the design note §4.4 reports a measurable inhomogeneity gap), but *topical* subgraphs — rooted at a thematic node like `Category:Physics` or `Category:Main_topic_classifications` — are homogeneous to within measurement noise. This is why UnifyWeaver's ingest pipeline builds LMDBs rooted at topical nodes rather than over the full categorisation graph.

Appendix B.2 develops the homogeneity framework more thoroughly. Appendix B.4 covers topical scoping as the empirical workaround.

## Tree-likeness index

Tree-likeness is a property of a **(graph, metric) pair**, not of the graph alone. The same graph can be highly tree-like under one metric and substantially non-tree-like under another. The framing matters: in the literature it is tempting to ask "is this graph tree-like?", and the right answer is "tree-like *under which metric?*" The design note's title (*Tree-likeness index: a (graph, metric)-pair statistic*) signals this. Appendix B.1 unpacks the framing.

For the metrics this book cares about — `d_wPow` against the shortest-path metric `d_BFS` — the **tree-likeness index** (TLI) is:

```
TLI(G) = E_{u, v}[ d_wPow(u, v) − d_BFS(u, v) ]
```

A tree has `TLI = 0` because the two metrics coincide. A graph with many redundant paths has `TLI > 0`. The size of `TLI` measures the average "shortcut effect" — how much `d_wPow` extends paths beyond the BFS minimum because of weight from longer alternatives.

For Wikipedia categorisation:

- Per-node TLI is *zero* (to floating-point precision) at BFS depths 1–9 on simplewiki and 1–11 on enwiki, when the path enumeration budget equals the BFS depth (`B = depth(v)`).
- TLI becomes positive only at deeper depths or with relaxed budget.

This is the empirical content of the "shortcuts are rare" property — but only along certain directions, and only at tight budgets. Chapter 5 unpacks the budget-dependence carefully.

### The child-step coherence principle

The operational content of tree-likeness, for queries in the child-to-root direction, is a per-edge statement: *each child-step traversed adds exactly one unit of metric distance toward the root*. Formally, for a (graph, metric) pair with `TLI ≈ 0` in this direction:

```
d_wPow(c, root) ≈ d_wPow(v, root) + 1     for every child c of every node v
```

This is what "the metric behaves like depth" *means*, expressed per-edge instead of per-node. It is the strong statement that the chapter-5 empirical work confirms in the regime where it holds: the per-node TLI being zero to floating-point precision is the aggregate of every individual child-step adding +1 with no deviation.

The principle has three immediate corollaries the rest of the book uses:

1. **Cheap metric computation in the tree-like regime.** Where child-step coherence holds, `d_wPow(v, root) = depth(v) + 1` — the BFS shortest-path distance plus one. The expensive path-enumeration computation is unnecessary; the cheap BFS computation gives the same answer. Chapter 6's cost-ledger analysis depends on this — when tree-likeness holds, the graph-search kernel can use plain BFS instead of weighted-power-mean enumeration.

2. **Local certifiability.** Coherence is a per-edge property. A query that walks downward from root to leaf only needs to verify the local coherence at each step — there is no global integrity check required. Design note §6.3 calls this *per-pair check is cheap* and uses it as a runtime certificate.

3. **Failure mode is interpretable.** When coherence fails — `d_wPow(c, root) < d_wPow(v, root) + 1` — the failure is a *shortcut*: `c` has an alternate route to root not passing through `v`, contributing weight that reduces the metric below the depth-based prediction. The localness of the failure makes the diagnosis straightforward; chapter 6's "drift as a diagnostic" subsection (and design note §6.2) builds on this.

The contrapositive — coherence-failure as a *shortcut* — is why the design note phrases the property as "shortcuts are rare" rather than "metric increments are coherent". Both phrasings describe the same regime; the *shortcut* phrasing is the failure-mode phrasing, the *coherence* phrasing is the success-mode phrasing. Appendix B.3 (weights as path-count normalisers) connects coherence to the underlying weight construction.

## Why this matters for compilation strategy

The metric a query asks for governs the algorithm needed to compute it:

- **`d_BFS`** is a single bidirectional or unidirectional BFS. Per-query cost is low; no path enumeration needed. Compiles cleanly to a graph-search kernel.
- **`d_wPow`** requires path information up to a budget. The exact node state is not usually a single scalar: with a path-length bound it is a finite distribution over path lengths; with direction-dependent costs it is a distribution over `(parent_hops, child_hops)`. A node-local scalar recurrence is exact only after the path constraints are removed or the distribution has been collapsed by an approximation.
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
