<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 5: Graph Search as a Primitive

This chapter is the empirical heart of the book. It walks through the F# bidirectional ancestor kernel, the experiments that tested it against the Wikipedia category DAG, and what the results revealed about when graph-search-as-primitive is the right primitive.

## The kernel

The artifact is `templates/targets/fsharp_wam/kernel_bidirectional_ancestor.fs.mustache`. It implements a bidirectional A\* search between two nodes on a DAG with an admissible heuristic based on minimum BFS distance to the target. The template fits in roughly 80 lines of F#. The compiled instantiation uses LightningDB cursors to traverse a disk-resident representation of the graph — no in-memory graph required, so it scales past what fits in RAM.

The kernel's signature, in F# notation:

```fsharp
nativeKernel_bidirectional_ancestor
    (lookupParents : int -> int list)
    (lookupChildren : int -> int list)
    (cat : int)
    (root : int)
    (parentCost : float)
    (childCost : float)
    (budget : float)
    : (int * int * int) list
```

It takes the two nodes, two cost weights (`parentCost` and `childCost`), a budget cap, and lookups against parent/child adjacency. It returns a list of paths represented as `(h, n, m)` triples — total hops, parent-direction hops, child-direction hops. Path enumeration is bounded by `budget` and a configurable maximum path count (default 200,000, used as a safety cap on pathological cases).

The kernel does not compute `d_wPow` directly. It produces the *path triples* from which `d_wPow` is computed downstream (the formula is in `examples/prototypes/tree_likeness_index_depth_likeness_probes/fsharp_v1_v3_probe/Program.fs` lines 62–75). The separation is deliberate — the kernel is the expensive part and is reusable across metric variants; the metric formula is cheap and varies per experiment.

## Calibration

The F# probe calibrates against the graph once per LMDB before running queries. The calibration computes `D`, `b_eff`, and the BFS distance map from the root. On a 2.26M-node enwiki LMDB, calibration takes around 8 seconds. After calibration, each per-seed query runs in roughly 0.05 seconds — the calibration is amortised across all seeds.

This split — once-per-graph calibration, once-per-query traversal — is the cost-amortisation pattern that any production kernel needs. An earlier version of the F# probe re-ran the full BFS calibration per query and took 31 seconds per seed on enwiki; reorganising it to pass the precomputed distance map dropped the per-seed cost by ~600×.

The lesson generalises beyond this kernel: *whenever a query depends on a property of the whole graph, lift the property computation out of the per-query loop.* In UnifyWeaver, this is what the compiler tries to do automatically when it has enough information about the access pattern.

## The four budget variants

The crucial parameter in the bidirectional ancestor kernel is `budget`. It caps the total path cost (`N · parentCost + M · childCost`) admissible during enumeration. Four budget choices were tested:

| Variant | Budget formula | What it tests |
|---|---|---|
| V1 | `B = depth(v)` (tightest) | child-direction shortcuts to root, exact depth match |
| V2 | `B = min_carrot_cost(u, v)` | shortcuts between arbitrary topical pairs |
| V3 | `B = max_acyclic_parent_distance(v)` | multi-ancestor averaging behaviour |
| V4 | `B = 15` (fixed) | a standard "loose" budget for production queries |

Each variant probes a different aspect of the graph's topology. V1 asks "are there ways to reach the root cheaper than the BFS minimum?" V2 asks "do non-ancestor pairs have shortcuts?" V3 asks "if we admit looser budgets, do parent-direction shortcuts appear?" V4 is the calibrated production setting.

## What V1 found

On simplewiki Articles, `d_wPow` equals `depth(v) + 1` *exactly* (to floating-point precision) at depths 1–9, across 168 nodes. Standard deviation: zero. Above depth 10, occasional shortcuts emerge.

On enwiki (30× larger), the result generalises: `d_wPow` equals `depth(v) + 1` at depths 1–11, across 220 measured nodes. Shortcuts begin to appear at depth 12, where roughly 20% of nodes show some divergence.

The interpretation: at tight budget and in the child-direction-to-root, the Wikipedia category DAG is *exactly* tree-like at the shallow-to-moderate depths that production queries care about. The "shortcuts are rare" property of the tree-likeness design is empirically confirmed in this regime.

## What V2 and V3 found

V2 (arbitrary pairs, Python prototype) found shortcuts in roughly 39% of measured pairs — and 61% of pairs hit the 5-second enumeration timeout, suggesting the true shortcut rate is higher. Cross-topical shortcuts are *common*, not rare.

V3 (max parent distance, F# enwiki) found that roughly 76% of nodes have *some* parent-direction shortcut: a path that uses parent-direction edges to reach an ancestor of `v` faster than the unique BFS path from that ancestor through `v`. The parent-direction shortcut rate is *higher* than what tree topology would predict.

The combined V1/V2/V3 picture: *the "shortcuts are rare" claim is direction-dependent and budget-dependent*. It holds for the specific configuration "child-direction toward root, at budget = BFS depth". It does not hold for parent-direction queries or for arbitrary-pair queries. The design-note framing in `docs/design/TREE_LIKENESS_INDEX.md` §3.3 was refined accordingly into three sub-claims; the budget-variants report at `docs/reports/depth_likeness_budget_variants.md` records the result.

## The zig-zag geometric series

A specific phenomenon the V3 data revealed: a node `v` reached via a "zig-zag" path — go up to a high ancestor, then down to `v` via a different subtree, then up to a still-higher ancestor — gets a small but non-zero weight contribution. The contributions form a geometric series:

```
Σ 0.76 × (0.04)^n ≈ 0.79
```

where 0.76 is the per-step probability of taking a parent-direction shortcut at all, and 0.04 is the convergence ratio `r` on enwiki. The series converges quickly (the second term is already 3% of the first), so zig-zag chains have a bounded total influence. This is the empirical analog of the theoretical convergence bound `r/(1−r)` from §2.3 of the design note.

The lesson: *the calibration constants directly predict the magnitude of redundant-path contributions to the metric.* This is what makes the metric well-defined on a graph that admits exponentially many paths — most paths contribute exponentially less.

## What graph-search-as-primitive does well

For the metrics this book cares about (`d_wPow`, TLI, anything path-enumeration-based), per-query graph search is:

- **Memory-efficient**: O(budget × branching) frontier, no global state.
- **Cache-friendly when LMDB-backed**: cursor traversal is sequential within each cursor, and LMDB caches hot pages aggressively.
- **Composable across metrics**: the kernel produces path triples; downstream code computes whichever metric it wants.
- **Bounded-cost**: with a budget cap and a max-paths cap, worst-case cost is predictable. The pathological cases (very dense local subgraph at high depth) are explicitly capped.

For these reasons, the F# kernel is genuinely production-quality. The book is not arguing against per-query graph search; it is arguing that *for some other classes of query*, per-query graph search is not the right primitive. Chapter 6 walks through the cost ledger that determines which class a query falls into.

## What graph-search-as-primitive does poorly

Three failure modes:

1. **Repeated queries against an evolving graph.** If a query is asked many times and the graph changes between queries, no work is amortised across calls. A precomputed index that updates incrementally would be cheaper.

2. **Queries that need a property of the whole graph at once.** "What is the diameter?" "What are the top-100 most central nodes?" Each per-query call does a small amount of work; what is needed is one global computation, not many local ones.

3. **Queries whose budget is hard to set in advance.** V2's 61% timeout rate is the canonical example. If the right budget depends on the answer, a fixed-budget kernel either over- or under-spends. Iterative refinement adapts naturally; fixed-budget graph search does not.

Each of these failures has a successful alternative. Chapter 7 walks through what those alternatives look like as compilation strategies.

## Cross-target parity

The F# kernel was validated against an earlier Python prototype on simplewiki. The two produce identical calibration constants (`D = 4.914`, `b_eff = 14.828`) and identical per-seed `d_wPow` values to floating-point precision. The simplewiki parity is the strongest cross-target validation in the project; enwiki F# results are accepted on the strength of that parity — they were not re-run in Python because the Python prototype does not scale to enwiki.

The parity-by-strict-equality story is itself worth noting. The two implementations share *no* code — Python uses dict-of-list adjacency in memory; F# uses LightningDB cursors against disk. The fact that they produce identical numbers is strong evidence that the metric formula and the kernel design are robust to implementation choices. It is also a useful pattern: when porting a metric to a new target, validate against a smaller-graph reference implementation rather than re-derive correctness from scratch.

## Next

Chapter 6: The cost ledger.
