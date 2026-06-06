<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: The Cost Ledger

A graph query has three cost categories. Most discussions confuse them; this chapter separates them, gives concrete numbers from the Wikipedia work, and identifies the regime where each compilation strategy is the right one.

## The three categories

1. **Ingest cost** — the cost of building a queryable representation of the graph from its source format. Paid once per graph version. Bounded by the source size and the format choice.

2. **Materialisation cost** — the cost of precomputing an answer (or an index, or a derived relation) that future queries will read directly. Paid once, but the answer may need to fit in memory or on disk.

3. **Per-query cost** — the cost of computing an answer when a query arrives, given whatever ingest and materialisation has already happened. Paid once per query.

The three are not interchangeable. They have different units (seconds-of-CPU vs bytes-of-storage), they amortise differently (ingest amortises across all queries; materialisation across all queries that use that index; per-query not at all), and they fail in different ways at scale.

A choice of compilation strategy is, fundamentally, a choice about how to distribute work across these three categories. The same answer can come from any distribution, but the wall-clock and storage costs vary wildly.

## Concrete numbers: Wikipedia categorisation

For the post-fix LMDB build of the categorisation DAG, measured numbers:

| Graph | Nodes | Edges | Ingest time | LMDB size | Per-query (V1 d_wPow) |
|---|---|---|---|---|---|
| simplewiki Articles | ~80k | ~290k | ~5 s | ~30 MB | ~0.005 s |
| enwiki MTC | 2.26M | 6.7M | ~3.5 min | ~600 MB | ~0.05 s |

The ingest costs are tractable. The LMDB sizes are well within commodity hardware limits. The per-query costs are fast enough that interactive use is realistic.

The *materialisation* cost is the elephant in the room. For pairwise `d_wPow`, full materialisation requires `~V²` entries. At `V = 2.26M`, that is `~5 × 10¹²` pairs — at 8 bytes per number, 40 TB of storage. Materialising even 1% of pairs is 400 GB. Materialisation does not fit.

Restricted materialisations help. "Materialise `d_wPow(v, root)` for every `v`" is `V` entries — 18 MB. That fits trivially. But it only answers the root-targeted query; it does not help arbitrary-pair queries (V2). Choosing the right restricted materialisation requires knowing the query distribution in advance.

## When each strategy wins

A simple decision rule, derived from the cost ledger:

- **If queries are few and the graph is small**, per-query graph search wins. No ingest, no materialisation, just run BFS in memory. This is the textbook case.

- **If queries are many but only one shape**, restricted materialisation wins. Precompute the index that fits the shape; serve queries from the index. SQL with an index. Datalog with a fully-evaluated relation.

- **If queries are many and varied shapes against a fixed graph**, per-query graph search against an indexed representation wins. The LMDB-backed F# kernel is this case. Build the LMDB once; serve any per-pair or per-node query in 0.05 seconds.

- **If queries are many, varied shapes, and the graph evolves**, iterative refinement wins. Maintain a partial materialisation; update incrementally; serve queries against the current state. This is the pattern chapter 7 unpacks.

- **If the metric is global** (top-K, percentile, diameter), iterative refinement also wins, because per-query graph search would have to run for every node anyway. One pass beats `V` passes.

The Wikipedia work this book draws from is the third bullet: many varied queries against a fixed graph version. The F# bidirectional kernel is the right choice for this regime, and chapter 5 reports the empirical evidence that it works.

## Where the ledger fails to predict

Three regimes the simple decision rule does not cover.

**Regime 1: queries arrive in bursts with predictable shape.** A research workload that runs 10,000 queries of the same shape, then nothing for an hour, then 10,000 of a different shape, is hard to optimise statically. Materialisation pays off within a burst and wastes storage between bursts. The right answer is dynamic materialisation — build the index when the burst starts, evict it when memory pressure rises. UnifyWeaver does not currently do this; the optimisation manifest is per-compilation, not per-query-burst.

**Regime 2: queries that combine cheap and expensive subqueries.** A single user-facing query might need `d_BFS` (cheap) for one node pair and `d_wPow` (expensive) for another, in the same answer. The decision rule above asks "which strategy for this query?" — but the right answer is *different strategies within the same query*. The optimisation manifest can in principle encode this, but only if the cost analyser sees inside the query.

**Regime 3: queries against a graph that is itself the answer to another query.** Composability. If `Q1` produces a relation `R1` that `Q2` then queries, the cost of `Q2` includes the cost of materialising `R1` (or of recomputing it lazily). This is the SQL-view problem and it does not have a clean general solution.

These regimes are not failures of the framework; they are open problems. Chapter 11 returns to them.

## Why "just precompute TC" doesn't scale

A natural objection: "transitive closure is well-studied; precompute it once and look up `(u, v) ∈ TC` in O(1)." The objection has technical merit on small graphs and fails on large ones.

The first problem is storage. As computed above, `TC` of a 2.26M-node DAG is up to `~5 × 10¹²` pairs. Even in a sparse-encoded form, this is hundreds of GB.

The second problem is time. Computing `TC` from scratch on a sparse DAG is `O(V · E)`. On enwiki at `V = 2.26M`, `E = 6.7M`, this is `~1.5 × 10¹³` operations — hours to days on a single core. Distributed computation reduces wall-clock but not total work.

The third problem is that `TC` is a *boolean* relation. It tells you `v` is reachable from `u`, not how far, not by how many paths, not under what budget. The metrics this book cares about are richer than reachability, so `TC` does not even directly answer the questions.

The fourth problem is incremental maintenance. If the graph changes — Wikipedia adds a category every few seconds — `TC` becomes stale. Incremental TC maintenance is its own research area; the algorithms are not trivial.

For Wikipedia-scale graphs, precomputing TC is the wrong shape. The right shape for transitive-closure questions is bottom-up Datalog evaluation with semi-naive iteration — a fixed-point computation that does not materialise the full closure but evaluates predicates on demand against incrementally-grown derived relations. Chapter 7 unpacks this.

## The ingest pipeline is also a choice

Ingest is not free, and it is not unique. For Wikipedia, the choice of LMDB schema turns out to matter:

- **Flat schema** (one sub-database, `(parent, child)` pairs as raw key-value): the ingester `examples/benchmark/build_articles_subgraph.py` and `mysql_stream_lmdb` runtime binary produce this in `--mode correct`.
- **Proper schema** (separate sub-databases `s2i`, `i2s`, `category_parent`, `category_child`): adds explicit indices for both directions of traversal. The bidirectional kernel needs the `category_child` index for downward lookups; without it, child lookups are O(N).

The choice trades ingest time and disk space for per-query speed. The proper schema costs ~25% more disk and a few seconds extra ingest time, and reduces per-query bidirectional lookups from O(N) to O(1) — orders of magnitude faster at the per-query level.

For a workload of more than a few queries, the trade is obviously favourable. The decision is the kind of thing a cost-aware compiler should make automatically based on the query shape. UnifyWeaver's `src/unifyweaver/core/cost_model.pl` is the module that, in principle, makes such decisions. In practice, the choice is currently made by the data-source ingest tool, not the compiler. This is one of the gaps chapter 11 records.

## The cost of changing the cost ledger

A consequence of the ingest/materialisation/per-query split: changing one's mind about the right distribution is expensive. If you committed to per-query graph search and now want a materialised index, you have to build the index. If you committed to a materialisation and the underlying metric changes, you have to rebuild.

This is the *recompilation cost* — invisible in the ledger until you try to change strategies. The optimisation manifest design (chapter 9) is partly about making the recompilation cost as small as possible: keep the algorithm declaration stable, vary only the manifest, regenerate target code. The ingest representation is still expensive to change, but the compilation strategy on top of it should not be.

The book's general advice: *make the optimisation choice late*. Defer the strategy decision until you have measured the query workload. The compiler should support this by treating the optimisation manifest as a tunable artifact, not as a fixed property of the source.

## Next

Chapter 7: The difference-equation pivot.
