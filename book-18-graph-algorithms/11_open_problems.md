<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 11: Open Problems

The book ends with the gaps. Some are forward-direction work items for UnifyWeaver itself; some are empirical questions the tree-likeness work raised but did not settle; some are conceptual issues that the book has stated without resolving. They are gathered here so a future reader (or the book's author re-reading it a year from now) can see at a glance what remains.

The chapter is deliberately a list rather than a discussion. Each item is a pointer to where the real work has to happen — code, experiments, or further design — not a place for the book to make new arguments.

## Empirical questions

### Tree-likeness on non-Wikipedia graphs

All measured tree-likeness data so far is from Wikipedia categorisation. The hypothesis "well-organised hierarchies are statistically tree-like in the `d_wPow` sense" is suggestive but not proven beyond this dataset. Concrete open work:

- Apply the calibration and probe machinery to a synthetic DAG with known structural properties — varying the redundant-connectivity rate, observing the resulting tree-likeness index, validating that the empirical TLI matches the theoretical prediction.
- Repeat the measurement on a non-categorisation hierarchy: a citation network, a corporate org chart, a software dependency graph. Does tree-likeness depend on the *kind* of hierarchy, or only on its degree distribution?
- Test on a graph deliberately *not* tree-like (e.g. a small-world social graph). Confirm the TLI is high and the metric diverges from BFS distance as predicted.

(Tracked as project task #10.)

### Routing-correction redundancy under topical scoping

A specific finding from the design-note work: the routing-correction term in the weighted-power-mean formula appears to be partially redundant when the query is scoped to a single topical subgraph. The hypothesis: under topical scoping, the parent-direction shortcuts that the routing correction accounts for are already captured by the local subgraph's structure, making the explicit correction term double-count.

Open work: an experiment that measures `d_wPow` with and without the routing correction on a topically-scoped subgraph and compares against a synthetic ground truth. The F# kernel infrastructure to do this experiment is now in place (since task #17 completed).

(Tracked as project task #14.)

### V2 (arbitrary-pair) on enwiki

The Python V2 prototype timed out on 61% of measured pairs, suggesting the true arbitrary-pair shortcut rate is higher than the measured 39%. The F# kernel's A* heuristic assumes the target is an ancestor of the source, which it is not for V2. A generalised heuristic — one that works for arbitrary target nodes, not just ancestors — would unblock V2 on enwiki and produce a tight measurement of arbitrary-pair shortcut rates.

Open work: design and implement an admissible heuristic for arbitrary-target queries on the categorisation DAG. The heuristic should not require precomputing all-pairs distances (infeasible at enwiki scale) but should be tighter than "zero", which degrades A\* to plain BFS.

### Distributional recurrence parity for constrained path metrics

Chapter 7's difference-equation pivot is exact for scalar, memoryless graph relations and for unconstrained convergent numeric recurrences. It is not automatically exact for constrained path metrics. Once the query has a `max_depth`, weighted budget, or direction-dependent step cost, the node state is a finite distribution over path statistics — length for parent-only paths, `(parent_hops, child_hops)` for the F# bidirectional kernel — and the final metric is a functional of that distribution.

Open work: build a parity harness that compares three implementations on synthetic DAGs with 10-100 nodes (covering trees, near-trees, and graphs with a few shortcuts) and on a sample of about 50-100 Wikipedia category nodes from both above and below the main-topic layer:

- the F# exact search aggregate;
- the finite distributional recurrence with the same constraints encoded in the state;
- the parent-distribution approximation used as an initializer or deep-region substitute.

Expected result: the finite distributional recurrence with full constraint state should match the F# exact aggregate exactly on the fixtures. The parent-distribution approximation should diverge most at high-degree near-root nodes, where shortcuts are common, and converge toward the exact result in deeper tree-like regions.

Surprising result: if the distributional recurrence fails to match F# even with full constraint state, the discrepancy indicates a missing constraint dimension rather than a performance problem.

The immediate question is semantic: identify exactly which constraints must be present in the recurrence state for equality, and measure when the parent-distribution approximation is close enough to use below the main-topic layer.

## Compilation infrastructure

### Cost-model integration with ingest decisions

The cost model (`src/unifyweaver/core/cost_model.pl`) currently informs per-query strategy selection. It does not currently inform ingest-time decisions: whether to build the proper LMDB schema or the flat one, which indices to materialise during ingest, what auxiliary structures (depth maps, calibration constants) to precompute. These decisions are currently made by hand outside the compiler.

Open work: extend the cost model so that an algorithm-manifest declaration can drive the ingest pipeline, not just the per-query code generation. This is the right place to make the decision "for this workload, precompute the calibration constants during ingest and store them in the LMDB as a side-table".

### Pattern detection for transitive closure

Chapter 10 described pattern detection by rule as substantially open. The most concrete next step: hand-write a recogniser for transitive-closure-style left-recursive rules, with a strategy selector that picks bottom-up semi-naive iteration when the cost model predicts iteration to be cheaper than per-query graph search. This is one well-scoped piece of work, not a research programme.

### Optimisation-manifest expressiveness gaps

The list in chapter 9 of unsupported but plannable hints — per-call materialisation thresholds, hybrid strategies, per-query-shape index selection, convergence tolerance, metric-specific hints — represents the next round of optimisation-manifest extensions. Each is independently work-itemisable.

### Cross-target parity at scale

The simplewiki Python/F# parity validation is the strongest cross-target check in the project, but it is on a single graph at a small scale. Parity between F# and Rust (or F# and Haskell) on the same simplewiki data, then extending to enwiki, would validate the metric formula's robustness across more implementations. This is straightforward but unglamorous engineering work; no algorithmic insight required.

### Pattern detection composition

The cross-predicate optimisation problem (chapter 10 §composition) — given a program of ten predicates with three strategies each, pick the best aggregate combination — is open. The heuristic search infrastructure (local greedy + global iteration) is a standard technique; UnifyWeaver does not have it. The implementation is a meaningful piece of work; the design is largely known.

## Conceptual gaps the book did not close

### The metric-on-graphs theory

Chapter 4 §9 noted that "metric on a graph" is not a settled concept. The book picked `d_wPow` because it is the one with the tightest connection to UnifyWeaver's compilation strategy. Other metrics (resistance distance, commute distance, personalised PageRank) have well-developed mathematical foundations and connect differently to compilation. A more comprehensive treatment of the design space would compare them systematically and articulate when each is the right metric to ask for.

This is a research-grade chapter waiting to be written. The book does not promise it.

### The boundary between declarative and procedural

Chapter 8 argued that compiler-emitted procedural code does not violate the declarative contract, *given* the two properties (readable as a statement; redirectable without rewrite). The argument is sound but the formal definition of "redirectable" is informal. A user-visible procedural choice that the user cannot redirect from the optimisation manifest *is* a violation of the contract — but the book does not give a formal way to identify such violations.

A more rigorous treatment would define the manifest's expressive class formally and prove that every strategy reachable by the compiler is also reachable from the manifest. The current state is that the two are kept aligned by hand: when a new strategy is added, the manifest is extended to expose it. This is fine in practice but is not a verified property.

### Pattern detection vs user-provided hints, when they conflict

If the compiler's pattern detection picks one strategy and the user's optimisation manifest specifies another, which wins? The book did not address this. The conservative answer is "user wins" — the manifest is the user's explicit declaration and the compiler's inference is a default. The aggressive answer is "compiler wins when its confidence is high" — there are cases where the user's manifest is stale (e.g. the workload changed) and the inferred strategy is better.

The actual decision should probably be: user always wins, but the compiler emits a diagnostic when it would have chosen differently. This is a small design decision that has not been made.

## How this book should evolve

The book itself is a forward-direction artifact. Three kinds of updates are anticipated:

**Code-driven updates.** As pattern detection, cost-model integration, and manifest expressiveness gaps close, the relevant chapters (especially 9 and 10) need updating. The book is written to make these updates obvious — each open item is named with enough specificity that closing it produces a concrete chapter edit.

**Empirical updates.** As tree-likeness measurements extend to new graphs (task #10), the chapters 4–5 empirical-anchor numbers and discussion should expand. The book's structure separates the theoretical claim (chapters 3–4) from the empirical evidence (chapter 5), so empirical updates land in a known place.

**Refinement-driven updates.** As the conceptual gaps above are addressed — the metric-on-graphs comparison, the manifest's formal expressive class, the user-vs-compiler conflict resolution — the corresponding sections of chapters 4, 8, and 10 should be revised. Some of these may grow into new chapters; the current count of 11 is not a commitment.

The book treats itself as a living document. The fact that you are reading the first version means many of the items in this chapter are still open. The fact that there is a list at all means future versions can be measured against it.

## Closing

The book started with a naive premise: graph queries are just graph search; that is the whole story. It ended at a much richer picture: the choice of *what* the query asks for (metric, scope, budget) interacts non-trivially with the choice of *how* the compiler implements it, and a declarative-to-imperative compiler has both the responsibility and the opportunity to mediate the choice well.

UnifyWeaver is partway to mediating it well. The infrastructure for declaring algorithms and optimisations separately exists. The bidirectional kernel template exists. The recursion-pattern classifier exists. The pieces are most of what is needed; wiring them together — and extending them where chapter 9 and 10 flag — is the work that remains.

The work also remains for the book. This first version is the starting point. Future versions, written after the open items above are closed, will say different things in different places — and the chapters most likely to need rewriting are exactly the ones (9, 10, 11) where the present version is most explicit about what is open. That is the right shape for a book that is also a roadmap.
