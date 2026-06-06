<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 7: The Difference-Equation Pivot

The previous chapter identified regimes where neither pure per-query graph search nor pure materialisation is right. This chapter is about the alternative: compile the query as an iterative refinement — a difference equation that converges to the answer over successive passes.

## What a difference equation on a graph is

A difference equation on a graph is a rule that takes the *current* approximation to an answer relation and produces the *next* approximation by combining the current state with the graph's edge structure. Iterated to convergence (or to a budget), the equation produces the final answer.

The canonical example is bottom-up transitive closure:

```
TC_{i+1}(x, z) := TC_i(x, z) ∨ ∃ y: TC_i(x, y) ∧ edge(y, z)
```

Starting from `TC_0 = edge`, the iteration converges when no new tuples are added. For a DAG, convergence takes at most `diameter` iterations.

This is a difference equation in the discrete-mathematics sense: a recurrence that defines the value at step `i+1` from the value at step `i`. It is also a *fixed-point computation* in the sense of chapter 3 §7: the converged value is a fixed point of the rule.

The two views are equivalent — a difference equation on a graph relation, iterated to convergence, *is* the least fixed point of the underlying rule. The "difference equation" framing emphasises that each step is incremental; the "fixed point" framing emphasises that the converged value has a clean denotational semantics.

## Why the difference-equation framing matters

Three reasons the framing pays off in practice.

**First**, it is incremental. After step `i`, the partial answer `TC_i` is meaningful — it is the set of pairs `(u, v)` connected by a path of length `≤ i`. If a query needs only short-path connections, the iteration can stop early. The fixed-point view, by contrast, treats only the converged value as meaningful; the intermediates are just stepping stones.

**Second**, it is computable incrementally on a changing graph. If an edge is added, the rule applies to the new edge once and propagates only the new derivations. Full re-evaluation is not required. The fixed-point view does not naturally accommodate updates.

**Third**, it admits early termination on convergence. The rule "stop iterating when no new tuples are added" is a standard semi-naive Datalog optimisation, but it generalises to any monotone difference equation: a step that adds nothing terminates the loop.

These three properties together make difference equations the right compilation target for a large class of graph queries — those where the answer is a relation whose size grows monotonically with iteration count.

## When the difference equation beats graph search

Three regimes.

**Regime A: queries that need a global property.** "Compute `d_BFS(root, v)` for every `v`" is V graph searches; or it is one BFS rooted at `root`, which is one pass of the difference equation `dist_{i+1}(v) = min(dist_i(v), dist_i(parent of v) + 1)`. The single-pass formulation is faster by a factor of V.

**Regime B: queries where the budget depends on the answer.** V2's 61% timeout rate (chapter 5) is the example. The difference-equation formulation computes pairwise reachability outward in layers; the answer at layer `k` is "all pairs reachable in `≤ k` edges". The user can ask for the smallest layer that contains the desired pair without committing to a budget in advance.

**Regime C: queries against an evolving graph.** Daily Wikipedia category updates are small relative to the total graph. Recomputing `TC` from scratch per day is wasteful. Maintaining `TC` incrementally — adding the new edges and propagating their consequences via one or two difference-equation passes — is much cheaper.

In all three, the iteration count is bounded by the *diameter* of the graph, which for an ultra-small-world graph (chapter 3 §6) is `O(log log N)`. The total work is *iterations × per-iteration cost*, and the per-iteration cost is a single join against the current relation. For sparse graphs, this is feasible at multi-million-node scale.

## When the difference equation loses

The pivot is not universal.

A *single* per-query lookup, where the answer is a single number for a single pair, is not worth a global iteration. Per-query graph search wins. The kernel walked through in chapter 5 is the right compilation target for this regime.

A query that requires the full materialised `TC` to be available — for example, *"how many ancestors does each node have on average?"* — needs the answer relation fully built. The difference equation gets you there but at full materialisation cost. If the underlying graph is too large, no strategy is feasible; the question must be reformulated.

A query whose iteration does not converge, or converges only after `O(N)` iterations, is also bad. Graphs with high diameter, graphs with very high effective branching factor, graphs where the difference equation amplifies error at each step — all break the assumed properties. Most real graphs the book cares about (ultra-small-world, sparse, monotone rules) avoid these failures, but the framework's applicability is not unconditional.

## Connection to UnifyWeaver's compilation targets

UnifyWeaver has multiple compilation strategies that correspond to the difference-equation view, with varying levels of integration:

- The **C# Query Runtime** target (covered in `book-03-csharp-target`) implements explicit fixed-point evaluation: a Prolog rule is lowered to a C# loop that iterates the rule application until no new tuples are added. This is the most direct realisation of the difference-equation compilation strategy in the codebase.

- The **Python generator** target lowers recursive predicates to lazy generators that produce tuples on demand. The generator emits one tuple per iteration of the underlying difference equation. This gives the user control over how much of the iteration to run.

- The **Prolog target** with bottom-up evaluation (where available) compiles to Datalog-style semi-naive iteration directly.

The **principal stream architecture** (the default for Bash, AWK, Go, Rust targets) does *not* implement difference-equation evaluation — it lowers predicates to one-shot forward-pass streams. Difference-equation queries on these targets require explicit user code outside the principal architecture.

This is one of the gaps the forward direction wants to close: the choice between principal-stream and fixed-point compilation should be made by the compiler from the optimisation manifest, not by the user choosing a target. As of writing, the C# target is the one that does this most cleanly.

## Convergence rate as a first-class concern

The convergence rate of a difference equation determines whether it is practically useful. Two factors govern the rate:

- **Graph diameter** — the upper bound on the number of iterations needed. For ultra-small-world graphs, this is `O(log log N)`, often single-digit for graphs of practical interest.
- **Per-iteration size growth** — the rate at which `|R_i|` grows. If each iteration doubles the relation size, the iteration is dominated by the last few steps. If it grows slowly, early steps dominate.

For transitive closure on a sparse DAG, both factors are favourable. The Wikipedia category DAG converges to `TC` in about 8–12 iterations from the root, with relation size at each step bounded by the cumulative reachable set.

For more general rules — for example, computing the weighted-power-mean metric `d_wPow` as a fixed-point iteration over partial-metric estimates — the convergence rate is harder to characterise. The calibration constant `r` (chapter 4 §calibration) bounds the per-step contribution from longer paths to `r/(1−r)`. With `r ≈ 0.04` on enwiki, this bound is roughly 4% — small, so a few iterations suffice. With `r ≈ 0.16` on simplewiki, the bound is roughly 19% — still manageable, but slower convergence.

The connection between graph structure (`b_eff`, `D`, `r`) and difference-equation convergence is the *empirical* anchor that makes the pivot tractable. Without the bound on `r`, an arbitrary difference equation might converge slowly or not at all. With the bound, the user can predict that "this query will be answerable within `~10` iterations of the rule against a fixed-up-front budget."

## Hybrid strategies

The pivot is not absolute. Many queries benefit from a hybrid: use graph search to seed an initial answer relation, then use difference-equation iteration to refine.

A concrete example: compute `d_wPow(v, root)` for all `v`. The graph-search approach is V kernel calls, each doing budgeted path enumeration. The difference-equation approach iterates a rule `d_wPow_{i+1}(v) = combine(d_wPow_i(parents of v))` until convergence — but the initial values matter, and seeding from a quick BFS gives a much better starting point than seeding from zero.

The hybrid pattern is: graph search produces a fast approximate answer; difference equation iterates to refine. This is similar to the seed-then-relax pattern in physics simulations (FMG, MGRIT) and in optimisation (warm-started SGD). UnifyWeaver does not currently express hybrid strategies as first-class compilation targets; they would have to be expressed by the user composing two predicates. The forward direction (chapter 10) considers what it would take to recognise and compile hybrids directly.

## The shift in what "declarative" means

Embracing difference-equation compilation changes the user's mental model of what their declarative spec does. In the per-query graph-search regime, the spec is "one call returns one answer". In the difference-equation regime, the spec is "the eventually-converged value of an iteration, which the runtime computes incrementally and possibly with early termination."

The semantic difference is subtle. The denotational meaning of the spec — the relation it defines, the value it asserts — is the same in both regimes. What differs is the *operational shape* the runtime takes to compute that meaning. If the user thinks of the spec as "the value", both regimes deliver the same value. If the user thinks of the spec as "the way the value is computed", the regimes feel different and the choice between them feels consequential.

The book argues (and chapter 8 unpacks) that the user *should* think of the spec as the value. The choice of operational shape is the compiler's. The user retains the right to redirect the compiler — but not, ideally, by editing the spec.

## Next

Chapter 8: Declarative vs procedural, revisited.
