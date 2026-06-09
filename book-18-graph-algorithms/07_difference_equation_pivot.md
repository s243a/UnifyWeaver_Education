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

## Scalar equations vs distributional recurrences

The clean scalar form above is not always the right state space. It works when the value at a node is path-independent (the Markov property on the graph): a node's next value can be computed from the current values of its neighbours alone, without remembering how those values were reached. Shortest distance, reachability, and ordinary transitive closure have that shape.

Budgeted path metrics do not. A query like `d_wPow(v, root)` is not asking for a single node value independent of path history; it is asking for a statistic of the paths from `v` to `root`, subject to a constraint. If the constraint is ordinary path length, the exact finite-horizon state is a count of paths of each length:

```
C_v[L] = number of paths from v to root of exactly length L
```

For parent-only paths on a DAG, the recurrence is simple and exact:

```
C_v[L + 1] = sum over p in parents(v) of C_p[L]
```

This equation is written as a pull recurrence: `v` asks each direct parent for its length-`L` count, then shifts that count by one edge. The equivalent root-down push pass is the same computation on a DAG, but it has different cache, memory, and parallelism behaviour.

In probability form this is a shifted mixture (each parent contributes its own distribution, shifted one hop forward; the weights `q(p | v)` normalise to 1), not a convolution between parent distributions:

```
P_v(L + 1) = sum over p in parents(v) of q(p | v) * P_p(L)
```

A single path chooses one parent, then continues. It does not combine independent paths through multiple parents. The "convolution" is only with the one-edge step kernel; with unit edges that convolution is just a shift.

The F# bidirectional effective-distance kernel has an even richer state. It tracks parent hops and child hops separately, then accepts only paths whose weighted cost fits the budget:

```
C_v[N, M] = mass of paths from v to root
            with N parent hops and M child hops

accepted iff N * parent_cost + M * child_cost <= budget
and, if a separate max_depth is active, N + M <= max_depth
```

Only after this finite distribution is computed can the metric collapse it to a single number by applying the weighted-power-mean formula.

This distinction matters for the rest of the chapter. The "difference equation" framing is still correct, but the equation is sometimes over a distributional state, not a scalar state. If the path constraint is removed and the weighted series converges, the distribution can sometimes be summarised to a single scalar value per node; that scalar may then satisfy a fixed-point equation in the denotational sense. If the path constraint remains, exactness requires carrying the finite path statistic, or else explicitly accepting an approximation.

### Parent-only baselines and metric functionals

The first useful distributional target is parent-only. Compute the exact parent-path distribution from each node to the chosen root before admitting child-direction paths:

```
P_v[S] = mass, count, or probability of parent-only paths from v to root
         whose path statistic is S
```

The statistic `S` is not fixed by the recurrence. It may be hop count, weighted path length, semantic edge cost, or a tuple such as `(parent_hops, child_hops)` once richer path types are admitted. The important point is that the parent-only aggregate is reusable state; it is not itself "the distance."

Different root-nearness metrics are functionals over the same baseline distribution:

```
minimum distance:
  min S with P_v[S] > 0

bounded average distance:
  sum_{S <= B} S * P_v[S] / sum_{S <= B} P_v[S]

bounded reachability mass:
  sum_{S <= B} P_v[S]

tail mass beyond the budget:
  sum_{S > B} P_v[S]

ambiguity / entropy:
  -sum_S P_v[S] log P_v[S]
```

This is why parent-only search should come before child-inclusive search. It provides an exact distributional baseline against which later approximations can be tested. Child-direction search can then be treated as a bounded correction to a chosen functional, not necessarily as a search for a shorter path. For a minimum-distance functional, the correction asks whether child paths create a shorter route to the root. For a bounded-average functional, it asks whether child paths move enough mass under the horizon to change the expected value. For an entropy or ambiguity functional, it asks whether the extra paths materially widen or concentrate the distribution.

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

A constrained path metric can also lose for a more subtle reason: the scalar equation may be the wrong abstraction. If the user's question depends on maximum path length, weighted path budget, or direction-specific hop counts, the compiler needs the distributional state described above. Dropping that state can turn an exact computation into a statistical approximation without saying so. This is acceptable only if the approximation has diagnostics, for example:

- a measured shortcut mass;
- the entropy or width of the parent distribution;
- a sampled parity check against the exact search aggregate.

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

For more general rules — for example, computing the weighted-power-mean metric `d_wPow` by propagating path-length or direction-hop distributions — the convergence rate is harder to characterise. The calibration constant `r` (chapter 4 §calibration) bounds the per-step contribution from longer paths to `r/(1−r)` when the weighted series is treated as an unconstrained tail. With `r ≈ 0.04` on enwiki, this bound is roughly 4% — small, so a few iterations suffice. With `r ≈ 0.16` on simplewiki, the bound is roughly 19% — still manageable, but slower convergence.

This bound applies to the unconstrained weighted series: no `max_depth`, no direction budget. With a finite-horizon constraint in place, the tail beyond the horizon is truncated by definition; the separate question is how much metric error that truncation introduces, which depends on how much mass falls beyond the horizon.

The connection between graph structure (`b_eff`, `D`, `r`) and difference-equation convergence is the *empirical* anchor that makes the pivot tractable. Without the bound on `r`, an arbitrary distributional recurrence might converge slowly or not at all. With the bound, the user can predict that "the tail mass beyond the finite horizon is small enough to ignore" — but only after checking that the finite-horizon constraints the query actually uses have been represented in the state.

## Hybrid strategies

The pivot is not absolute. Many queries benefit from a hybrid: use graph search to seed an initial answer relation, then use difference-equation iteration to refine.

A concrete example: compute `d_wPow(v, root)` for all `v`. The graph-search approach is V kernel calls, each doing budgeted path enumeration. The distributional approach propagates path-length distributions downward from the root, or direction-hop distributions when child hops are admitted. The initial values matter, and seeding from a quick BFS gives a much better starting point than seeding from zero.

For the Wikipedia category graph, the natural hybrid is depth-stratified. Take `Category:Main_topic_classifications` (or a topical root) as the root, compute the minimum parent distance from each node to that root, and use that distance as the notion of "near the root." The stratification depth is itself a plain `d_BFS` computation over the parent-edge graph, so it is cheap and requires no distributional state. Nodes with the fewest parent hops to the root are where the graph is most likely to contain high-degree hubs and cross-topic shortcuts, so they are the best candidates for exact search aggregates. Farther down a topical branch, especially below the main-topic layer, the graph appears more tree-like and the shifted parent-distribution approximation becomes more credible.

One possible hybrid schedule:

```
near root:       exact bounded search aggregates
middle band:     blend exact aggregates with parent-distribution propagation
deep region:     propagate parent distributions only
```

The blend should not be controlled by depth alone. Useful diagnostics include the width or entropy of the parent distribution, the number of direct parents, a local shortcut-mass estimate, and periodic sampled parity checks against the F# search aggregate. If those diagnostics drift above threshold, the exact zone can expand locally. This turns the depth-stratified hybrid into an adaptive statistical compiler pass: one where the compilation strategy for `d_wPow` is chosen node-locally rather than globally. Chapter 10 returns to this kind of choice as a first-class compilation target.

The cutoff is also metric-specific. Exact child-inclusive search is valuable only when it can change the functional the query will actually report: a shorter support point for minimum distance, enough additional under-budget mass to move a bounded average, enough new mass to change reachability confidence, or enough spread to affect an entropy diagnostic. The decision criterion is therefore marginal value per marginal cost for the selected functional, with the parent-only distribution acting as the baseline estimate.

UnifyWeaver does not currently express hybrid strategies as first-class compilation targets; they would have to be expressed by the user composing two predicates. The forward direction (chapter 10) considers what it would take to recognise and compile hybrids directly.

## The shift in what "declarative" means

Embracing difference-equation compilation changes the user's mental model of what their declarative spec does. In the per-query graph-search regime, the spec is "one call returns one answer". In the difference-equation regime, the spec is "the eventually-converged value of an iteration, which the runtime computes incrementally and possibly with early termination."

The semantic difference is subtle. The denotational meaning of the spec — the relation it defines, the value it asserts — is the same in both regimes. What differs is the *operational shape* the runtime takes to compute that meaning. If the user thinks of the spec as "the value", both regimes deliver the same value. If the user thinks of the spec as "the way the value is computed", the regimes feel different and the choice between them feels consequential.

The book argues (and chapter 8 unpacks) that the user *should* think of the spec as the value. The choice of operational shape is the compiler's. The user retains the right to redirect the compiler — but not, ideally, by editing the spec.

## Next

Chapter 8: Declarative vs procedural, revisited.
