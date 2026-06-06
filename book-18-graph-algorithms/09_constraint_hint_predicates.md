<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 9: Constraint-Hint Predicates

This chapter walks through the actual UnifyWeaver mechanisms by which a user declares the *what* of a query and separately declares the *how* of its compilation. The discussion is grounded in real modules: `src/unifyweaver/core/constraint_analyzer.pl`, `src/unifyweaver/core/algorithm_manifest.pl`, and the surrounding cost-model code.

## Two layers of hints

UnifyWeaver supports two layers of user-declared hints, with different semantic content:

1. **Constraint declarations** (managed by `constraint_analyzer.pl`) state properties the *answer* must satisfy: uniqueness, ordering, deduplication strategy. These are properties of the relation, not of the implementation.

2. **Algorithm-and-optimisation declarations** (managed by `algorithm_manifest.pl`) separately declare the *algorithm* (the relation being defined) and the *optimisation* (the operational specification of how to compile it: scan strategy, cache mode, demand filtering, cost-model knobs).

The two layers compose. A predicate can have both a constraint declaration ("the answer is a set with no duplicates") and an algorithm-optimisation declaration ("compile this with stream-mode against the indexed source"). The compiler reads both and picks code-generation strategies that satisfy both.

## Constraint declarations

The constraint analyser exposes:

```prolog
declare_constraint(+Pred, +Constraints).
get_constraints(+Pred, -Constraints).
get_dedup_strategy(+Constraints, -Strategy).
```

A constraint is a property the *answer relation* must satisfy. The two most common are:

- `unique` — no duplicate tuples in the output.
- `unordered` — the output is a set, not a sequence; the runtime is free to reorder.

The defaults (set by `set_default_constraints/1`) are `unique=true, unordered=true`. This matches the relational-algebra default and is the right baseline for most graph queries.

The compiler maps the constraint set to a *deduplication strategy*:

- `sort_u` — sort and uniquify (cheap when the output is small and already nearly-sorted).
- `hash` — hash-based dedup (cheap for large outputs without ordering constraints).
- `none` — no dedup; appropriate when the user knows the source naturally has no duplicates.

The mapping from constraints to strategy is *not* the user's job. The user states the constraint; the compiler picks the strategy. If the user does not care about uniqueness, they declare `unique=false` and the compiler is free to skip dedup entirely.

For graph queries, the constraint layer is where the user declares "I want the set of ancestors, not a multiset" or "I want the closest ancestor, not all of them". The compiler's downstream choices follow from these constraints.

## Algorithm-and-optimisation declarations

The algorithm manifest exposes:

```prolog
decl_algorithm(+Name, +AlgorithmOpts).
decl_algorithm_optimization(+Name, +OptList).
```

The two declarations are *separately addressable*. The user writes the algorithm definition once; the optimisation is then specified by name, possibly with multiple optimisation variants attached. The module documentation states the intent:

> Algorithm here means the Datalog/SQL sense of "a query" — the declarative *what*, not the algorithms-textbook sense of "a step-by-step procedure". The optimization manifest *completes* the definition by saying how the algorithm should be compiled.

This is the clean split the book has been arguing for. The algorithm declaration is the durable specification. The optimisation declaration is the per-deployment, per-workload, possibly-revisable choice of how to compile it.

A schematic example (the actual surface syntax varies; see the module for current details):

```prolog
:- decl_algorithm(ancestor_query, [
    relation(ancestor/2),
    inputs([parent/2]),
    outputs([ancestor/2])
]).

:- decl_algorithm_optimization(ancestor_query, [
    target(fsharp_wam),
    strategy(bidirectional_search),
    budget(15),
    cache_mode(per_session),
    scan_strategy(indexed)
]).
```

The user can attach a second optimisation manifest under a different name (`ancestor_query_batch`, say) with `strategy(fixed_point)` and `target(csharp_query_runtime)`. The algorithm declaration is shared; the two compilations produce different generated code for the same underlying relation.

This is the redirectable-declarative property made concrete. The algorithm survives the change of optimisation; only the manifest is touched.

## What the compiler does with hints

When `compile_predicate_to_<target>` is invoked, the compiler:

1. Resolves the algorithm manifest for the predicate. This gives the relation, inputs, outputs, and any algorithm-level annotations.
2. Resolves the optimisation manifest. If one exists for this algorithm, its options merge with caller-supplied options.
3. Reads the constraint declarations for the predicate. These constrain what dedup/ordering strategies are acceptable.
4. Reads the cost-model declarations. These let the compiler estimate the cost of candidate strategies.
5. Classifies the predicate's recursion pattern (tail, linear, mutual, fixed point, none) via `recursion_pattern.pl` and related modules.
6. Dispatches to a target-specific code generator that emits code respecting all of the above.

The dispatch is multifile in the Prolog sense: each target registers clauses that the analyser calls into. New targets can be added without touching the analyser; new patterns can be added without touching the targets.

For the graph problems this book is about, the most important dispatches are:

- **Stream / principal** for queries that have a clean one-pass implementation.
- **Fixed-point** for queries that compile to iterative refinement (currently best supported by the C# Query Runtime target).
- **Generator** for lazy on-demand computation (Python target).
- **Symbolic WAM** for queries where the symbolic-instruction form is more useful than native lowering.

## Hints that exist; hints that don't yet

Currently supported (or nearly so):

- `unique`, `unordered` — constraint declarations on the answer relation.
- `target(...)` — explicit target choice in the optimisation manifest.
- `scan_strategy(...)` — choice of how to scan the input source.
- `cache_mode(...)` — caching behaviour for repeated queries.
- Recursion-pattern hints implicitly via the pattern detector.

Not currently supported, but planned:

- **Per-call materialisation thresholds.** "If the materialised TC would exceed N entries, fall back to per-query graph search." Requires the cost model to estimate output size in advance, which is not yet implemented for most patterns.
- **Hybrid compilation hints.** "Use graph search to seed, then iterate to refine." Would require the compiler to recognise compositional hybrid strategies, which is closer to chapter 10's territory.
- **Per-query-shape index selection.** "If the query binds `X` and leaves `Y` free, use index A; if reversed, use index B." Partially supported by the binding-state analyser, but not exposed cleanly in the optimisation manifest.
- **Convergence tolerance for difference-equation compilations.** "Stop iterating when the change per step is below ε." Would let users trade accuracy for speed; not currently exposed.
- **Metric-specific hints for graph queries.** "This query computes `d_wPow`; the calibration constants are already available at path X." Would let the compiler skip redundant calibration work.

The list of "planned" is not a roadmap commitment; it is the gap the book observes between what graph-algorithm compilation could express and what UnifyWeaver currently lets the user say. Each is an opportunity for the compiler to do more on the user's behalf.

## How users discover hints

A practical question: how does a user know which hints are available, which apply to their predicate, and what each does?

UnifyWeaver currently relies on three sources:

1. **Documentation** — module docstrings, this book, the design notes under `docs/design/`.
2. **Default behaviour** — sensible defaults mean most users do not need to declare anything.
3. **Diagnostic output** — when compilation fails or produces sub-optimal code, the compiler emits warnings naming the relevant manifest entries.

This is enough for power users but not enough for new users. A more discoverable interface — perhaps a `unifyweaver explain <pred>` command that prints the manifest, the inferred constraints, the chosen strategy, and the available alternatives — is a clear next step. Chapter 11 lists it among the open problems.

## The user's responsibility

In the redirectable-declarative bargain:

- The user is responsible for the *algorithm declaration*: stating what relation they want.
- The user is responsible for the *constraint declarations*: stating properties of the answer.
- The user is *optionally* responsible for the *optimisation declarations*: stating how to compile. Defaults are usually fine; explicit declarations are for cases where the user knows something the compiler does not.
- The user is *not* responsible for the generated code shape, the choice of internal data structures, the loop ordering, or the per-iteration evaluation strategy. These are the compiler's job.

The split is consistent across the modules. A user who treats the algorithm declaration as their main artifact and the optimisation manifest as a tunable side-file is using UnifyWeaver as intended.

## Next

Chapter 10: Pattern detection by rule.
