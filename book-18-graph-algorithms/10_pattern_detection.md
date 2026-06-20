<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 10: Pattern Detection by Rule

The forward direction. Chapter 9 covered hints the user supplies *explicitly*; this chapter covers the opposite — the compiler reading the surface specification and *inferring* the right compilation strategy without the user saying so.

This is the chapter most aligned with what is not yet fully built. Parts of the infrastructure exist (`src/unifyweaver/core/recursive_kernel_detection.pl`, the recursion-pattern classifier). The full vision — a compiler that detects a graph-search formulation and rewrites it as a difference equation by rule — is partly prototyped and substantially open.

## The vision

A user writes:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

The compiler observes that this is a classic transitive-closure formulation: a base case + a left-recursive rule that joins the base with a recursive call. The compiler infers that the right shape on a large graph is bottom-up semi-naive iteration. It emits code accordingly — a difference-equation loop, not a top-down depth-first traversal.

The user did not say "compile this as a fixed point". They wrote the relation; the compiler picked the shape.

This is what pattern detection by rule means: the compiler has a library of recognisable patterns (transitive closure, shortest path, single-source reachability, etc.) and the strategies that fit each pattern. The user writes the specification in surface Prolog; the compiler reads the specification, classifies it against the library, and emits the strategy.

## What exists today

Three pieces of the infrastructure are in place.

**Recursion-pattern classification.** The modules under `src/unifyweaver/core/` (`recursive_compiler.pl`, `recursive_kernel_detection.pl`, the `recursion/` directory) classify predicates into recursion shapes: tail recursion, linear recursion, mutual recursion, fixed-point, or non-recursive. The classifier looks at the structure of the rule bodies and the call graph among predicates. For each classified pattern, target-specific code generators register multifile clauses that emit the right code.

This is already a form of pattern detection. The pattern catalogue is recursion-shape-based, not graph-algorithm-based, but the architecture generalises.

**Algorithm manifest with separately-stated *what*.** Per chapter 9, the user states the algorithm with `decl_algorithm/2` separately from how to compile it. This means the algorithm declaration is *available to the compiler as data* — the compiler can read it, inspect it, and reason about its shape. The compiler does not yet do much with this beyond reading the optimisation manifest, but the architecture supports more.

**The recurrence-evaluation-strategy selector.** `src/unifyweaver/core/recurrence_evaluation_strategy.pl` is the dispatch hub between recognised patterns and emitted strategies. Inputs: a recurrence term (kernel kind plus per-recurrence properties) and a workload signal set (the classified intent / declared-data / inferred-data tiers from chapter 9). Output: a chosen strategy plus a structured trace that records every step of the classification, cost-model evaluation, and conflict resolution. The trace is exposed to the user — for the F# WAM target the selector's narrative is emitted as a comment block at the top of the generated module, and a short summary is logged to stderr at codegen time.

The selector ships with three concrete cost-model rules (prefer-bidirectional-when-CSR-present being the one used in the prototyped bidirectional path), six conflict-resolution steps (intent-matches, third-option, scope-disambiguation, satisfiability, caller-wins, no-intent), and a pluggable adapter shape so each target can render the trace its own way. This is the piece that closes the loop between chapter 9's hints and the code generator's choice.

All three pieces are necessary for the vision. Without recursion-pattern classification, the compiler cannot tell a transitive-closure rule from an arbitrary recursive rule. Without separately-stated *what*, the compiler does not have a clean place to attach the inferred strategy. Without the selector, the recognised pattern and the classified workload have no shared mechanism to combine into a code-generation choice.

## What is missing

The gap between current and full vision has narrowed. What remains:

- **The fixed-point branch of the selector.** The strategy selector's `fixed_point(...)` modes are structurally present in the API and gated by admissibility checks, but no cost-model rule currently chooses them — every implemented rule routes to a `per_query(...)` strategy. Filling in the fixed-point side requires a second consumer of the selector (the natural candidate is the C# parameterised query runtime, which already realises bottom-up fixed-point iteration); the selector then has somewhere to *send* the recommendation, and the cost-model rules become worth writing. Until then, the selector is half a converter.

- **A second target consumer of the selector.** The F# WAM target is the only call site today. A second consumer (C# query runtime, Haskell WAM, or C WAM) would shake out whether the helper API in `src/unifyweaver/core/recurrence_inputs.pl` has accidentally calcified around F#-specific assumptions. The mock-second-caller test in `tests/core/test_recurrence_inputs_mock_caller.pl` is a stand-in for now; a real consumer is the durable signal.

- **A library of graph-algorithm patterns.** Transitive closure has a recognisable shape; so do shortest-path computations, reachability, strongly connected components, topological sort. The compiler does not yet have explicit pattern templates for these. (The relation-policy module `src/unifyweaver/core/relation_policy.pl` is a candidate place for this catalogue.)

- **Confidence in the classification.** Pattern recognition is a heuristic. Two rules that look identical may have different semantics in subtle ways (cycles in the input, non-monotone updates, etc.). The compiler needs to know how confident the classification is and what to do when confidence is low. The trace structure introduced for the recurrence-evaluation-strategy selector is the natural place to expose this — the selector already records *why* a strategy was chosen; a confidence rating per signal tier is a small addition. One option is to emit the recognised strategy with a fallback to the general-purpose strategy; another is to require the user to confirm.

- **The graph-shape side.** A pattern that should compile as a fixed point on a sparse DAG might compile as something different on a dense graph or one with cycles. The compiler needs to know the graph's shape — which means either the user declares it or the runtime measures it. Neither is currently automatic.

- **ML-based detection.** Discussed below; not currently in scope.

## Concrete prototyped detection: bidirectional ancestor

One specific pattern has been worked out as a prototype: the bidirectional ancestor kernel. The F# template `templates/targets/fsharp_wam/kernel_bidirectional_ancestor.fs.mustache` is what gets emitted when the compiler recognises an ancestor-query pattern targeting the `fsharp_wam` backend with appropriate workload signals.

Auto-selection now works. As of the recurrence-evaluation-strategy implementation, the three components named in earlier drafts of this chapter are wired together end-to-end:

- **The pattern matcher.** `recursive_kernel_detection.pl` recognises ancestor-style left-recursive transitive-closure rules and classifies them as `category_ancestor` kernels.
- **The cost-model rule.** The selector's `prefer_bidirectional_csr_present` rule fires when (a) the recurrence is a `category_ancestor` kernel, (b) the workload has `query_pattern(single_pair)` and `cardinality(large)`, and (c) the data side carries `csr_available(true)` (declared via `csr_path/1` in the optimisation manifest, or inferred when a CSR artifact exists).
- **The strategy selector.** Combines the two: when the rule fires and no conflicting intent is declared, the selector recommends `strategy(per_query(bidirectional))` and the F# WAM target emits the `bidirectional_ancestor` kernel in place of the default `category_ancestor` kernel. A safe-default policy keeps the kernel-kind swap behind `allow_bidirectional_kernel_swap(true)` in the F# WAM options — without the flag, the selector's recommendation is logged but the emission stays as `category_ancestor` (the recommendation is advisory until the consumer opts in).

The auto-select end-to-end test (`tests/core/test_wam_fsharp_strategy_autoselect_e2e.pl`) and the opt-in bidirectional emission test (`tests/core/test_wam_fsharp_bidirectional_e2e.pl`) cover both arms: cost-model says bidirectional and the F# project builds either way; the bidirectional emission produces a runnable .NET binary that the second test exercises against a synthetic Phase-1 LMDB graph.

What is *not* yet validated by the prototype: the analytical justification for choosing bidirectional. The bidirectional kernel internally computes `calibrateGraph` (which produces `b_eff`, `D`, `routing_correction`, and a `min_dist[node]` BFS table used as an A\* lower bound), and the cost-model rule's `prefer_bidirectional_csr_present` clause is informally motivated by the chapter-7 difference-equation framing — the search cost grows as `b_eff^depth` and bidirectional search amortises the depth across two converging frontiers. But the calibration outputs are consumed inside the kernel and never logged at the benchmark level; the synthetic fixtures used in the e2e tests are too small and too tree-like (a depth-1 4-ary tree, `b_eff ≈ 1`) to discriminate between the analytical prediction and any other strategy. A meaningful comparison between the difference-equation prediction and observed search cost will need (a) a real-scale graph with non-trivial `b_eff` (the simplewiki topical subgraph used in chapters 3–4 is the obvious candidate) and (b) instrumentation that exposes the calibration metrics alongside `solutions=` and `query_ms=` in the benchmark output. Both are concrete forward-direction items — the prototype is wired, but it does not yet prove the analytical model.

A second gap worth naming: the bidirectional kernel's cost weights (parent-step cost, child-step cost, budget) are currently hardcoded in the F# WAM dispatch template (`WamRuntime.fs`'s `executeForeign` branch sets `parent=1.0`, `child=3.0`, `budget=10.0`) and the native benchmark loop mirrors them. The selector chooses *which* kernel to emit but does not yet plumb cost-model parameters through to the kernel's runtime weights. Closing this gap is what would let the difference-equation framing actually drive the search.

## Rule-based vs ML-based detection

A design question: should pattern detection be implemented as hand-written rules or as a learned classifier?

The book's position: rule-based, for now. The patterns of interest are well-understood mathematical objects (transitive closure, shortest path, fixed points of monotone rules). They have crisp definitions and small numbers of variations. Rule-based detection is appropriate.

ML-based detection becomes interesting when:

- The pattern space is large and ill-defined (a learned classifier scales better than a hand-written rule library).
- The user writes idiomatic but non-canonical formulations of standard patterns (the classifier can learn the variations from examples).
- The cost model has many parameters and the optimal choice is non-obvious (the classifier can learn the cost-model-to-strategy mapping).

None of these are currently the case in UnifyWeaver. The pattern library is small, the user formulations are typically canonical (because Prolog has a small number of idiomatic ways to write transitive closure), and the cost model is simple. Rule-based is the right starting point.

A long-term option: hybrid. Rule-based for the common cases (cheap and predictable), ML-based as a fallback for unrecognised patterns (gives a guess, possibly wrong). The user is informed which mechanism produced the strategy. This is a fine design but is well beyond the current state.

## What detection should *not* do

Some failures the design should avoid.

**Silently choosing the wrong strategy.** If the compiler detects a pattern with low confidence and picks a strategy that turns out badly, the user has lost the ability to redirect (because they did not know detection happened). The compiler should always emit a diagnostic naming the detected pattern and the chosen strategy.

**Hiding the user's spec from the user.** A compiler that aggressively rewrites the surface predicate before code generation can produce error messages that reference rewritten code, not the user's code. The detection-and-rewrite pipeline must preserve enough provenance that errors and warnings refer back to the user's original spec.

**Inferring intent that the user did not have.** If the compiler decides that a particular ancestor query *should* be implemented as a precomputed materialisation because it predicts the query will be called many times — but the user actually intended a one-shot query — the materialisation cost is wasted. Detection should be conservative: pick the strategy that fits the spec, not the strategy that fits a predicted usage pattern, unless the usage pattern is also declared.

These are general design principles, but they bite particularly hard for pattern detection because detection is implicit. The user did not write `compile_as_fixed_point`; if the compiler emits a fixed-point loop, the user did not consent. The way to make the user consent is by clear diagnostics and by ensuring the chosen strategy is also redirectable.

## Composition: chains of detection

Real workloads have many predicates. A user's program might have ten predicates, each compilable to multiple strategies. The detection question generalises: given the *program*, what combination of strategies produces the best aggregate compilation?

This is the optimiser's problem in any production compiler. The detection layer is one input; the cost model is another; the user's optimisation manifests are a third. The compiler combines all three to choose a per-predicate strategy.

The combinatorics get large quickly. If each of ten predicates has three reasonable strategies, the total number of combinations is `3^10 ≈ 60k`. Exhaustive search is infeasible past five or six predicates. Heuristic search — pick locally optimal strategies, then iterate with global cost estimation — is the standard approach. UnifyWeaver does not currently do this; the optimisation manifests are per-predicate and combinations are not optimised globally. The cross-predicate optimisation is the layer above pattern detection and is itself a forward-direction work item.

## Why the chapter says "by rule" rather than "automatically"

The chapter title says "by rule" deliberately. The contrast is with "by inference from usage" or "by training on examples". The mechanism the book is committed to is *rule-based pattern detection*: a finite, well-defined set of rules that map recognised patterns to compilation strategies. The rules are debuggable, modifiable, and traceable. The user can see exactly which rule fired and why.

This is consistent with UnifyWeaver's broader design philosophy: declarative interfaces, predictable behaviour, debuggable internals. A learned pattern classifier would optimise for a different objective (high-recall recognition of many patterns) at the cost of debuggability. The book's argument is that for the current scale and scope of the problem, the rule-based approach is the right point on the trade-off curve.

## Next

Chapter 11: Open problems.
