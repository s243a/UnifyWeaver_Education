<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 8: Declarative vs Procedural, Revisited

Chapter 2 introduced the declarative/procedural split as a property of the *interface*: what the user writes against. Chapter 7 introduced compilation strategies that emit explicitly procedural loops. This chapter reconciles the two — and argues that a compiler-emitted procedural loop does not, on its own, make the user's specification procedural.

## The naive view

A naive view: any code that contains a loop is procedural. A spec that compiles to a loop is therefore also procedural, at least in its compiled form. Declarative is the absence of loops.

The naive view collapses two distinctions: *who wrote the loop* and *who is responsible for getting it right*. If the user wrote the loop, the user is responsible for the iteration variable, the termination condition, the per-step body. If the compiler wrote the loop in response to a declarative spec, the user is responsible for the spec; the compiler is responsible for the loop. The compiled artifact is procedural either way, but the *programming experience* is fundamentally different.

A more careful definition: *declarative* describes a contract between the user and the compiler. The user states what should be true of the answer. The compiler picks an implementation that satisfies the statement. The implementation may contain any number of loops, branches, mutations, or other procedural artifacts. What matters is that the user did not write them and is not asked to reason about them at the surface level.

## The two-property test (recap)

Chapter 2 proposed two properties as the definition of *redirectable declarative*:

1. **Readable as a statement.** The user can read the surface spec and reason about the answer it defines.
2. **Redirectable without rewrite.** The user can change the optimisation manifest or target and still get the same answer (modulo target capabilities); the surface spec stays unchanged.

Both properties hold of a difference-equation compilation if the compiler is well-architected:

- The surface spec is still Prolog, and the user reads it as the relation it defines. The fact that the runtime computes that relation by iterating a fixed-point rule is an implementation choice. The relation is still the relation.
- The user can switch between graph-search compilation and difference-equation compilation by changing the optimisation manifest. The spec does not need rewriting. The same `ancestor/2` predicate compiles, in principle, to either backend.

The book's claim: this satisfies the redirectable-declarative property. The procedural-ness of the *generated code* is irrelevant — what matters is the procedural-ness of the *user's source*.

## What the user gives up

The redirectable-declarative bargain has a price.

**Predictability of cost.** A hand-written procedural implementation has knowable cost: you can read the loop and estimate the work. A declarative spec compiled by a sufficiently clever compiler has cost determined by the compiler's choices. If the compiler picks a `O(V³)` materialisation when the user expected `O(V·E)` per-query, the user's mental model of cost diverges from the system's behaviour.

UnifyWeaver mitigates this by making the optimisation manifest *visible*: the user sees what strategy was chosen, and can edit the manifest to redirect. The user does not need to *prescribe* the strategy, but they do see it.

**Direct control over execution order.** In hand-written Prolog, the order of clauses and the placement of cuts determine the proof-search order. The user can optimise for the expected query pattern. In UnifyWeaver, the compiler may reorder, may transform, may merge clauses. The user loses control over the execution order in exchange for the compiler's freedom to optimise.

**Predictability of behaviour under load.** A difference-equation iteration that early-terminates on convergence behaves differently when the input changes. A query that returned the same answer on Monday and Tuesday may, on Wednesday, see one more iteration because the graph grew enough to push convergence past a threshold. The denotational answer is the same; the wall-clock cost is not. Users with hard latency budgets need to plan for the variation.

These costs are real. They are also costs the user *also* pays for SQL, for any sufficiently-optimising language, and for any system where a compiler chooses between strategies that have different cost profiles. The bargain is not unique to UnifyWeaver; it is the same bargain anyone trading direct control for redirectability makes.

## What the user gets back

The benefit of the bargain: the user's surface spec is *durable*. It survives:

- Changes in the target language. A spec written for the F# target compiles to Rust or Python with no rewrite.
- Changes in the optimisation strategy. A spec originally compiled as a stream can be recompiled as a fixed point.
- Changes in the underlying graph representation. A spec originally backed by LMDB can be backed by SQLite, by an in-memory dict, by a remote service.
- Changes in the compiler's optimisation choices over time. As the compiler gets smarter, the same spec gets faster code generated for it — without the user touching their source.

This durability is the practical value of redirectable-declarative. The user invests in writing a clear specification once; the compiler delivers many implementations of it over the project's lifetime. The ratio of user effort to compiled artifacts is small, and getting smaller as the compiler matures.

## When the bargain breaks

The bargain breaks when the compiler's choice is *wrong* and the user cannot redirect.

If the compiler defaults to per-query graph search but the workload would benefit from fixed-point materialisation, and the optimisation manifest does not let the user say "use fixed-point", the user is stuck. They either accept poor performance or abandon the declarative interface and write the loop themselves.

This is the failure mode the optimisation manifest is designed to avoid. The manifest must expose enough levers that any reasonable optimisation choice is reachable from the manifest. If the user wants something the manifest does not name, the system has failed at the redirectable property.

UnifyWeaver's current optimisation manifest exposes coarse choices (target, deduplication strategy, cache mode, scan strategy). It does not yet expose finer-grained choices (per-predicate compilation variant within a target, hybrid strategies, materialisation-vs-iteration trade-offs). This is a known gap. The forward direction is to grow the manifest's expressive range while keeping it manageable for the user.

## When the bargain is too strong

The opposite failure: the compiler is so committed to declarative-purity that it refuses to let the user state a procedural fact even when the procedural fact is the right knowledge to convey.

Example: the user knows that one particular query is in the hot path and should always use a particular index. If the optimisation manifest only accepts declarative hints ("the answer should be deduplicated") and not procedural ones ("use the `category_child` index for this lookup"), the user is forced to either accept the compiler's choice or hack around the compiler.

UnifyWeaver currently lets users supply both declarative and procedural hints, and this seems to be the right balance. The `constraint_analyzer.pl` module accepts declarative constraints (`unique`, `unordered`); the `cost_model.pl` and `algorithm_manifest.pl` modules accept more procedural hints (scan strategy, materialisation thresholds). The user picks the level of abstraction appropriate to what they know.

## Why the philosophical question matters

The chapter title says "revisited" because chapter 2 already laid out the position. What's new in this chapter is the *recognition* that compiled procedural code does not violate the declarative contract — and the *naming* of the cost the contract imposes.

The reason this matters: every conversation about UnifyWeaver eventually reaches the question "but is it really declarative if the compiler emits a loop?" The answer is yes, provided the two properties hold. The conversation should then move to whether the properties actually hold for the case at hand — which is a productive engineering question, not a philosophical one.

Without the framing, the conversation gets stuck on definitions. With the framing, it focuses on the actual question: *is the optimisation manifest expressive enough? Is the compiler's choice predictable enough? Is the user's spec durable enough?* These are answerable questions with concrete success criteria. The philosophical question is not.

## Next

Chapter 9: Constraint-hint predicates.
