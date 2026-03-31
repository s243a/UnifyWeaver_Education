<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025-2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: The Parameterized Query Engine

## Introduction

The **parameterized query engine** is UnifyWeaver's most powerful C#
compilation target. It extends the original query engine (Appendix A) with:

- **Mode declarations** — specify which arguments are inputs (`+`) vs
  outputs (`-`)
- **Arithmetic support** — `is/2` expressions work inside recursive
  predicates
- **Demand analysis** — the engine computes which tuples are "needed"
  and avoids unnecessary work
- **Parameter seeding** — known input values seed the evaluation, making
  queries more efficient

If you haven't read Appendix A yet, we recommend starting there — it
covers the simpler IR concepts (RelationScan, JoinNode, ProjectionNode)
that this chapter builds on.

## Why Parameterized Queries?

The original query engine (Appendix A) could compile predicates like:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

But it **failed** on predicates with arithmetic:

```prolog
category_ancestor(Cat, Parent, 1) :- category_parent(Cat, Parent).
category_ancestor(Cat, Ancestor, Hops) :-
    category_parent(Cat, Mid),
    category_ancestor(Mid, Ancestor, H1),
    Hops is H1 + 1.                         % ← This failed!
```

The error: "variable not bound in body" — the old engine couldn't track
that `H1` would be bound by the recursive call before being used in `is/2`.

The parameterized engine solves this by:
1. Declaring which arguments are inputs/outputs via **mode declarations**
2. Using **ParamSeedNode** to seed the evaluation with known input bindings
3. Tracking variable binding through the query plan

## Mode Declarations

Tell the engine which arguments are inputs and which are outputs:

```prolog
:- assert(user:mode(category_ancestor(+, -, -))).
%                                      ^  ^  ^
%                                      |  |  └── output: Hops
%                                      |  └───── output: Ancestor
%                                      └──────── input: Cat (known at query time)
```

Mode symbols:
- `+` — input: value is known when the predicate is called
- `-` — output: value is computed by the predicate
- `?` — either (not yet fully supported)

## Compiling with the Parameterized Engine

```prolog
% 1. Load your facts
[data/benchmark/dev/facts].

% 2. Declare mode
assert(user:mode(category_ancestor(+, -, -))).

% 3. Define the predicate (note: constant in BODY, not head)
assert(user:(category_ancestor(Cat, Parent, Hops) :-
    category_parent(Cat, Parent), Hops is 1)).
assert(user:(category_ancestor(Cat, Ancestor, Hops) :-
    category_parent(Cat, Mid),
    category_ancestor(Mid, Ancestor, H1),
    Hops is H1 + 1)).

% 4. Compile
use_module('src/unifyweaver/targets/csharp_target'),
compile_predicate_to_csharp(category_ancestor/3,
    [target(csharp_query)], Code),
write(Code).
```

### Important: Constants in Body, Not Head

The parameterized engine currently requires variables in head positions.
Instead of:

```prolog
category_ancestor(Cat, Parent, 1) :- ...    % ← head constant
```

Write:

```prolog
category_ancestor(Cat, Parent, Hops) :- ..., Hops is 1.  % ← body constant
```

This is a known limitation — the plan compiler should eventually handle
head constants by generating a SelectionNode automatically.

## What Gets Generated

The parameterized engine generates a C# module with:

1. **InMemoryRelationProvider** — fact store loaded with your data
2. **QueryPlan** — tree of plan nodes describing the computation
3. **QueryExecutor** — evaluates the plan against the provider

```csharp
// Simplified generated code
public static (InMemoryRelationProvider, QueryPlan) Build() {
    var provider = new InMemoryRelationProvider();

    // Load facts
    provider.AddFact(new PredicateId("category_parent", 2),
        "Physics", "Natural_sciences");
    // ... more facts ...

    // Query plan with FixpointNode for recursion
    var plan = new QueryPlan {
        Root = new FixpointNode {
            Head = new PredicateId("category_ancestor", 3),
            Base = new RelationScanNode { ... },
            Recursive = new ProjectionNode {
                Input = new ArithmeticNode {
                    Input = new JoinNode {
                        Left = new RelationScanNode { /* category_parent */ },
                        Right = new RecursiveRefNode { /* delta */ }
                    },
                    Expression = "H1 + 1"
                }
            }
        }
    };

    return (provider, plan);
}
```

### Key Plan Nodes

| Node | Purpose | SQL Analogy |
|------|---------|-------------|
| `RelationScanNode` | Read facts from a relation | `SELECT * FROM table` |
| `JoinNode` | Combine two relations | `JOIN ON key` |
| `ProjectionNode` | Select/reorder columns | `SELECT col1, col2` |
| `SelectionNode` | Filter rows | `WHERE condition` |
| `ArithmeticNode` | Compute `is/2` expressions | Computed column |
| `FixpointNode` | Semi-naive recursive evaluation | Recursive CTE |
| `ParamSeedNode` | Seed with known input values | Query parameter |
| `AggregateNode` | `aggregate_all/3` operations | `GROUP BY ... SUM()` |

## Semi-Naive Evaluation with Parameters

The `FixpointNode` evaluates recursion using **semi-naive evaluation**
with **delta sets**:

```
Iteration 0: Base case
  category_ancestor("Physics", "Natural_sciences", 1)
  category_ancestor("Physics", "Scientific_disciplines", 1)

Iteration 1: Delta = new tuples from joining with step relation
  category_ancestor("Quantum_mechanics", "Natural_sciences", 2)
  category_ancestor("Quantum_mechanics", "Scientific_disciplines", 2)
  ... (only NEW tuples, not recomputed old ones)

Iteration 2: Delta from joining iteration 1's delta
  ...

Converged: Delta is empty → done
```

Key properties:
- **Each iteration only processes new tuples** (delta set)
- **HashSet<T> deduplication** prevents duplicates automatically
- **Cycle detection is implicit** — if a tuple was already produced,
  it's in the HashSet and won't be added to delta again
- **No visited list needed** — unlike Prolog, the engine handles cycles
  through tuple-level deduplication

## Demand Analysis

When input modes are declared, the engine can compute a **demand set** —
which tuples are actually needed to answer the query. This avoids
computing tuples that will never contribute to the answer.

For example, if you query `category_ancestor("Physics", X, Y)`, the
engine knows the first argument is always "Physics" and can:
1. Seed the evaluation with `ParamSeedNode("Physics")`
2. Only compute ancestors reachable from "Physics"
3. Skip all other starting points

This is similar to how a database pushes query predicates down into
the execution plan.

### Related Commits

- `dce3945b` — Demand closure for parameterized mutual SCCs
- `88346fe9` — Allow parameterized mutual recursion
- `e738fee3` — Seed transitive closure for parameters
- `50f02ba9` — Broaden `is/2` and arithmetic comparisons

## Comparison: Old vs Parameterized Engine

| Feature | Old Engine (Appendix A) | Parameterized (This Chapter) |
|---------|------------------------|------------------------------|
| `is/2` arithmetic | Not supported | Supported |
| Mode declarations | Not needed | Required for parameterized |
| Demand analysis | None | Backward reachability |
| ParamSeedNode | No | Yes |
| Mutual recursion | Supported | Supported (with demand) |
| Head constants | Supported | Must be in body (current limitation) |
| Performance at scale | Good | Better (demand pruning) |

## Performance at Scale

The parameterized query engine was benchmarked against Rust, Go, Python,
and other targets on a Wikipedia category hierarchy workload. Results
at scale are covered in Chapter 4, but the headline:

**C# overtakes Rust at 1K articles and widens the gap at 10K.**

The LINQ-inspired architecture — HashSet deduplication, .NET JIT
optimization, and semi-naive evaluation — scales better than raw
compiled DFS in Rust/Go. This validates the parameterized engine
design for enterprise workloads.

## Exercises

1. **Compile `ancestor/2`** (no arithmetic) using both the old engine
   (Appendix A) and the parameterized engine. Compare the generated code.

2. **Add `is/2` arithmetic**: Modify `ancestor/2` to track hop count
   (`ancestor/3`). Verify it compiles with the parameterized engine but
   fails with the old engine.

3. **Declare modes**: Try `mode(ancestor(+, -))` vs `mode(ancestor(-, +))`.
   How does the generated plan change?

4. **Benchmark**: Run the effective distance benchmark from
   `examples/benchmark/` and compare C# vs Go execution times at
   different scales.

## Summary

The parameterized query engine is UnifyWeaver's most capable C# target:
- Supports arithmetic (`is/2`) in recursive predicates
- Uses mode declarations to optimize evaluation
- Implements demand analysis for pruning unnecessary computation
- Scales better than compiled alternatives at enterprise data volumes

For the foundational concepts (RelationScan, Join, Projection, Fixpoint),
see Appendix A. For benchmark results demonstrating performance at scale,
continue to Chapter 4.
