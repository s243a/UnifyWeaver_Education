# Book 3 Restructuring Plan

## Problem

Chapter 3 ("C# Query Runtime - Deep Dive") documents the **old
non-parameterized query engine**. This engine:
- Cannot handle `is/2` arithmetic in recursive predicates
- Has no mode declarations or parameter seeding
- Was superseded by the **parameterized query engine** which supports
  `is/2`, mode declarations (`user:mode/1`), demand closure, and
  `ParamSeedNode`

The old engine is still in the codebase and is simpler to understand.
It provides a good pedagogical stepping stone to the parameterized engine.

## Proposed Structure

### Current Structure

```
Ch 1: Introduction to Multi-Target Compilation
Ch 2: C# Native Target
Ch 3: C# Query Runtime (OLD — non-parameterized)  ← problem
Ch 4: Runtime Libraries and Deployment
Ch 5: Semantic Crawling
Ch 6: PowerShell Semantic
```

### Proposed Structure

```
Ch 1: Introduction to Multi-Target Compilation (minor updates)
Ch 2: C# Native Target (unchanged)
Ch 3: Parameterized Query Engine (NEW — the current engine)
Ch 4: Performance at Scale — Benchmark Results (NEW)
Ch 5: Runtime Libraries and Deployment (was Ch 4)
Ch 6: Semantic Crawling (was Ch 5)
Ch 7: PowerShell Semantic (was Ch 6)

Appendix A: The Non-Parameterized Query Engine (was Ch 3)
    "Read this first for a simpler introduction to query plans"
```

## New Chapter 3: Parameterized Query Engine

### Content Outline

1. **Why parameterized queries?**
   - The limitation: old engine couldn't support `is/2` arithmetic
   - Example: `category_ancestor(Cat, Ancestor, Hops)` with
     `Hops is H1 + 1` — fails on old engine, works on parameterized
   - Mode declarations: `user:mode(category_ancestor(+, -, -))`

2. **How it works**
   - `ParamSeedNode` — seeds evaluation with known input bindings
   - Demand closure — computes which tuples are needed (backward from query)
   - `FixpointNode` with semi-naive delta-set evaluation
   - `HashSet<T>` deduplication — automatic cycle detection

3. **Worked example: category_ancestor/3**
   - Prolog source with `is/2` arithmetic
   - How to declare modes
   - Base case constant in body (not head) — current limitation
   - Generated C# code walkthrough

4. **Comparison with old engine**
   - What changed: `cl_target_id` resolution, parameter binding
   - What stayed: RelationScan, JoinNode, ProjectionNode (same IR nodes)
   - Migration: old predicates still work, parameterized adds new capability

5. **Advanced: mutual recursion with parameters**
   - SCC analysis for mutually recursive predicates
   - Demand closure across SCC groups
   - Key commits: `dce3945b`, `88346fe9`

### Key Reference

- Implementation: `src/unifyweaver/targets/csharp_target.pl`
- Runtime: `src/unifyweaver/targets/csharp_query_runtime/QueryRuntime.cs`
- Tests: `tests/core/test_csharp_query_target.pl`
- Effective distance benchmark: `examples/benchmark/`

## New Chapter 4: Performance at Scale — Benchmark Results

### Content Outline

1. **The effective distance benchmark**
   - Wikipedia category hierarchy traversal
   - `d_eff = (Σ dᵢ^(-n))^(-1/n)` formula and spectral dimensionality
   - Why this is a challenging workload (all-simple-paths, cycle detection,
     aggregation)

2. **Scaling results**
   - Table from README: C#, Rust, Go, Codon, CPython across 300→10K articles
   - C# takes the lead at 1K+ and widens gap at scale
   - Why: LINQ-inspired HashSet architecture + .NET JIT optimization

3. **What this means for users**
   - Enterprise/.NET: C# query engine is the best choice at scale
   - Write once in Prolog, compile to optimal target
   - The UnifyWeaver value proposition

4. **Per-path visited recursion**
   - New recursion pattern discovered during benchmark
   - How each target handles it differently
   - Design docs in `docs/design/PER_PATH_VISITED_*.md`

### Key Reference

- Benchmark code: `examples/benchmark/`
- Results README: `examples/benchmark/README.md`
- Theory: `docs/proposals/CROSS_TARGET_EFFECTIVE_DISTANCE_THEORY.md`
- PRs: #1054 through #1094

## Appendix A: The Non-Parameterized Query Engine

Move current Chapter 3 content here with:

1. **Preface**: "This appendix covers the original non-parameterized query
   engine. While superseded by the parameterized engine (Chapter 3), it
   provides a simpler introduction to query plan concepts. We recommend
   reading this appendix first if you're new to query engines, then
   returning to Chapter 3 for the full parameterized approach."

2. **Content**: Current Chapter 3, largely unchanged

3. **Postscript**: "The non-parameterized engine couldn't handle arithmetic
   (`is/2`) in recursive predicates. This limitation motivated the
   parameterized redesign described in Chapter 3. The old engine is still
   available in the codebase at `csharp_native_target.pl`."

## Implementation Priority

| Task | Effort | Priority |
|------|--------|----------|
| Move Ch 3 → Appendix A | Low | 1 — no content changes, just file rename + preface |
| Write new Ch 3 (parameterized engine) | High | 2 — core educational content |
| Write new Ch 4 (benchmark results) | Medium | 3 — data already exists in README |
| Update README.md table of contents | Low | 4 — after chapters are written |
| Renumber Ch 4-6 → Ch 5-7 | Low | 5 — file renames |
| Update cross-references | Low | 6 — links between chapters |

## Notes

- The `education/` folder is its own git repository
- Video scripts in `video-scripts/` may also need updating
- Implementation docs in `implementation/` reference the old engine
