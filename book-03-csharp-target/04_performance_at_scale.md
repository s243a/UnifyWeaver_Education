<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: Performance at Scale — Cross-Target Benchmark

## Introduction

UnifyWeaver compiles Prolog to multiple target languages. But which
target performs best? The answer depends on the workload and scale.

This chapter presents results from the **effective distance benchmark**
— a real-world workload that computes hierarchical distances through
Wikipedia's category graph. The benchmark demonstrates that the C#
parameterized query engine (Chapter 3) scales better than compiled
alternatives at enterprise data volumes.

## The Benchmark: Effective Distance

### The Formula

Given an article connected to a target category through multiple paths
of lengths d₁, d₂, ..., dₖ:

    d_eff = (Σ dᵢ^(-n))^(-1/n)    where n = 5

This is a **generalized power mean** with dimension parameter n.
Short paths dominate (d^(-5) makes long paths negligible), but
multiple paths through different intermediates all contribute.

### Why It's Hard

The computation requires:
1. **Recursive graph traversal** — find all simple paths through a DAG
2. **Per-path cycle detection** — each path must not revisit a node
3. **Aggregation** — sum d^(-5) over all paths, then compute power mean

This exercises every target's strengths and weaknesses:
- Data structure choice (hash set vs list for visited check)
- Recursion strategy (native vs DFS stack vs fixpoint)
- JIT optimization (hot loop performance)

### Dataset

Source: Simple English Wikipedia category hierarchy

| Scale | Articles | Category Edges |
|-------|----------|---------------|
| 300 | 289 | 6,008 |
| 1K | 1,000 | 5,933 |
| 5K | 5,000 | 12,981 |
| 10K | 10,000 | 25,227 |

## Results: Execute Time

| Target | 300 art | 1K art | 5K art | 10K art |
|--------|---------|--------|--------|---------|
| **C#** | 0.43s | **1.13s** | **4.74s** | **9.48s** |
| **Rust** | **0.33s** | 1.33s | 6.86s | 12.44s |
| **Go** | 0.43s | 1.96s | 11.36s | 18.71s |
| **Codon** | 0.67s | 2.55s | 10.98s | 22.14s |
| **CPython** | 0.73s | 2.93s | 15.71s | — |
| **Prolog** | 1.26s | — | — | — |
| **AWK** | 2.46s | — | — | — |

### The Crossover: C# Overtakes Rust

At 300 articles, Rust is fastest (0.33s) — zero-overhead native binary
with `HashSet<String>`. But at 1K articles, **C# takes the lead** (1.13s
vs 1.33s) and the gap widens:

- At 5K: C# is **1.4x faster** than Rust
- At 10K: C# is **1.3x faster** than Rust, **2.0x faster** than Go

### Why C# Wins at Scale

The parameterized query engine's architecture gives it structural
advantages:

1. **.NET JIT optimization** — The hot DFS loop gets aggressively
   optimized after warmup. Rust's static compilation can't adapt to
   runtime patterns.

2. **HashSet with efficient copy semantics** — C#'s `HashSet<string>`
   has optimized copy constructors. Go's `map[string]bool` requires
   iterating and copying every entry.

3. **LINQ-inspired evaluation** — The query engine's plan-based execution
   naturally decomposes the problem into cacheable sub-operations.

### Targets That Fall Off

- **CPython** — Interpreter overhead per operation. Dropped at 10K.
- **Prolog** — `member/2` list check is O(n) per node. Needs demand
  analysis optimization (not yet implemented).
- **AWK** — Interpreter overhead. Fast for text streaming, slow for
  graph traversal.

### Go vs Codon: Neck and Neck

Go and Codon (compiled Python) are surprisingly close:

| Scale | Go | Codon | Winner |
|-------|-----|-------|--------|
| 5K | 11.36s | 10.98s | Codon (by 3%) |
| 10K | 18.71s | 22.14s | Go (by 16%) |

No clear winner — the choice between them is about ecosystem, not
performance.

## Results: Compile Time

File-based data loading (not embedded):

| Target | Compile Time | Notes |
|--------|-------------|-------|
| **Go** | 0.13–0.50s | Fastest compile |
| **Rust** | 0.31–0.72s | Fast with file loading |
| **C#** | 1.21–3.00s | dotnet build overhead |
| **Codon** | 4.00–5.53s | LLVM backend |

Note: Earlier versions embedded data as string literals, causing
Rust to take **12 minutes** to compile 6K edges. Switching to
file-based loading reduced this to **0.65 seconds** — a 1100x
improvement.

## Target Recommendations

| Audience | Recommended Target | Why |
|----------|-------------------|-----|
| Enterprise / .NET | **C#** | Best scaling, purpose-built query engine |
| ML / Data Science | **Codon** | Stay in Python ecosystem, competitive performance |
| Cloud / DevOps | **Go** | Fast compile, excellent ecosystem |
| Systems / Embedded | **Rust** | Fastest at small scale, no runtime overhead |

## The UnifyWeaver Value Proposition

This benchmark demonstrates UnifyWeaver's core promise:

> **Write your logic once in Prolog. Compile to the optimal target
> for your scale and ecosystem.**

At small scale, Rust wins. At enterprise scale, C# wins. For ML
workloads, Codon competes with Go. The user doesn't need to know
which — UnifyWeaver handles the compilation.

And in the future, the same demand analysis that powers the C#
engine can optimize Prolog itself — making UnifyWeaver a tool
that optimizes Prolog-to-Prolog, not just Prolog-to-other-languages.

## Exercises

1. **Run the benchmark yourself**: Follow the instructions in
   `examples/benchmark/README.md` to generate datasets and profile
   each target.

2. **Compare at different scales**: Start with 19 articles (dev),
   then scale to 300, 1K, 5K. At what scale does C# overtake Rust
   on your machine?

3. **Try different max-depth values**: How does `--max-depth 5` vs
   `--max-depth 15` affect execution time and result accuracy?

4. **Add a new target**: If you have a favorite language, try adding
   it to `generate_pipeline.py`. How does it compare?

## Further Reading

- Benchmark code and full results: `examples/benchmark/README.md`
- Theory (spectral dimensionality): `docs/proposals/CROSS_TARGET_EFFECTIVE_DISTANCE_THEORY.md`
- Per-path visited recursion: `docs/design/PER_PATH_VISITED_RECURSION.md`
- Demand analysis proposal: `docs/proposals/PROLOG_TARGET_DEMAND_ANALYSIS.md`
