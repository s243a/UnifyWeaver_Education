<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: The F# WAM Target

UnifyWeaver has two F# compilation variants. The previous chapters covered the native-lowering `fsharp_target`. This chapter covers `fsharp_wam` — a second F# target that hosts UnifyWeaver's symbolic WAM instruction set on F# functions, with explicit support for disk-resident graph data via LightningDB.

## Two F# variants — when to pick which

| Variant | Module | What it lowers to | Best for |
|---|---|---|---|
| `fsharp_target` | `src/unifyweaver/targets/fsharp_target.pl` | Idiomatic F# records, `Seq` pipelines, `let rec`/`and` recursion | General-purpose predicates that fit cleanly into native F# functional style |
| `fsharp_wam` | `src/unifyweaver/targets/fsharp_wam/` (and `templates/targets/fsharp_wam/`) | F# functions that emulate the WAM instruction set, plus kernel templates | Patterns the native lowering handles poorly: heavy non-determinism, deep unification, graph traversal over large disk-resident datasets |

The split mirrors the same trade-off that motivates the symbolic WAM target ([Book 17](../../book-17-wam-target/)): native lowering is faster and more idiomatic when it applies, but some Prolog features are awkward to lower natively. The WAM-hosted variant preserves Prolog semantics exactly at some cost in code size and a different (but still good) performance profile.

For F# specifically, the `fsharp_wam` variant adds one more capability the native variant does not have: **kernel templates that integrate with disk-resident data sources**. The bidirectional ancestor kernel — used as the worked example throughout `book-18-graph-algorithms` — is implemented as an `fsharp_wam` template that uses LightningDB cursors to traverse multi-million-node graphs without loading them into memory.

## Architecture at a glance

The `fsharp_wam` target consists of three things:

1. **Generated F# code** for the user's predicates, written in WAM-instruction style as F# functions that operate on an explicit machine state.
2. **Runtime support code** that implements the WAM machine state (registers, choice point stack, unification routines) in F#.
3. **Kernel templates** — pre-written F# code parameterised by Mustache variables, instantiated by the compiler for specific predicate signatures. Templates live in `templates/targets/fsharp_wam/`.

The third piece is what makes `fsharp_wam` distinctive in UnifyWeaver. Native targets typically rely on user-provided libraries; `fsharp_wam` ships its own templates for performance-critical patterns. The bidirectional ancestor kernel is the most developed of these, but the design accommodates more.

## When to use `fsharp_wam`

Concrete situations where the WAM-hosted variant is the right pick.

**You need exact Prolog semantics for deep unification.** The native lowering of complex structure unification is possible but verbose; the WAM-style emulation handles it uniformly.

**You have mutual recursion among many predicates.** Native `let rec ... and` works for small mutual-recursion clusters but becomes unwieldy past three or four interlinked predicates. The WAM target's choice-point mechanism handles arbitrary mutual recursion uniformly.

**You need disk-resident data sources.** Native lowering generally expects data in memory. The `fsharp_wam` target's kernel templates can be instantiated against any data source for which a cursor adapter exists — LightningDB (LMDB) is the most-developed adapter; SQLite, file-system, and remote-source adapters are smaller pieces of work.

**You need a kernel template that has been hand-tuned for a specific pattern.** The bidirectional ancestor kernel is the canonical example; new patterns can be added as templates and reused across many predicates.

Conversely, the native `fsharp_target` is preferred when:

- The predicate fits idiomatically into functional F#.
- The data fits in memory.
- The patterns are standard (tail recursion, linear recursion, simple mutual recursion).
- The user wants generated code that is human-readable and matches how an F# programmer would write the function by hand.

The two variants are not mutually exclusive: a single program can compile some predicates with `fsharp_target` and others with `fsharp_wam`, mixing them in the same project. The target choice is per-predicate.

## LightningDB integration

LightningDB is a .NET binding for LMDB — a memory-mapped key-value store with cursor-based traversal. The combination of LMDB's on-disk durability, the OS page cache for hot data, and cursor-style sequential scans makes it well-suited to graph storage.

The `fsharp_wam` target opens an LMDB environment and uses cursors to look up adjacency:

```fsharp
let openEnv (path: string) =
    let cfg = EnvironmentConfiguration(MaxDatabases = 8)
    cfg.MapSize <- 12L * 1024L * 1024L * 1024L
    let env = new LightningEnvironment(path, cfg)
    env.Open(EnvironmentOpenFlags.ReadOnly ||| EnvironmentOpenFlags.NoLock)
    env

type LmdbLookup(env: LightningEnvironment, dbName: string) =
    member _.Lookup (key: int) : int list = ...
```

(The full code lives in `examples/prototypes/tree_likeness_index_depth_likeness_probes/fsharp_v1_v3_probe/Program.fs`; the snippet above is condensed.)

A LightningDB-backed `LmdbLookup` is the adjacency function the kernel templates take as input. The kernel does not know it is talking to LMDB — it sees only a function `int -> int list`. Other adapters can substitute without changing the kernel.

## Worked example: the bidirectional ancestor kernel

The artifact is `templates/targets/fsharp_wam/kernel_bidirectional_ancestor.fs.mustache`. After Mustache substitution, it is roughly 80 lines of F#. The kernel computes paths between a source node and a target node on a DAG, using bidirectional A\* with an admissible heuristic based on minimum BFS distance.

The kernel signature, after instantiation:

```fsharp
let nativeKernel_bidirectional_ancestor
    (lookupParents: int -> int list)
    (lookupChildren: int -> int list)
    (cat: int)
    (root: int)
    (parentCost: float)
    (childCost: float)
    (budget: float)
    : (int * int * int) list
```

It takes the source/target node IDs, cost weights for parent-direction and child-direction edges, and a budget cap. It returns a list of paths represented as `(h, n, m)` triples — total hops, parent-direction hops, child-direction hops.

The kernel is reusable across many metrics: the same path-triple output feeds shortest-path distance, weighted-power-mean metric, simple path counting, and tree-likeness measurements. The kernel does the expensive work; the per-metric formula is cheap.

For the algorithmic depth on what these path triples mean and what metrics are computed from them, see [book-18-graph-algorithms](../../book-18-graph-algorithms/), particularly chapters 4 (metrics) and 5 (the kernel in detail).

## Performance characteristics

Measurements from the F# probe in `examples/prototypes/tree_likeness_index_depth_likeness_probes/fsharp_v1_v3_probe/`, against the post-fix LMDB build of the English Wikipedia categorisation DAG (2.26M nodes, 6.7M edges):

| Metric | Cost |
|---|---|
| LMDB open | < 100 ms |
| One-time calibration scan (full BFS + degree statistics) | ~8 s |
| Per-seed bidirectional ancestor query | ~0.05 s |
| Maximum path enumeration cap | 200,000 paths per query (configurable) |

The per-seed cost is small enough that interactive use is realistic. The calibration is amortised across many queries — running 1,000 queries amortises calibration to 8 ms per query, dwarfed by the 50 ms per-query cost.

A subtle gotcha: an earlier version of the probe re-ran calibration per query and took 31 seconds per seed. Lifting the calibration out of the per-query loop was a ~600× speedup. The lesson generalises — any kernel that depends on a global graph property should accept the property as an input, not recompute it. The kernel template is structured this way; user code that wraps the kernel should preserve the separation.

## Compilation flow

The user's invocation looks similar to the native target:

```prolog
?- use_module('src/unifyweaver/targets/fsharp_wam_target').
?- init_fsharp_wam_target.
?- compile_predicate_to_fsharp_wam(ancestor/2, [
       target(fsharp_wam),
       strategy(bidirectional_search),
       source(lmdb(path('/data/enwiki.lmdb')))
   ], Code),
   write_fsharp_program(Code, 'AncestorKernel.fs').
```

The compiler:
1. Resolves the optimisation manifest (sees `strategy(bidirectional_search)`).
2. Selects the corresponding kernel template (`kernel_bidirectional_ancestor.fs.mustache`).
3. Resolves the data-source adapter (LMDB at the given path).
4. Substitutes the Mustache variables (predicate name, cost weights, adapter type).
5. Emits the F# code plus the surrounding runtime support.

The generated F# is a complete program that the user can build with `dotnet build` and run against any LMDB conforming to the expected schema.

## Cross-references

- [Book 17: WAM Target](../../book-17-wam-target/) — background on the symbolic WAM instruction set and the architectural reasons for hosting it on multiple backends.
- [book-18-graph-algorithms](../../book-18-graph-algorithms/) — the algorithmic side of the bidirectional kernel: what `d_wPow` is, why the kernel computes path triples instead of a single metric, the four budget variants tested, what the experimental results showed.
- [Chapter 1 of this book](01_introduction.md) — the native F# target, for predicates that don't need the WAM-hosted machinery.
- [Chapter 3 of this book](03_recursive_queries.md) — recursion patterns in the native F# target. The WAM variant handles these too but lowers them differently.

## What is in the `fsharp_wam` target today

- The bidirectional ancestor kernel template, complete and used at multi-million-node scale.
- A LightningDB cursor adapter, complete.
- Runtime support for the WAM machine state in F#.
- Mustache-based template instantiation.

## What is not yet in the target

- A general predicate-to-WAM lowering pipeline for the `fsharp_wam` target with the same coverage as `fsharp_target` has for native lowering. The kernel-template path is the main usage today; arbitrary-predicate compilation is less developed.
- Cursor adapters for non-LMDB sources (SQLite, file-system, remote services). The architecture supports them; the adapters have not been written.
- More kernel templates beyond the bidirectional ancestor. Plausible additions: single-source shortest paths, strongly connected components, topological sort, k-shortest-paths. None implemented.
- Cross-target parity validation of `fsharp_wam`-generated code against native targets at scale. Validated at simplewiki scale; not at enwiki scale, where the kernel was treated as authoritative because no comparable native implementation exists.

These gaps are not blockers for the current use case (the bidirectional kernel does what it needs to do) but they are the natural directions to extend the target.

## Next

The native F# book has no chapter 5 planned; the F# target documentation across books is structured as:

- This book ([book-fsharp-target](README.md)) — F# target usage, both native and WAM variants.
- [book-18-graph-algorithms](../../book-18-graph-algorithms/) — algorithmic content using F# as the example language.
- [Book 17: WAM Target](../../book-17-wam-target/) — the symbolic WAM background that `fsharp_wam` rests on.
