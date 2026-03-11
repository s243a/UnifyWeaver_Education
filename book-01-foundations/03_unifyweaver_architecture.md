<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: The UnifyWeaver Architecture

Now that we have a grasp of basic Prolog, we can explore how UnifyWeaver translates these concepts into executable code. This chapter provides a high-level overview of the compiler's **principal architecture**.

> **Note on Architecture Variants**: This chapter describes the *principal architecture* used for stream-based targets (Bash, AWK, Go, Rust). Other targets use variant architectures:
> - **Fixed-Point Architecture** (Book 3: C# Target) - Uses iterative fixpoint evaluation for recursive predicates
> - **Query Engine Architecture** (Book 3: C# Target) - Uses IR + runtime library with plan nodes
> - **Generator Architecture** - Uses Python generators for lazy evaluation
>
> The core concepts (classification, constraint analysis, template rendering) apply across all variants, but the execution model differs.

## The Core Concept Revisited

UnifyWeaver treats Prolog as a **declarative specification language** and target languages as **efficient execution targets**. The primary goal is not to replicate all of Prolog's features, but to compile a specific, useful subset of Prolog—data relationships and queries—into optimized code that works well in the target environment.

## The Compilation Pipeline

The transformation from a Prolog predicate to a Bash script follows a clear pipeline:

```
┌──────────────────┐
│ Prolog Predicate │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ Classify Pattern │ (recursive_compiler.pl)
└────────┬─────────┘
         │
         ├────────────────────────┐
         │                        │
         ▼                        ▼
┌──────────────────┐      ┌───────────────────────────┐
│ Non-Recursive    │      │ Recursive                 │
│ (stream_compiler)│      │ (advanced_recursive_compiler) │
└────────┬─────────┘      └───────────┬───────────────┘
         │                            │
         │                            ▼
         │                  ┌───────────────────────────┐
         │                  │   Try Advanced Patterns   │
         │                  │  (tail -> linear ->       |
         |                  |  graph -> mutual)         │
         │                  └───────────┬───────────────┘
         │                              │
         └───────────────┬──────────────┘
                         │
                         ▼
┌──────────────────────────────────┐
│ Multifile Dispatch (if target≠   │
│ bash): route to target-specific  │
│ code generator via multifile     │
│ predicates                       │
└────────────────┬─────────────────┘
                 │
                 ▼
┌──────────────────────────────────┐
│ Analyze Constraints & Options    │ (constraint_analyzer.pl)
└────────────────┬─────────────────┘
                 │
                 ▼
┌──────────────────────────────────┐
│ Select & Render Template         │ (template_system.pl / multifile)
└────────────────┬─────────────────┘
                 │
                 ▼
        ┌────────────────┐
        │  Target Code   │
        │ (Bash, Ruby,   │
        │  Java, C, ...) │
        └────────────────┘
```

1.  **Prolog Predicate:** The process starts with the Prolog predicate you want to compile (e.g., `ancestor/2`).

2.  **Pattern Analysis:** The main `recursive_compiler` first inspects the predicate to classify its pattern (non-recursive, simple recursion, or a candidate for advanced compilation).

3.  **Strategy Selection & Dispatch:**
    *   If the predicate is **not recursive**, it is handed off to the `stream_compiler`.
    *   If the predicate is **recursive**, it is passed to the `advanced_recursive_compiler`.

4.  **Advanced Pattern Matching:** The advanced compiler attempts to match the predicate against its known patterns in order of specificity: tail recursion, then linear recursion, then graph, then mutual recursion (by detecting Strongly Connected Components).

5.  **Multifile Dispatch (Multi-Target):** When `target(T)` is specified and an advanced pattern is detected, the compiler dispatches code generation to the appropriate target via Prolog's **multifile predicates**. Each target registers clauses such as `tail_recursion:compile_tail_pattern/9` and `linear_recursion:compile_linear_pattern/8`. Prolog's first-argument indexing routes to the correct target-specific code generator automatically. This means adding a new target only requires appending multifile clauses — no changes to the core analysis modules.

6.  **Constraint Analysis:** The compiler queries the `constraint_analyzer` to fetch any constraints for the predicate (e.g. `unique(true)`).

7.  **Template Rendering:** For the Bash target, the compiler selects an appropriate code template and uses the `template_system` to generate the final script. For other targets, the multifile dispatch clauses generate idiomatic code directly (e.g., `@tailrec` in Scala, `loop/recur` in Clojure, `tailrec fun` in Kotlin).

8.  **Output:** The final output is executable code in the selected target language — a Bash script, Ruby method, Java class, C function, or any of the 20+ supported targets.

## The Core Modules

The UnifyWeaver compiler is built on a set of core Prolog modules.

### 1. `template_system.pl`
This module is a flexible templating engine used to generate the final Bash code. It allows for file-based templates, caching, and a clean separation between the compiler's logic and the Bash implementation details.

### 2. `stream_compiler.pl`
This module handles simple, **non-recursive predicates**, converting them into efficient streaming Unix pipelines.

### 3. `recursive_compiler.pl`
This is the main entry point and **dispatcher**. It performs the initial analysis and decides which specialized compiler to use.

### 4. `constraint_analyzer.pl`
This module manages and analyzes predicate constraints, such as `unique` and `ordered`, which guide the optimization process.

### 5. `advanced_recursive_compiler.pl`
This is the orchestrator for complex recursion. It uses several sub-modules (`pattern_matchers.pl`, `scc_detection.pl`, `tail_recursion.pl`, etc.) to identify and compile advanced patterns. For the Bash target, it generates optimized shell code directly. For all other targets, it uses **multifile dispatch** — each target file registers clauses like `tail_recursion:compile_tail_pattern/9` and `linear_recursion:compile_linear_pattern/8`, and Prolog's first-argument indexing routes code generation to the correct target automatically. This architecture means the core analysis modules never need to change when new targets are added.

## Structure of the Generated Bash Code

The Bash code generated by UnifyWeaver follows a consistent structure.

### Facts as Associative Arrays

Prolog facts are typically compiled into a `declare -A` statement, which creates an **associative array** (also known as a hash map or dictionary). This allows for O(1), or constant time, lookups.

For a predicate like `parent(alice, bob).`, the generated code would look something like this:

```bash
declare -A parent_data=(
    ["alice:bob"]=1
)

parent() {
    local key="$1:$2"
    # Check if the key exists in the array
    [[ -n "${parent_data[$key]}" ]] && echo "$key"
}
```

### Rules as Functions and Pipelines

Prolog rules are compiled into Bash functions. 

*   **Non-recursive rules** become pipelines of commands that filter, join, and transform data streams.
*   **Recursive rules** (especially transitive closures) become the more complex BFS implementation discussed above, with `while` loops managing the queue and `grep` or similar tools finding the next set of relationships.

By compiling to these optimized structures, UnifyWeaver produces Bash code that is often far more efficient than a naive, direct translation of the Prolog logic would be.

## Try It Yourself: Compiling a Predicate

Let's see the compiler in action. Start SWI-Prolog with UnifyWeaver:

```bash
cd UnifyWeaver
swipl -f init.pl
```

Load the compiler and define some facts:

```prolog
?- use_module(unifyweaver(core/compiler_driver)).
true.

?- assertz(edge(a, b)).
?- assertz(edge(b, c)).
?- assertz(edge(c, d)).
true.
```

Now compile the `edge/2` predicate to Bash:

```prolog
?- compile(edge/2, [target(bash)], Scripts).
=== Compiling edge/2 ===
  Constraints: [unique(true),unordered(true)]
Type: facts (3 clauses)
Scripts = ['education/output/advanced/edge.sh'].
```

The generated file `edge.sh` contains:

```bash
#!/bin/bash
# edge - fact lookup
declare -A edge_data=(
    [a:b]=1
    [b:c]=1
    [c:d]=1
)
edge() {
  local key="$1:$2"
  [[ -n "${edge_data[$key]}" ]] && echo "$key"
}
edge_stream() {
  for key in "${!edge_data[@]}"; do
    echo "$key"
  done
}
```

You can test it:

```bash
source education/output/advanced/edge.sh
edge_stream
# Output: a:b  b:c  c:d
```

For more comprehensive examples including recursive predicates and different targets, see [Book 2: Bash Target](../book-02-bash-target/).

## Incremental Compilation

UnifyWeaver includes an **optional incremental compilation** system that caches compiled code to avoid recompiling unchanged predicates. This significantly speeds up iterative development.

### How It Works

```
┌──────────────────┐
│ Predicate Source │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐     ┌─────────────┐
│ Content Hasher   │────►│ Hash Match? │
│ (term_hash/2)    │     └──────┬──────┘
└──────────────────┘            │
                         Yes    │    No
                    ┌───────────┴───────────┐
                    │                       │
                    ▼                       ▼
            ┌───────────────┐       ┌───────────────┐
            │ Return Cached │       │ Compile Fresh │
            │    Code       │       │ & Store Cache │
            └───────────────┘       └───────────────┘
```

### Key Features

- **Predicate Hashing** - Detects source changes via `term_hash/2` with variable normalization
- **Dependency Tracking** - Automatically invalidates dependent predicates when a dependency changes
- **Multi-Target Support** - Independent caching for all 20 compilation targets
- **Disk Persistence** - Cache survives restarts (saved to `.unifyweaver_cache/`)
- **~11x Speedup** - Measured speedup from cache hits in benchmarks

### Usage

```prolog
% Use incremental compilation
?- use_module(unifyweaver(incremental/incremental_compiler)).

% Compile with caching (default)
?- compile_incremental(edge/2, bash, [], Code).

% Force fresh compilation (bypass cache)
?- compile_incremental(edge/2, bash, [incremental(false)], Code).

% View cache statistics
?- incremental_stats.

% Save cache to disk
?- save_cache.

% Clear all caches
?- clear_all_cache.
```

### Disabling Incremental Compilation

Incremental compilation is **optional** and can be disabled at multiple levels:

| Level | Method |
|-------|--------|
| Per-call | `[incremental(false)]` option |
| Per-session | `set_prolog_flag(unifyweaver_incremental, false)` |
| Environment | `UNIFYWEAVER_CACHE=0` |

## Next Steps

With an understanding of the architecture and having seen compilation in action, you are ready to explore specific targets in depth. Book 2 covers the Bash target with complete examples, while later books cover Python, Go, Rust, and C# targets.

---

## Navigation

**←** [Previous: Chapter 2: Prolog Fundamentals for UnifyWeaver](02_prolog_fundamentals) | [📖 Book 1: Foundations](./) | [Next: Book 2: Bash Target →](../book-02-bash-target/)
