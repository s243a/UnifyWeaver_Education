<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: UnifyWeaver Architecture - Implementation Details

This document provides function-level documentation for the UnifyWeaver compilation pipeline.

**Source**: `src/unifyweaver/core/`

---

## Compilation Pipeline Overview

```
Prolog Predicate
      │
      ▼
┌─────────────────┐
│ Pattern Analysis│ (recursive_compiler.pl)
└────────┬────────┘
         │
    ┌────┴────┐
    ▼         ▼
Non-Recursive  Recursive
(stream_compiler) (advanced_recursive_compiler)
    │         │
    └────┬────┘
         ▼
┌─────────────────┐
│Constraint Analysis│ (constraint_analyzer.pl)
└────────┬────────┘
         ▼
┌─────────────────┐
│Template Rendering│ (template_system.pl)
└────────┬────────┘
         ▼
    Bash Script
```

---

## compile/3

Main entry point for compilation.

### Signature

```prolog
compile(+Predicate/Arity, +Options, -Scripts)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | The predicate to compile |
| `Options` | `list` | Compilation options |
| `Scripts` | `list` | Generated script paths |

### Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `target(T)` | `bash`, `python`, `go`, etc. | `bash` | Target language |
| `output_dir(D)` | path | `education/output/advanced` | Output directory |
| `incremental(B)` | `true`, `false` | `true` | Use caching |

### Example

```prolog
?- compile(edge/2, [target(bash)], Scripts).
=== Compiling edge/2 ===
  Constraints: [unique(true),unordered(true)]
Type: facts (3 clauses)
Scripts = ['education/output/advanced/edge.sh'].
```

---

## Core Modules

### template_system.pl

Flexible templating engine for code generation.

#### render_template/3

```prolog
render_template(+TemplateName, +Bindings, -Output)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| `TemplateName` | `atom` | Template file name |
| `Bindings` | `list` | Variable bindings `[Var=Value, ...]` |
| `Output` | `string` | Rendered template |

**Template Syntax**:
```
#!/bin/bash
# {{predicate_name}} - {{description}}
declare -A {{predicate_name}}_data=(
{{#facts}}
    [{{key}}]=1
{{/facts}}
)
```

### stream_compiler.pl

Handles non-recursive predicates as Unix pipelines.

#### compile_stream/3

```prolog
stream_compiler:compile_stream(+Predicate/Arity, +Options, -Code)
```

Generates streaming pipeline code for simple predicates.

### recursive_compiler.pl

Main dispatcher that classifies predicates and routes to appropriate compiler.

#### classify_predicate/2

```prolog
classify_predicate(+Predicate/Arity, -Type)
```

Returns:
- `facts` - Pure facts, no rules
- `non_recursive` - Rules without self-reference
- `recursive` - Contains recursive calls

### constraint_analyzer.pl

Manages predicate constraints for optimization.

#### get_constraints/2

```prolog
get_constraints(+Predicate/Arity, -Constraints)
```

Returns list of constraints: `[unique(true), unordered(true), ...]`

### advanced_recursive_compiler.pl

Orchestrates complex recursion compilation.

#### Pattern Matching Order

1. **Tail recursion** → Iterative loop
2. **Linear recursion** → Single recursion with accumulator
3. **Graph recursion** → BFS/DFS traversal
4. **Mutual recursion** → SCC detection and joint compilation

---

## Generated Code Structure

### Facts as Associative Arrays

```prolog
parent(alice, bob).
parent(alice, carol).
```

Generates:

```bash
declare -A parent_data=(
    ["alice:bob"]=1
    ["alice:carol"]=1
)

parent() {
    local key="$1:$2"
    [[ -n "${parent_data[$key]}" ]] && echo "$key"
}

parent_stream() {
    for key in "${!parent_data[@]}"; do
        echo "$key"
    done
}
```

### Rules as Functions

```prolog
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
```

Generates pipeline-based function:

```bash
grandparent() {
    parent_stream | while IFS=: read -r gp p; do
        parent "$p" | while IFS=: read -r _ gc; do
            echo "$gp:$gc"
        done
    done | sort -u
}
```

---

## Incremental Compilation

### Architecture

```
Predicate Source
      │
      ▼
┌─────────────┐     ┌─────────────┐
│Content Hasher│────►│Hash Match?  │
│(term_hash/2) │     └──────┬──────┘
└─────────────┘            │
                     Yes   │   No
                  ┌────────┴────────┐
                  ▼                 ▼
          ┌─────────────┐   ┌─────────────┐
          │Return Cached│   │Compile Fresh│
          │    Code     │   │& Store Cache│
          └─────────────┘   └─────────────┘
```

### compile_incremental/4

```prolog
compile_incremental(+Predicate/Arity, +Target, +Options, -Code)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Target` | `atom` | Target language |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated code |

### Cache Operations

```prolog
% Force fresh compilation
?- compile_incremental(edge/2, bash, [incremental(false)], Code).

% View statistics
?- incremental_stats.

% Save to disk
?- save_cache.

% Clear all caches
?- clear_all_cache.
```

### Disabling Incremental Compilation

| Level | Method |
|-------|--------|
| Per-call | `[incremental(false)]` option |
| Per-session | `set_prolog_flag(unifyweaver_incremental, false)` |
| Environment | `UNIFYWEAVER_CACHE=0` |

### Performance

- **~11x speedup** from cache hits in benchmarks
- Cache stored in `.unifyweaver_cache/`
- Survives restarts
- Independent caching per target

---

## Architecture Variants

The principal architecture (described above) applies to stream-based targets. Other variants:

| Variant | Targets | Execution Model |
|---------|---------|-----------------|
| **Principal (Stream)** | Bash, AWK, Go, Rust | Unix pipelines |
| **Fixed-Point** | C# Query | Iterative fixpoint |
| **Generator** | Python | Lazy evaluation |
| **Query Engine** | C# IR | Plan nodes + runtime |

Core concepts (classification, constraints, templates) apply across all variants.

---

## Constraint System

### Available Constraints

| Constraint | Values | Effect |
|------------|--------|--------|
| `unique(B)` | `true`, `false` | Enable deduplication |
| `unordered(B)` | `true`, `false` | Allow hash-based dedup |
| `ordered(B)` | `true`, `false` | Preserve input order |

### Setting Constraints

```prolog
% In your Prolog file
:- constraint(my_pred/2, unique(true)).
:- constraint(my_pred/2, ordered(true)).
```

### Constraint Inference

The analyzer infers constraints from predicate structure:
- Pure facts → `unique(true), unordered(true)`
- Recursive rules → `unique(true)` (visited tracking)

---

## Related Documentation

- [Book 2: Bash Target](../../book-02-bash-target/)
- [Template System Source](../../../../src/unifyweaver/core/template_system.pl)
- [Recursive Compiler Source](../../../../src/unifyweaver/core/recursive_compiler.pl)
