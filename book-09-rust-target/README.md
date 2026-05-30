<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 9: Rust Target

**Memory-Safe High-Performance Compilation**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers how to use UnifyWeaver to compile Prolog predicates into safe, high-performance Rust programs. The Rust target combines the safety of Rust with the declarative power of Prolog.

## Prerequisites

**Required:**
- [Book 1: Foundations](../book-01-foundations/README.md)

**Recommended:**
- [Book 6: Go Target](../book-06-go-target/README.md) - similar native compilation concepts

**Technical:**
- Rust and Cargo installed (`rustc`, `cargo`)
- UnifyWeaver with Rust target support

## Learning Path

**1. Introduction** (`01_introduction.md`)
-   Why use the Rust target?
-   Safety and performance benefits
-   Comparison with Go and C# targets

**2. Basic Compilation** (`02_basic_compilation.md`)
-   Compiling facts and rules
-   Single-file compilation (`rustc`)
-   Stream processing

**3. Project Generation** (`03_project_generation.md`)
-   Generating full Cargo projects
-   Dependency management
-   Building release binaries

**4. Advanced Features** (`04_advanced_features.md`)
-   Regex matching
-   JSON I/O with Serde
-   Constraints and aggregations

**5. Native Clause Body Lowering** (`05_native_clause_lowering.md`)
-   Multi-clause predicates → if/else if/else chains
-   Expression-based returns (no explicit `return`)
-   If-then-else → nested if/else
-   Verified: compiles and runs with `rustc`

## Generating Rust Hybrid WAM Code

Use the WAM Rust target for Hybrid WAM projects:

```prolog
?- use_module('src/unifyweaver/targets/wam_rust_target').
?- write_wam_rust_project([ancestor/2],
       [emit_mode(interpreter), lmdb_mode(none)],
       'out/wam_rust_ancestor').
```

The project writer compiles each predicate through the WAM pipeline, detects
kernel opportunities, and emits a full Cargo project (`Cargo.toml`, `src/lib.rs`,
the WAM runtime modules, plus any lowered helpers the target can safely
generate). For a single predicate fragment, use `compile_wam_predicate_to_rust/4`
after obtaining WAM items or text from `wam_target`.

Key options (defaults in parentheses):

| Option | Values | Effect |
|---|---|---|
| `emit_mode(Mode)` | `interpreter` (default), `functions` | Keep the WAM interpreter loop, or lower deterministic predicates to plain Rust functions. |
| `lmdb_mode(Mode)` | `none` (default), `cursor` | Back fact predicates with an LMDB cursor source instead of inlined facts. |
| `lmdb_crate(Crate)` | `auto` (default → `lmdb_zero`), `lmdb_zero`, `heed` | Which Rust LMDB binding to emit when `lmdb_mode(cursor)`. |
| `parallel(Bool)` | `false` (default), `true` | Enable Rayon parallel query execution. |
| `wam_fallback(Bool)` | `true` (default), `false` | Allow falling back to the WAM path when native lowering does not apply. |

## Hybrid WAM Role

Rust is the main example for memory-safe WAM state, external fact sources,
cost-aware materialization, and FFI/kernel dispatch. Book 17 covers the shared
Hybrid WAM concepts; this book shows how those concepts become concrete in a
systems language with ownership and explicit storage choices.

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
