# LLVM Target Education Book

Compile Prolog predicates to native code via LLVM IR.

## Chapters

1. [Introduction](01_introduction.md) - Getting started with LLVM target
2. [Integration](02_integration.md) - C, Go, and Rust FFI
3. [Recursive Queries](03_recursive_queries.md) - All recursion patterns

## Prerequisites

- SWI-Prolog 8.0+
- LLVM toolchain (`llc`, `clang`)

```bash
sudo apt install llvm clang  # Ubuntu/Debian
```

## Why LLVM?

| Problem | LLVM Solution |
|---------|---------------|
| Tail call not guaranteed | `musttail` instruction |
| Single architecture | Any LLVM target (x86, ARM, RISC-V) |
| No C interop | Native C ABI with `dllexport` |

## Generating LLVM Hybrid WAM Code

Use the WAM LLVM target for lowered LLVM IR or WASM-oriented output:

```prolog
?- use_module('src/unifyweaver/targets/wam_llvm_target').
?- write_wam_llvm_project([ancestor/2],
       [emit_mode(functions), foreign_lowering(true)],
       'out/ancestor.ll').
```

For browser or sandboxed deployment, use `write_wam_llvm_wasm_project/3` when
the workflow needs the LLVM-to-WASM variant. `foreign_lowering(true)` lets the
target recognize supported kernel shapes before falling back to ordinary WAM
control flow.

## Hybrid WAM Role

LLVM is the lowered-code perspective on WAM items, control flow, value
representation, and FFI boundaries. Book 17 covers the shared semantic model;
this book shows how that model can become lower-level IR.

## Quick Example

```prolog
?- use_module('src/unifyweaver/targets/llvm_target').
?- compile_tail_recursion_llvm(sum/2, [], Code).
```
