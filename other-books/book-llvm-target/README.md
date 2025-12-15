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

## Quick Example

```prolog
?- use_module('src/unifyweaver/targets/llvm_target').
?- compile_tail_recursion_llvm(sum/2, [], Code).
```
