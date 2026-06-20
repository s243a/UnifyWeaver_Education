# WebAssembly Target for UnifyWeaver

Compile Prolog predicates to WebAssembly for browser and Node.js execution.

## Chapters

1. **[Introduction](01_introduction.md)** - What is WebAssembly and why use it
2. **[Compilation](02_compilation.md)** - How to compile Prolog to WASM
3. **[JavaScript Integration](03_javascript.md)** - Using from Node.js and browser
4. **[String Support](04_string_support.md)** - Handling strings in WASM
5. **[Graph Visualization](05_graph_visualization.md)** - Cytoscape.js demo
6. **[Cross-Target Integration](06_cross_target.md)** - Component system

## Prerequisites

- SWI-Prolog
- LLVM toolchain (`llc`)
- WASM linker (`lld` package)

```bash
sudo apt install llvm clang lld
```

## Quick Example

```prolog
?- compile_wasm_module(
       [func(sum, 2, tail_recursion)],
       [module_name(prolog_wasm)],
       Code).
```

```javascript
const { instance } = await WebAssembly.instantiate(bytes);
console.log(instance.exports.sum(10)); // 55
```

## Generating WASM/WAT Hybrid WAM Code

Use the WAM WAT target when the desired output is WebAssembly text:

```prolog
?- use_module('src/unifyweaver/targets/wam_wat_target').
?- write_wam_wat_project([ancestor/2],
       [],
       'out/ancestor.wat').
```

This path emits a portable WAM-shaped runtime for WASM. It is separate from
using symbolic WAM text as a debug listing; the WAT target is an executable
backend.

## Hybrid WAM Role

WASM is a portable execution target for WAM-shaped programs, especially when
browser or sandboxed deployment matters. Book 17 explains the shared Hybrid
WAM concepts and why WASM is one target option rather than the default meaning
of WAM.

## Why WebAssembly?

| Platform | Use Case |
|----------|----------|
| Browser | Client-side computation |
| Node.js | Server-side, CLI tools |
| Edge | Cloudflare Workers, Deno |
| Portable | Single .wasm runs everywhere |
