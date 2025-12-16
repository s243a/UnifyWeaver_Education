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

## Why WebAssembly?

| Platform | Use Case |
|----------|----------|
| Browser | Client-side computation |
| Node.js | Server-side, CLI tools |
| Edge | Cloudflare Workers, Deno |
| Portable | Single .wasm runs everywhere |
