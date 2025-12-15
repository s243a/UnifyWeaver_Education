# Chapter 1: Introduction to WebAssembly

WebAssembly (WASM) is a binary instruction format for stack-based virtual machines.

## Why WASM for Prolog?

| Benefit | Description |
|---------|-------------|
| **Portability** | Same `.wasm` runs in browser, Node.js, edge workers |
| **Performance** | Near-native speed |
| **Size** | Compact binaries (our test is 376 bytes!) |
| **Safety** | Sandboxed execution |

## Architecture

```
┌─────────────────────────────────────┐
│        UnifyWeaver (Prolog)         │
│    compile_wasm_module/3            │
└─────────────────┬───────────────────┘
                  │
                  ▼
         prolog_wasm.ll (LLVM IR)
         target: wasm32-unknown-unknown
                  │
        ┌─────────┴─────────┐
        ▼                   ▼
   llc -march=wasm32    wasm-ld
        │                   │
        └─────────┬─────────┘
                  ▼
         prolog_wasm.wasm (376 bytes)
                  │
        ┌─────────┴─────────┐
        ▼                   ▼
     Node.js             Browser
     require()           fetch()
```

## Key Differences from Native

| Feature | Native (.so) | WASM (.wasm) |
|---------|--------------|--------------|
| Target | x86_64 | wasm32 |
| Types | i64 | i32 (mostly) |
| Tail calls | musttail | tail |
| Globals | Supported | Limited |

## Installation

```bash
# LLVM with WASM support
sudo apt install llvm clang

# WASM linker (provides wasm-ld)
sudo apt install lld
```

## Next Steps

- [Chapter 2: Compilation](02_compilation.md)
