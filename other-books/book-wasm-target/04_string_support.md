# Chapter 4: String Support

WASM's linear memory doesn't have native string types. This chapter explains how UnifyWeaver adds string support for real-world applications like graph visualization.

## The Challenge

WASM only has numeric types (i32, i64, f32, f64). To handle strings:

1. **Allocate memory** in WASM's linear memory
2. **Encode strings** as UTF-8 bytes
3. **Pass pointers** instead of strings
4. **Decode in TypeScript** back to strings

## Memory Model

```
┌─────────────────────────────────────────────────────┐
│                 WASM Linear Memory                  │
├─────────────────────────────────────────────────────┤
│  ptr → [UTF-8 bytes...][null]                       │
│  1024   t o m \0                                    │
└─────────────────────────────────────────────────────┘
```

## String Runtime

UnifyWeaver generates these WASM exports:

```llvm
; Allocate memory (bump allocator)
define i32 @alloc(i32 %size) { ... }

; Deallocate (no-op in bump allocator)
define void @dealloc(i32 %ptr, i32 %size) { ... }

; String length
define i32 @strlen_wasm(i32 %ptr) { ... }
```

## Using from Prolog

```prolog
?- compile_wasm_string_module(
       [func(ancestor, 2, transitive_closure)],
       [module_name(family_graph)],
       Code).
```

This generates LLVM IR with:
- String runtime (alloc, dealloc)
- Edge storage (addEdge, getEdge, getEdgeCount)
- Graph query functions

## TypeScript Integration

The generated TypeScript hides pointer management:

```typescript
class GraphWasm {
  private encodeString(str: string): [number, number] {
    const bytes = this.encoder.encode(str);
    const ptr = this.exports.alloc(bytes.length + 1);
    // Copy bytes to WASM memory
    new Uint8Array(this.memory.buffer, ptr).set(bytes);
    return [ptr, bytes.length];
  }

  addEdge(from: string, to: string): void {
    const [fromPtr, fromLen] = this.encodeString(from);
    const [toPtr, toLen] = this.encodeString(to);
    this.exports.addEdge(fromPtr, fromLen, toPtr, toLen);
  }
}
```

## Key Concepts

| Concept | Description |
|---------|-------------|
| **Linear Memory** | WASM's byte array for all data |
| **Bump Allocator** | Simple allocation, just increment pointer |
| **Pointer+Length** | How strings are passed (ptr, len) |
| **UTF-8** | Standard string encoding |

---

**Next**: [Chapter 5: Graph Visualization](05_graph_visualization.md)
