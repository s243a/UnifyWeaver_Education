# Chapter 6: Cross-Target Integration

The WASM graph demo proves UnifyWeaver's component system works across multiple targets.

## The Component System

UnifyWeaver's architecture enables cross-target compilation:

```
┌────────────────────────────────────────────────────────────────────┐
│                      Target Registry                               │
│  target_module(wasm, llvm_target)                                  │
│  target_module(typescript, typescript_target)                      │
│  target_module(haskell, haskell_target)                            │
└────────────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┼───────────────────┐
          ▼                   ▼                   ▼
   ┌──────────────┐   ┌──────────────┐   ┌──────────────┐
   │ LLVM Target  │   │  TypeScript  │   │   Haskell    │
   │ → WASM       │   │  Target      │   │   Target     │
   └──────────────┘   └──────────────┘   └──────────────┘
```

## Standard Interface

Each target implements:

```prolog
% Metadata
target_info(Info) :-
    Info = target_info{
        name: wasm,
        family: llvm,
        features: [numeric, strings, edges]
    }.

% Unified dispatch
compile_predicate(Pred/Arity, Options, Code) :- ...
```

## Multi-Target Pipeline

The graph demo uses multiple targets:

| Stage | Target | Output |
|-------|--------|--------|
| 1 | LLVM | LLVM IR with string support |
| 2 | WASM | Binary .wasm module |
| 3 | TypeScript | GraphWasm bindings |
| 4 | Browser | Cytoscape visualization |

## Unified Dispatch

```prolog
% Compile to any target via registry
compile_to_target(wasm, ancestor/2, [string_support(true)], Code).
compile_to_target(typescript, bindings, [module(GraphWasm)], TSCode).
```

## Cross-Target Flow

```prolog
% 1. Define facts
parent(tom, bob).
parent(bob, alice).

% 2. Compile transitive closure to WASM
compile_wasm_string_module(
    [func(ancestor, 2, transitive_closure)],
    [module_name(family)],
    WASMCode).

% 3. Generate TypeScript bindings
generate_ts_string_bindings(
    [func(ancestor, 2, transitive_closure)],
    TSCode).

% 4. Visualize in browser with Cytoscape.js
```

## Glue Layer

The `js_glue.pl` module connects JavaScript runtimes:

```prolog
% Select best runtime for features
js_runtime_choice([fetch, websocket], node).  % Node.js
js_runtime_choice([dom, canvas], browser).     % Browser
```

## Benefits

| Benefit | Description |
|---------|-------------|
| **Reusable Logic** | Write once in Prolog |
| **Target Flexibility** | Compile to any supported target |
| **Interoperability** | Mix targets (WASM backend, TS frontend) |
| **Unified API** | Same interface across targets |

---

**See Also**:
- [target_registry.pl](../../../src/unifyweaver/core/target_registry.pl)
- [Book 7: Cross-Target Glue](../../book-07-cross-target-glue/)
