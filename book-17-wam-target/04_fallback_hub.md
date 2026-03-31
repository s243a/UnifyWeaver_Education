# Chapter 4: WAM as a Fallback Hub

The WAM target occupies a unique position in the UnifyWeaver ecosystem. While most targets aim for direct, idiomatic code generation, WAM provides a "universal fallback."

## The Hub Pattern

When a Prolog predicate is too complex for native lowering (e.g., it heavily uses backtracking or complex unification), UnifyWeaver can route it through the WAM target.

```
Prolog Source
      │
      ▼
[ WAM Target ] ──────┐
      │              │
      ▼              ▼
[ WAT Target ]  [ Jamaica Target ]
      │              │
      ▼              ▼
 WebAssembly        JVM Bytecode
```

## Compilation Pipeline

1.  **Prolog to WAM**: Symbolic instructions are generated that preserve logic semantics.
2.  **WAM to Target Bytecode**: The symbolic instructions are mapped to the instruction set of the specific virtual machine (e.g., WASM stack operations).
3.  **Execution**: The resulting code is run in its native environment with full Prolog capability.

## Target Backends

- **WAT (WebAssembly Text)**: A text representation of the WebAssembly binary format, suitable for execution in browsers and Node.js.
- **Jamaica**: A JVM bytecode generator tool (part of the larger JVM family of targets in UnifyWeaver).
- **Krakatau**: A robust assembler and disassembler for Java bytecode, used alongside Jamaica for high-fidelity JVM targets.

## Benefits

-   **Uniformity**: We only need to solve "Backtracking in WASM" once by mapping WAM choice points to WASM.
-   **Correctness**: WAM is a proven model for Prolog, ensuring that the generated code behaves exactly like the source.
-   **Portability**: Symbolic WAM is easy to port to any new platform that supports basic VM operations.
