<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book WASM Target - Implementation Documentation

Technical deep-dive documentation for WebAssembly compilation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Compilation | [02_compilation_impl.md](./02_compilation_impl.md) | [02_compilation_questions.md](./02_compilation_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_wasm_module/3` API
- LLVM IR generation
- Target triple (wasm32-unknown-unknown)
- Tail recursion with tail call hint
- Factorial compilation
- Build process (llc, wasm-ld)
- Prolog %% escaping fix

## Source Files

- `src/unifyweaver/targets/llvm_target.pl`
