<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book LLVM Target - Implementation Documentation

Technical deep-dive documentation for LLVM integration with C, Go, and Rust.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Integration | [02_integration_impl.md](./02_integration_impl.md) | [02_integration_questions.md](./02_integration_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_tail_recursion_llvm/3` with export option
- dllexport wrapper functions
- Shared library building (llc, clang)
- C integration with header files
- Go integration via cgo
- Rust integration via FFI
- Prolog %% escaping fix
- Platform-specific library paths

## Source Files

- `src/unifyweaver/targets/llvm_target.pl`
