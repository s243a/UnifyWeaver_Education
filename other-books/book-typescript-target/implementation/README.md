<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book TypeScript Target - Implementation Documentation

Technical deep-dive documentation for TypeScript code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Recursion Patterns | [02_recursion_impl.md](./02_recursion_impl.md) | [02_recursion_questions.md](./02_recursion_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_recursion/3` API
- Tail recursion with accumulators
- Loop version for guaranteed O(1) stack
- List fold via `Array.reduce()`
- Memoization with `Map`
- Factorial compilation
- `compile_module/3` for multi-predicate modules
- Full TypeScript type annotations

## Source Files

- `src/unifyweaver/targets/typescript_target.pl`
