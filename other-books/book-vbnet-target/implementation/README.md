<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book VB.NET Target - Implementation Documentation

Technical deep-dive documentation for VB.NET code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 3: Recursive Queries | [03_recursive_queries_impl.md](./03_recursive_queries_impl.md) | [03_recursive_queries_questions.md](./03_recursive_queries_questions.md) |

## Question Count

- Chapter 3: 10 questions

## Topics Covered

- `compile_tail_recursion_vbnet/3` API
- `compile_linear_recursion_vbnet/3` API
- `compile_mutual_recursion_vbnet/3` API
- Do While loop for tail recursion
- Dictionary(Of T, T) memoization
- Shared dictionary for mutual recursion
- VB.NET vs F# comparison
- Type declarations

## Source Files

- `src/unifyweaver/targets/vbnet_target.pl`
