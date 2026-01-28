<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book F# Target - Implementation Documentation

Technical deep-dive documentation for F# code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 3: Recursive Queries | [03_recursive_queries_impl.md](./03_recursive_queries_impl.md) | [03_recursive_queries_questions.md](./03_recursive_queries_questions.md) |

## Question Count

- Chapter 3: 10 questions

## Topics Covered

- `compile_tail_recursion_fsharp/3` API
- `compile_linear_recursion_fsharp/3` API
- `compile_mutual_recursion_fsharp/3` API
- Inner loop pattern for tail recursion
- TryGetValue pattern matching
- `and` keyword for mutual recursion
- Guards with `when` clause
- Tail call optimization

## Source Files

- `src/unifyweaver/targets/fsharp_target.pl`
