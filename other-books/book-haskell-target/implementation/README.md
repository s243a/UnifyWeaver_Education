<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book Haskell Target - Implementation Documentation

Technical deep-dive documentation for Haskell code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Recursion Patterns | [02_recursion_impl.md](./02_recursion_impl.md) | [02_recursion_questions.md](./02_recursion_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_module_to_haskell/3` API
- List fold via `foldr`
- Tail recursion with BangPatterns
- Strict accumulator pattern
- Factorial with pattern matching
- Fibonacci (tree recursion)
- `compile_predicate_to_haskell/3` alternative
- Type signatures

## Source Files

- `src/unifyweaver/targets/haskell_target.pl`
